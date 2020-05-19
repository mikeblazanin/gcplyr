#TODO:
# run curves varying initial density & moi
# given control curves and curves varying in density & moi,
#   figure out how one could calculate phage parameters
#   or phage fitness
# can dede itself handle a stop-at-equilibrium condition?
#   Yes, they're called roots. However, it's not clear whether
#     it would really make things faster or not
# add in the evolution of resistant bacteria
# fix area-under-curve calculations
# use deriv-percap of B to find max decay rate?
# think about characteristics to be taken when curves are too slow
#   (e.g. time to grow above some threshold density)
# test whether it's better to average replicate wells first, then analyze
#   or analyze each independently then average stats together
# When stats are plotted against each other there are lots of logistic curves
#  fit a logistic eq to them and see what they are
#  (e.g. are they just the bacterial curve when no phage around?)
# Go back and re-learn multivariate calc
# Look at b-tau tradeoff and see if we re-create optimal values
# In theory we should be able to predict how much pfu_final
#  is above b*max_dens based on auc before max_dens (or similar)
# Should calculate bacterial decay rate from max_dens to extin_time
#  try as negative exponential growth from max_dens
#  e.g. B(t) = max_dens - e^(r(t-max_time))
# Perhaps the whole B density curve can be reduced to two logistic-like curves?
#  One representing bacterial growth, and one representing bacterial decay
#  We're kind of idealizing bacterial growth as logistic and phage
#   growth as logistic and B(t) is just the integrated difference between them
# Parallelize for running on cluster with much faster walltime
# Perhaps a B-tau tradeoff will allow us to think only about variation
#   along two axes: alpha and tau (since B ~ c (intercept) + d*tau (slope))
# Plot B(t) for dift a, b, tau values. Use regression to predict which sets
#   of a, b, tau values should give same max time & extin time then plot those
#   values
# Measure amount of time I is above S (and so I is more of B) – should be correlated w/ tau

## Import libraries ----

library(deSolve)
#library(reshape2)
library(data.table)
library(ggplot2)
library(dplyr)

#Okabe and Ito 2008 colorblind-safe qualitative color scale
my_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7", "#000000")
scales::show_col(my_cols)

## Define derivatives function ----
derivs <- function(t, y, parms) {
  #The derivs function must return the derivative of all the variables at a
  # given time, in a list
  
  #Issue warning about too small/negative yvals (if warnings is 1)
  if (parms["warnings"]==1 & any(y < parms["thresh_min_dens"])) {
    warning(paste("pop(s)",
                  paste(which(y < parms["thresh_min_dens"]), collapse = ","),
                  "below thresh_min_dens, treating as 0"))
  }

  #Set small/negative y values to 0 so they don't affect the dN's
  y[y < parms["thresh_min_dens"]] <- 0
  
  #Create output vector
  dY <- c(S = 0, I = 0, P = 0)
  
  ##Calculate dS
  
  #V1 (exponential dS/dt)
  #dS/dt = rS - aSP
  #dS <- parms["r"] * y["S"] - a * y["S"] * y["P"]
  
  #V2 (logistic dS/dt)
  #dS/dt = rS((K-S)/K) - aSP
  # dY["S"] <- parms["r"] * y["S"] * ((parms["K"] - y["S"])/parms["K"]) - 
  #   parms["a"] * y["S"] * y["P"]
  
  #V3 (logistic dS/dt) (including competition from I pop)
  #dS/dt = rS((K-S-c*I)/K) - aSP
  dY["S"] <- parms["r"] * y["S"] * 
      ((parms["K"] - y["S"] - parms["c"] * y["I"])/parms["K"]) - 
    parms["a"] * y["S"] * y["P"]
  
  ##Calculate dI
  #dI/dt = aSP - aS(t-tau)P(t-tau)
  if (t < parms["tau"]) {
    dY["I"] <- parms["a"] * y["S"] * y["P"]
  } else {
    dY["I"] <- parms["a"] * y["S"]*y["P"] - 
      parms["a"] * lagvalue(t - parms["tau"], 1)*lagvalue(t - parms["tau"], 3)
  }
  
  ##Calculate dP
  #dP/dt = baS(t-tau)P(t-tau) - aSP
  if (t < parms["tau"]) {
    dY["P"] <- -parms["a"] * y["S"] * y["P"]
  } else {
    dY["P"] <- parms["b"] * parms["a"] * 
      lagvalue(t-parms["tau"], 1)*lagvalue(t-parms["tau"], 3) - 
      parms["a"]*y["S"]*y["P"]
  }
  
  #Issue warning about too large pop (if warnings is TRUE)
  if (parms["warnings"]==1 & any(y > 10**100)) {
    warning(paste("pop(s)",
                  paste(which(y > 10**100), collapse = ","),
                  "exceed max limit, 10^100, returning dY = 0"))
  }
  dY[y > 10**100] <- 0
  
  #From documentation: The return value of func should be a list, whose first 
  #element is a vector containing the derivatives of y with respect to time
  return(list(dY))
}

## Define function for running simulations across many parameter values ----
run_sims <- function(rvals,
                     kvals,
                     avals,
                     tauvals,
                     bvals,
                     cvals = 1,
                     init_bact_dens_vals = 10**6,
                     init_moi_vals = 10**-2,
                     min_dens = 0.1,
                     init_time = 100,
                     init_stepsize = 1,
                     print_info = TRUE) {
  
  #Inputs: vectors of parameters to be combined factorially to make
  #         all possible combinations & run simulations with
  #       min_dens is threshold density to consider a population at equilibrium
  #       init_time is the length of simulation that will first be tried
  #         (and subsequently lengthened as necessary to find equilibrium)
  #       init_stepsize is the length of stepsize that will first be tried
  #         (step sizes scale as simulation legnth increases so that the 
  #         number of timepoints returned for each simulation is constant)
  #       print_info will print about the simulations (e.g. how many will be run)
  #
  #Output: a list with three entries
  #   1. a dataframe (AKA ybig) with all parameters and densities of each pop 
  #       at each timepoint (melted), along with a boolean for whether it'd 
  #       reached equilibrium
  #   2. a dataframe (AKA y_noequil) with just the parameters for runs where
  #       at least one pop didn't reach equilibrium
  #   3. a dataframe (AKA yfail) with the parameters for runs that failed
  #       to successfully complete
  
  if(init_time %% init_stepsize != 0) {
    warning("init_time is not divisible by init_stepsize, this has not been tested")
  }
  
  #Print number of simulations that will be run
  if(print_info) {
    print(paste(length(rvals)*length(kvals)*length(avals)*
                  length(tauvals)*length(bvals)*length(cvals)*
                  length(init_bact_dens_vals)*length(init_moi_vals),
                "simulations will be run"))
    
    #Save sequence of 10% cutoff points for later reference
    progress_seq <- round(seq(from = 0, 
                              to = (length(rvals)*length(kvals)*length(avals)*
                                      length(tauvals)*length(bvals)*length(cvals)*
                                      length(init_bact_dens_vals)*
                                      length(init_moi_vals)),
                              by = (length(rvals)*length(kvals)*length(avals)*
                                      length(tauvals)*length(bvals)*length(cvals)*
                                      length(init_bact_dens_vals)*
                                      length(init_moi_vals))/10))
  }
  
  #Make placeholders
  yfail <- NULL #for runs that fail
  i <- 1 #the uniq_run counter
  #for runs that succeed, pre-allocate ybig now to save on memory/speed
  ybig <- data.frame("uniq_run" = rep(NA, 5*(1+init_time/init_stepsize)*
                                         length(rvals)*length(kvals)*
                                         length(avals)*length(tauvals)*
                                         length(bvals)*length(cvals)*
                                         length(init_bact_dens_vals)*
                                         length(init_moi_vals)),
                     "r" = NA, "a" = NA, "b" = NA, "tau" = NA, "K" = NA,
                     "c" = NA, "init_bact_dens" = NA, "init_moi" = NA,
                     "equil" = NA, "time" = NA, "Pop" = as.character(NA), 
                     "Density" = NA, stringsAsFactors = F)
                                        
  #Cycle through all parameter combinations
  for (myr in rvals) {
    for (myk in kvals) {
      for (mya in avals) {
        for (mytau in tauvals) {
          for (myb in bvals) {
            for (myc in cvals) {
              for (my_init_bact in init_bact_dens_vals) {
                for (my_moi in init_moi_vals) {
                  #Define pops & parameters
                  yinit <- c(S = my_init_bact,
                             I = 0,
                             P = my_init_bact*my_moi)
                  params <- c(r = myr, a = mya, b = myb, tau = mytau,
                              K = myk, c = myc,
                              warnings = 0, thresh_min_dens = 10**-100)
                  
                  #Run simulation(s) with longer & longer times until equil reached
                  #Also, if equil has non-zero I run with shorter steps
                  keep_running <- TRUE #placeholder for triggering end of sims
                  #Placeholder for the number of times I has been detected above
                  # min_dens while S has not been
                  i_only_pos_times <- 0
                  j <- 0 #length counter (larger is longer times)
                  k <- 0 #step size counter (larger is smaller steps)
                  while(keep_running) {
                    #Define times, with lengths & steps doubling for ea j count
                    # (so that the number of timepoints returned is constant)
                    times <- seq(0, init_time*2**j, init_stepsize*2**j)
                    
                    #Run simulation (using 1st entry of list as error code:\
                    # 0 - success
                    # 1 - error
                    # 2 - warning
                    yout_list <- tryCatch(
                      expr = {
                        #Note that the max step size for the integrator is the 
                        # same as our step size except halved for each k count
                        list(0,
                             as.data.frame(
                              dede(y = yinit, times = times, func = derivs, 
                                  parms = params, hmax = init_stepsize*2**(j-k))))
                      },
                      error = function(e) {list(1)},
                      warning = function(w) {
                        list(2,
                             as.data.frame(
                               dede(y = yinit, times = times, func = derivs, 
                                    parms = params, hmax = 2**(j-k))))
                      }
                    )
                    
                    #Infinite loop prevention check (j = 10 is 24 hrs)
                    if (j >= 10 | k >= 15 | j+k >= 20) {
                      keep_running <- FALSE
                      at_equil <- FALSE
                    }
                    
                    #If there was an error, increase k by 1 and re-run
                    if(yout_list[[1]] == 1) {
                      k <- k+1
                    #If there was a warning, could be several causes, so we
                    # generally just halve step size and increase length
                    } else if (yout_list[[1]] == 2) {
                      j <- j+1
                      k <- k+2
                    #If it was successful, check for equilibrium
                    } else if (yout_list[[1]] == 0) {
                      #First drop all rows with nan
                      yout_list[[2]] <- yout_list[[2]][!(is.nan(yout_list[[2]]$S) |
                                                           is.nan(yout_list[[2]]$I) |
                                                           is.nan(yout_list[[2]]$P)), ]
                      
                      #S and I both at equil, we're done
                      if (yout_list[[2]]$S[nrow(yout_list[[2]])] < min_dens & 
                          yout_list[[2]]$I[nrow(yout_list[[2]])] < min_dens) {
                        keep_running <- FALSE
                        at_equil <- TRUE
                      #S not at equil, need more time
                      } else if (yout_list[[2]]$S[nrow(yout_list[[2]])] >= min_dens) { 
                        j <- j+1
                      #I not at equil (but S is because above check failed),
                      #   first we'll lengthen the simulation
                      #    (to make sure it was long enough to catch the last burst)
                      #   then we'll start shrinking our step size
                      } else if (yout_list[[2]]$I[nrow(yout_list[[2]])] >= min_dens) {
                        if (i_only_pos_times < 1) {
                          j <- j+1
                          i_only_pos_times <- i_only_pos_times+1
                        } else {
                          k <- k+1
                        }
                      }
                      
                      ###Old version of equilibrium checking
                      # #then check for equil
                      # if (all(abs(yout[nrow(yout), 2:4] - 
                      #             yout[nrow(yout)-1, 2:4]) < .001)) {
                      #   #If at equil but S or I are non-zero, halve step size
                      #   if(any(yout[nrow(yout), c("S", "I")] > 0.1)) {
                      #     k <- k+1
                      #   #If at equil and S & I are zero, stop
                      #   } else {
                      #     keep_running <- FALSE
                      #     at_equil <- TRUE
                      #   }
                      # } else {
                      #   j <- j+1
                      # }
                    }
                  }
                  
                  #Once end conditions triggered, if run succeeded
                  if(yout_list[[1]] == 0 | yout_list[[1]] == 2) {
                    #Calculate all bacteria (B)
                    yout_list[[2]]$B <- yout_list[[2]]$S + yout_list[[2]]$I
                    #Calculate all phage (PI)
                    yout_list[[2]]$PI <- yout_list[[2]]$P + yout_list[[2]]$I
                    
                    #Reshape, add parameters, and fill into ybig in right rows
                    ybig[((i-1)*5*(1+init_time/init_stepsize)+1):
                           ((i)*5*(1+init_time/init_stepsize)), ] <- 
                      cbind(data.frame(uniq_run = i, r = myr, a = mya, 
                                              b = myb, tau = mytau, K = myk, 
                                              c = myc, init_bact_dens = my_init_bact, 
                                              init_moi = my_moi, equil = at_equil),
                                   data.table::melt(data = data.table::as.data.table(yout_list[[2]]), 
                                                  id.vars = c("time"),
                                                  value.name = "Density", 
                                                  variable.name = "Pop",
                                                  variable.factor = FALSE))
                    
                  #If the run failed
                  } else {
                    if (is.null(yfail)) { #This is the first failed run
                      yfail <- data.frame(uniq_run = i, r = myr, a = mya, 
                                           b = myb, tau = mytau, K = myk,
                                          c = myc, init_bact_dens = my_init_bact, 
                                           init_moi = my_moi, equil = at_equil)
                    } else { #This is a non-first failed run
                      yfail <- rbind(yfail, 
                                     data.frame(uniq_run = i, r = myr, a = mya, 
                                                b = myb, tau = mytau, K = myk, 
                                                c = myc, init_bact_dens = my_init_bact, 
                                                init_moi = my_moi, equil = at_equil))
                    }
                  }
                  i <- i+1
                  
                  #Print progress update
                  if (print_info & i %in% progress_seq) {
                    print(paste((which(progress_seq == i)-1)*10,
                                "% completed", sep = ""))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #Pull out all the runs that didn't reach equilibrium
  y_noequil <- NULL
  for (run in unique(ybig$uniq_run[which(!ybig$equil)])) {
    if (is.null(y_noequil)) {
      y_noequil <- ybig[min(which(ybig$uniq_run == run)), 1:9]
    } else {
      y_noequil <- rbind(y_noequil, ybig[min(which(ybig$uniq_run == run)), 1:9])
    }
  }
  
  return(list(ybig, y_noequil, yfail))
  
  #Code for visualizing while debugging
  if (F) {
    #Code for plotting population sizes over time
    ymelt <- reshape2::melt(data = as.data.frame(yout_list[[2]]), 
                            id = c("time"),
                            value.name = "Density", 
                            variable.name = "Pop")
    
    ggplot(data = ymelt, 
           aes(x = time, y = Density+10, color = Pop)) +
      geom_line(lwd = 1.5, alpha = 1) + 
      scale_y_continuous(trans = "log10") +
      scale_x_continuous(breaks = seq(from = 0, to = max(ymelt$time), 
                                      by = round(max(ymelt$time)/10))) +
      geom_hline(yintercept = 10, lty = 2) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      NULL
  }
  
  #for troubleshooting runs that don't get to equilibrium
  if (F) {
    my_row <- 1
    myr <- y_noequil$r[my_row]
    mya <- y_noequil$a[my_row]
    myb <- y_noequil$b[my_row]
    mytau <- y_noequil$tau[my_row]
    myk <- y_noequil$K[my_row]
    myc <- y_noequil$c[my_row]
  }
  
  #for troubleshooting runs that fail
  if (F) {
    my_row <- 1
    myr <- yfail$r[my_row]
    mya <- yfail$a[my_row]
    myb <- yfail$b[my_row]
    mytau <- yfail$tau[my_row]
    myk <- yfail$K[my_row]
    myc <- yfail$c[my_row]
  }
}

## Define function that calculates derivatives ----
calc_deriv <- function(density, percapita = FALSE,
                       subset_by = NULL, time = NULL,
                       time_normalize = NULL) {
  #Note! density values must be sorted sequentially into their unique sets already
  
  #Provided a vector of density values, this function returns (by default) the
  # difference between sequential values
  #if percapita = TRUE, the differences of density are divided by density
  #if subset_by is provided, it should be a vector (same length as density),
  # the unique values of which will separate calculations
  #if time_normalize is specified, time should be provided as a simple 
  # numeric (e.g. number of seconds) in some unit
  #Then the difference will be normalized for the time_normalize value
  #(e.g. if time is provided in seconds and the difference per hour is wanted,
  # time_normalize should = 3600)
  
  #Check inputs
  if (!is.numeric(time)) {
    stop("time is not numeric")
  }
  if (!is.null(time_normalize)) {
    if (!is.numeric(time_normalize)) {
      stop("time_normalize is not numeric")
    } else if (is.null(time)) {
      stop("time_normalize is specified, but time is not provided")
    }
  }
  
  #Calc derivative
  ans <- c(density[2:length(density)]-density[1:(length(density)-1)])
  #Percapita (if specified)
  if (percapita) {
    ans <- ans/density[1:(length(density)-1)]
  }
  #Time normalize (if specified)
  if (!is.null(time_normalize)) {
    ans <- ans/
      (c(time[2:length(time)]-time[1:(length(time)-1)])/time_normalize)
  }
  #Subset by (if specified)
  if (!is.null(subset_by)) {
    ans[subset_by[2:length(subset_by)] != subset_by[1:(length(subset_by)-1)]] <- NA
  }
  return(c(ans, NA))
}

## Lit review of parameters ----
#r ranges from .04/min (17 min doubling time)
#         to 0.007/min (90 min doubling time)
#   ln(1/2) = -r * doub_time
#K ranges from say 10^7 to 10^9
#adsorption ranges from 1x10^-12 to 1x10^-8 /min
#Lysis time ranges from 10 to 105 mins
#Burst size ranges from 4.5 to 1000

## Run #1 ----

if (F) {
  run1 <- run_sims(rvals = c(0.023), #(30 min doubling time)
                   kvals = c(10**9),
                   avals = 10**seq(from = -12, to = -8, by = 1),
                   tauvals = signif(10**seq(from = 1, to = 2, by = 0.25), 3),
                   bvals = signif(5*10**seq(from = 0, to = 2, by = 0.5), 3),
                   cvals = 1,
                   init_bact_dens_vals = 10**6,
                   init_moi_vals = 10**-2,
                   min_dens = 0.1,
                   init_time = 100,
                   init_stepsize = 1,
                   print_info = TRUE)
  #Save results so they can be re-loaded in future
  write.csv(run1[[1]], "run1_1.csv", row.names = F)
  if (!is.null(run1[[2]])) {write.csv(run1[[2]], "run1_2.csv", row.names = F)}
  if (!is.null(run1[[3]])) {write.csv(run1[[3]], "run1_3.csv", row.names = F)}
} else {
  #Load results previously simulated
  temp1 <- read.csv("run1_1.csv", stringsAsFactors = F)
  if ("run1_2.csv" %in% list.files()) {
    temp2 <- read.csv("run1_2.csv", stringsAsFactors = F)
  } else {temp2 <- NULL}
  if ("run1_3.csv" %in% list.files()) {
    temp3 <- read.csv("run1_3.csv", stringsAsFactors = F)
  } else {temp3 <- NULL}
  run1 <- list(temp1, temp2, temp3)
}

#Find peaks & extinction via summarize
ybig1 <- group_by_at(run1[[1]], .vars = 1:9)
y_summarized1 <- summarize(ybig1,
                          max_dens = max(Density[Pop == "B"]),
                          max_time = time[Pop == "B" & 
                                            Density[Pop == "B"] == max_dens],
                          extin_index = min(which(Pop == "B" &
                                                    Density <= 10**4)),
                          extin_dens = Density[extin_index],
                          extin_time = time[extin_index],
                          auc = sum(Density[Pop == "B" & time < extin_time])*
                            extin_time,
                          phage_final = max(Density[Pop == "P"]),
                          phage_extin = Density[Pop == "P" & time == extin_time],
                          phage_r = (log(phage_final)-
                                       log(init_bact_dens[1]*init_moi[1]))/
                            extin_time
)
                          

#Calculate derivatives
ybig1$deriv <- calc_deriv(density = ybig1$Density, 
                           percapita = FALSE,
                           subset_by = paste(ybig1$uniq_run, ybig1$Pop), 
                           time = ybig1$time,
                           time_normalize = 60)
ybig1$deriv_percap <- calc_deriv(density = ybig1$Density, 
                                percapita = TRUE,
                                subset_by = paste(ybig1$uniq_run, ybig1$Pop), 
                                time = ybig1$time,
                                time_normalize = 60)

#Make plots of density against time ----
dens_offset <- 10
if (F) {
  for (run in unique(ybig1$uniq_run)) {
    tiff(paste("./run1_dens_curves/", run, ".tiff", sep = ""),
         width = 5, height = 5, units = "in", res = 300)
    print(
      ggplot(data = ybig1[ybig1$uniq_run == run &
                           ybig1$Pop %in% c("S", "I", "P"),], 
             aes(x = time, y = Density+dens_offset, color = Pop)) +
              geom_line(lwd = 1.5, alpha = 1) + 
        geom_line(data = ybig1[ybig1$uniq_run == run &
                                ybig1$Pop == "B",], 
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1.1) +
        geom_line(data = ybig1[ybig1$uniq_run == run &
                                ybig1$Pop == "PI",],
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1, lty = 3) +
        geom_point(data = y_summarized1[y_summarized1$uniq_run == run, ],
                   aes(x = max_time, y = max_dens+dens_offset), color = "black") +
        geom_point(data = y_summarized1[y_summarized1$uniq_run == run, ],
                   aes(x = extin_time, y = extin_dens+dens_offset), color = "black") +
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(breaks = seq(from = 0, to = max(ybig1$time), 
                                        by = round(max(ybig1[ybig1$uniq_run == run &
                                                            ybig1$Pop != "B", 
                                                            "time"])/10))) +
        scale_color_manual(values = my_cols[c(2, 3, 1)]) +
        geom_hline(yintercept = 10, lty = 2) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              title = element_text(size = 9)) +
        ggtitle(paste(ybig1[min(which(ybig1$uniq_run == run)), 2:9],
                      collapse = ", ")) +
        labs(y = paste("Density +", dens_offset)) +
        NULL
    )
    dev.off()
  }
}

#Plot summarized statistics ----
y_summarized1$b <- as.factor(y_summarized1$b)
y_summarized1$tau <- as.factor(y_summarized1$tau)
y_summarized1$a <- as.factor(y_summarized1$a)
#y_summarized1$r <- as.character(y_summarized1$r)
for (stat in c("max_dens", "max_time", "extin_time", 
               "auc", "phage_final", "phage_r")) {
  tiff(paste("./run1_statplots/", stat, ".tiff", sep = ""),
       width = 5, height = 5, units = "in", res = 300)
  print(ggplot(data = y_summarized1,
               aes(x = a, y = get(stat), color = b, group = b)) + 
          geom_point(size = 3, alpha = 0.8) + 
          geom_line(size = 1.1, alpha = 0.6) +
          facet_grid(~tau) +
          labs(y = stat) +
          scale_y_continuous(trans = "log10") +
          #scale_x_continuous(trans = "log10") +
          scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle("tau") +
          NULL)
  dev.off()
}

y_sum_melt1 <- reshape2::melt(y_summarized1,
                   id.vars = 1:9,
                   variable.name = "sum_stat",
                   value.name = "stat_val")

tiff("./run1_statplots/all_stats.tiff",
     width = 5, height = 6, units = "in", res = 300)
ggplot(data = y_sum_melt1[y_sum_melt1$sum_stat %in%
                            c("max_dens", "max_time", "extin_time", 
                              #"auc", 
                              "phage_final", 
                              "phage_r"
                              ), ],
       aes(x = a, y = stat_val, color = b, group = b)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  geom_line(size = 1.1, alpha = 0.6) +
  facet_grid(sum_stat~tau, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(size = 10)) +
  ggtitle("tau") +
  NULL
dev.off()

tiff("./run1_statplots/all_stats2.tiff",
     width = 5, height = 5, units = "in", res = 300)
ggplot(data = y_sum_melt1[y_sum_melt1$sum_stat != "extin_dens", ],
       aes(x = b, y = stat_val, color = tau, group = tau)) +
  geom_point(size = 2, alpha = 0.8) + 
  geom_line(size = 1.1, alpha = 0.6) +
  facet_grid(sum_stat~a, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("a") +
  NULL
dev.off()

tiff("./run1_statplots/all_stats3.tiff",
     width = 5, height = 5, units = "in", res = 300)
ggplot(data = y_sum_melt1[y_sum_melt1$sum_stat != "extin_dens", ],
       aes(x = tau, y = stat_val, color = a, group = a)) +
  geom_point(size = 2, alpha = 0.8) + 
  geom_line(size = 1.1, alpha = 0.6) +
  facet_grid(sum_stat~b, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("b") +
  NULL
dev.off()

#Plot stats against ea other ----

#First plot all at once
for (col in c("max_dens", "max_time",
              "extin_time", "auc", "phage_final", "phage_r")) {
  y_summarized1[, paste(col, "_log10", sep = "")] <- log10(y_summarized1[, col])
}

tiff("./run1_statplots/stat_cors.tiff", width = 10, height = 10, units = "in", res = 300)
#Make base figure
p <- GGally::ggpairs(y_summarized1,
                     aes(color = b, shape = a),
                     columns = c("max_dens_log10", "max_time_log10",
                                 "extin_time_log10", 
                                 #"auc_log10", 
                                 "phage_final_log10", 
                                 "phage_r_log10"),
                     lower = list(continuous = "points"),
                     upper = list(continuous = "points")) +
  theme_bw() +
  theme(strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
print(p)
dev.off()

#Then make indiv paired plots
tiff("./run1_statplots/maxdens_maxtime.tiff",
     width = 5, height = 5, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_dens, y = max_time, color = b, fill = b, 
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  scale_shape_manual(values = 21:25) +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  NULL
dev.off()

tiff("./run1_statplots/maxdens_maxtime_facet.tiff",
     width = 6, height = 4, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_dens, y = max_time, color = b, fill = b, 
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
  facet_grid(~tau) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  scale_shape_manual(values = 21:25) +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("tau") +
  NULL
dev.off()

tiff("./run1_statplots/maxdens_extintime.tiff",
     width = 5, height = 5, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_dens, y = extin_time, color = b, fill = b, 
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
  #facet_grid(tau~.) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  scale_shape_manual(values = 21:25) +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  NULL
dev.off()

tiff("./run1_statplots/maxdens_extintime_facet.tiff",
     width = 6, height = 4, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_dens, y = extin_time, color = b, fill = b, 
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
  facet_grid(~tau) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  scale_shape_manual(values = 21:25) +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("tau") +
  NULL
dev.off()

tiff("./run1_statplots/maxtime_extintime.tiff",
     width = 5, height = 5, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_time, y = extin_time, color = b, fill = b,
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
#  facet_grid(tau~.) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_shape_manual(values = 21:25) +
  theme_bw() +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  NULL
dev.off()

tiff("./run1_statplots/maxtime_extintime_facet.tiff",
     width = 6, height = 4, units = "in", res = 300)
ggplot(data = y_summarized1,
       aes(x = max_time, y = extin_time, color = b, fill = b,
           shape = a)) +
  geom_point(size = 2.5, alpha = 0.5) +
  facet_grid(~tau) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_shape_manual(values = 21:25) +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  scale_fill_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("tau") +
  NULL
dev.off()

#Make plots that include derivs
ybig_melt <- data.table::melt(as.data.table(ybig1),
                              measure.vars = c("Density", "deriv", "deriv_percap"),
                              variable.name = "var_measured",
                              value.name = "value")
for (run in unique(ybig_melt$uniq_run)) {
  tiff(paste("./run1_dens_and_derivs/", run, ".tiff", sep = ""),
       width = 4, height = 8, units = "in", res = 300)
  print(ggplot(data = ybig_melt[ybig_melt$uniq_run == run &
                                  ybig_melt$Pop == "B" &
                                  ybig_melt$time > 0, ],
               aes(x = time, y = value)) +
          geom_point() +
          facet_grid(var_measured~., scales = "free_y") +
          theme_bw())
  dev.off()
}

# Plot multiple B's on same axes ----
ybig1$a <- as.factor(ybig1$a)
ybig1$b <- as.factor(ybig1$b)
ybig1$tau <- as.factor(ybig1$tau)
tiff("./run1_statplots/B_plots.tiff", width = 10, height = 10, 
     units = "in", res = 300)
ggplot(data = ybig1[ybig1$Pop == "B" &
                     ybig1$Density > 0, ],
       aes(x = time, y = Density+10, color = a)) +
  geom_line(lwd = 1, alpha = 0.5) +
  geom_hline(yintercept = 10, lty = 2) +
  facet_grid(tau~b, scales = "free") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  ggtitle("b (top), tau (side)")
dev.off()

tiff("./run1_statplots/B_plots2.tiff", width = 10, height = 10, 
     units = "in", res = 300)
ggplot(data = ybig1[ybig1$Pop == "B" &
                     ybig1$Density > 0, ],
       aes(x = time, y = Density+10, color = tau)) +
  geom_line(lwd = 1, alpha = 0.5) +
  geom_hline(yintercept = 10, lty = 2) +
  facet_grid(b~a, scales = "free") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  ggtitle("a (top), b (side)")
dev.off()

tiff("./run1_statplots/B_plots3.tiff", width = 10, height = 10, 
     units = "in", res = 300)
ggplot(data = ybig1[ybig1$Pop == "B" &
                     ybig1$Density > 0, ],
       aes(x = time, y = Density+10, color = b)) +
  geom_line(lwd = 1, alpha = 0.5) +
  geom_hline(yintercept = 10, lty = 2) +
  facet_grid(tau~a, scales = "free") +
  scale_y_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  ggtitle("a (top), tau (side)")
dev.off()

## Run #2 ----
if (F) {
  run2 <- run_sims(rvals = signif(0.04*10**seq(from = 0, to = -0.7, by = -0.175), 3),
                   kvals = c(10**9),
                   avals = 10**seq(from = -12, to = -8, by = 1),
                   tauvals = signif(10**seq(from = 1, to = 2, by = 0.25), 3),
                   bvals = signif(5*10**seq(from = 0, to = 2, by = 0.5), 3),
                   cvals = 1,
                   init_bact_dens_vals = 10**6,
                   init_moi_vals = 10**-2,
                   min_dens = 0.1,
                   init_time = 100,
                   init_stepsize = 1,
                   print_info = TRUE)
  #Save results so they can be re-loaded in future
  write.csv(run2[[1]], "run2_1.csv", row.names = F)
  if (!is.null(run2[[2]])) {write.csv(run2[[2]], "run2_2.csv", row.names = F)}
  if (!is.null(run2[[3]])) {write.csv(run2[[3]], "run2_3.csv", row.names = F)}
} else {
  #Load results previously simulated
  temp1 <- read.csv("run2_1.csv", stringsAsFactors = F)
  if ("run2_2.csv" %in% list.files()) {
    temp2 <- read.csv("run2_2.csv", stringsAsFactors = F)
  } else {temp2 <- NULL}
  if ("run2_3.csv" %in% list.files()) {
    temp3 <- read.csv("run2_3.csv", stringsAsFactors = F)
  } else {temp3 <- NULL}
  run2 <- list(temp1, temp2, temp3)
}

#Check fails/no equils
run2[[2]]

run2[[3]]

#Find peaks & extinction via summarize
ybig2 <- group_by_at(run2[[1]], .vars = 1:9)
ybig2 <- ybig2[complete.cases(ybig2), ]
y_summarized2 <- summarize(ybig2,
                           max_dens = max(Density[Pop == "B"]),
                           max_time = time[Pop == "B" & 
                                             Density[Pop == "B"] == max_dens],
                           extin_index = min(which(Pop == "B" &
                                                     Density <= 10**4)),
                           extin_dens = Density[extin_index],
                           extin_time = time[extin_index],
                           extin_time_sincemax = extin_time-max_time,
                           auc = sum(Density[Pop == "B" & time < extin_time])*
                             extin_time,
                           phage_final = max(Density[Pop == "P"]),
                           phage_extin = Density[Pop == "P" & time == extin_time],
                           phage_r = (log(phage_final)-
                                        log(init_bact_dens[1]*init_moi[1]))/
                             extin_time,
                           phage_atmaxdens = Density[Pop == "P" & time == max_time]
)

## Plot summarized stats ----
y_sum_melt2 <- reshape2::melt(y_summarized2,
                              id.vars = 1:9,
                              variable.name = "sum_stat",
                              value.name = "stat_val")

y_sum_melt2$b <- as.factor(y_sum_melt2$b)
for (myr in unique(y_sum_melt2$r)) {
  tiff(paste("./run2_statplots/all_stats_r=", 
             formatC(myr, digits = 5, format = "f"), 
             ".tiff", sep = ""),
       width = 5, height = 7, units = "in", res = 300)
  print(ggplot(data = y_sum_melt2[y_sum_melt2$r == myr &
                              y_sum_melt2$sum_stat %in% 
                              c("max_dens", "max_time", 
                                "extin_time", 
                                #"extin_time_sincemax",
                                "phage_final", 
                                "phage_r",
                                "phage_atmaxdens"
                                ), ],
         aes(x = a, y = stat_val, color = b, group = b)) +
    geom_point(size = 2, alpha = 0.8) + 
    geom_line(size = 1.1, alpha = 0.6) +
    facet_grid(sum_stat~tau, scales = "free_y") +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("r=", myr, " tau", sep = "")) +
    NULL
  )
  dev.off()
}

##Just extin_time ----
ggplot(data = y_summarized2,
       aes(x = a, y = extin_time, color = b, group = b)) +
  #geom_point() + 
  geom_line(lwd = 1.25, alpha = 0.8) +
  scale_color_manual(values = my_cols[c(1, 2, 3, 5, 7)]) +
  facet_grid(r~tau) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = y_summarized2,
       aes(x = b, y = extin_time, color = as.factor(r),
           group = as.factor(r))) +
  #geom_point() + 
  geom_line(lwd = 1.25, alpha = 0.8) +
  scale_color_manual(values = my_cols[c(1, 2, 3, 5, 7)]) +
  facet_grid(tau~a) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = y_summarized2,
       aes(x = r, y = extin_time, color = as.factor(tau),
           group = as.factor(tau))) +
  #geom_point() + 
  geom_line(lwd = 1.25, alpha = 0.8) +
  scale_color_manual(values = my_cols[c(1, 2, 3, 5, 7)]) +
  facet_grid(a~b) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = y_summarized2,
       aes(x = tau, y = extin_time, color = as.factor(a),
           group = as.factor(a))) +
  #geom_point() + 
  geom_line(lwd = 1.25, alpha = 0.8) +
  scale_color_manual(values = my_cols[c(1, 2, 3, 5, 7)]) +
  facet_grid(b~r) +
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Plot stats against ea other ----

##First plot all at once

#Calculate log10's
for (col in c("max_dens", "max_time", "extin_time", "extin_time_sincemax",
              "auc", "phage_final", "phage_r")) {
  y_summarized2[, paste(col, "_log10", sep = "")] <- log10(y_summarized2[, col])
}

#Make plots
y_summarized2$a <- as.factor(y_summarized2$a)
y_summarized2$b <- as.factor(y_summarized2$b)
for (myr in unique(y_summarized2$r)) {
  tiff(paste("./run2_statplots/stat_cors_r=", 
             formatC(myr, digits = 5, format = "f"),
             ".tiff", sep = ""),
       width = 15, height = 15, units = "in", res = 300)
  #Make base figure
  p <- GGally::ggpairs(y_summarized2[y_summarized2$r == myr, ],
                       aes(color = b, shape = a),
                       columns = c("max_dens_log10", "max_time_log10",
                                   "extin_time_log10", 
                                   "extin_time_sincemax_log10",
                                   "auc_log10", 
                                   "phage_final_log10", "phage_r_log10"),
                       lower = list(continuous = "points"),
                       upper = list(continuous = "points")) +
    theme_bw() +
    theme(strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 0))
  print(p)
  dev.off()
}

##Now selected pairs, looking for underlying functions

#Relating max_dens to max_time
max_dens_func <- function(t, K, P_0, r) log10(K/(1+((K-P_0)/P_0)*exp(-r*t)))
for (myr in unique(y_summarized2$r)) {
  tiff(paste("./run2_statplots/maxdens_maxtime_r=", myr, ".tiff", sep = ""),
       width = 5, height = 5, units = "in", res = 300)
  print(ggplot(data = y_summarized2[y_summarized2$r == myr, ],
               aes(x = max_time, y = max_dens, color = a, shape = b)) +
          geom_point() +
          scale_y_continuous(trans = "log10") +
          stat_function(fun = max_dens_func,
                        args = list(K = 10**9, P_0 = 1*10**6, r = myr),
                        color = "black", lwd = 1, alpha = 0.1) +
          ggtitle(paste("r =", myr)) +
          theme_bw()
  )
  dev.off()
}

#Relating phage_final to max_dens and b
y_summarized2$tau <- as.factor(y_summarized2$tau)

#First try fitting a model to the data
temp <- y_summarized2[y_summarized2$r == 0.00798 &
                        y_summarized2$max_dens_log10 < 8.95, ]
model1 <- lm(phage_final_log10 ~ max_dens_log10 + b,
             temp)
summary(model1)

ggplot(data = temp,
       aes(x = max_dens_log10, y = phage_final_log10, color = b)) +
  geom_point() +
  geom_line(data = fortify(model2), aes(x = max_dens_log10, y = .fitted))

#The model seems to suggest the following "true" underlying model:
phage_final_func <- function(x, a, b, tau) {
  log10(b*x)
}

#Pre-define all the stat_functions into a list to add all at once
stat_func_list1 <- list()
i <- 1
for (myb in unique(y_summarized2$b)) {
  stat_func_list1[[i]] <- stat_function(fun = phage_final_func,
                                        args = list(b = as.numeric(as.character(myb))),
                                        color = colorRampPalette(colors = c("gold", "dark red"))(5)[i])
  i <- i+1
}

#Make plots
for (myr in unique(y_summarized2$r)) {
#myr <- 0.00798
  tiff(paste("./run2_statplots/phagefinal_maxdens_r=", myr, ".tiff", sep = ""),
       width = 5, height = 5, units = "in", res = 300)
  print(ggplot(data = y_summarized2[y_summarized2$r == myr, ],
               aes(x = max_dens, y = phage_final, color = b, shape = tau)) +
          geom_point() +
          scale_y_continuous(trans = "log10") +
          scale_x_continuous(trans = "log10") +
          scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
          ggtitle(paste("r =", myr)) +
          stat_func_list1 +
          theme_bw() +
          NULL
  )
  dev.off()
}

#Since max_dens is related to max_time, and phage_final is related to max_dens
#We can relate phage_final to max_time
phage_final_func2 <- function(max_time, K, P_0, r, b) {
  #note output is in terms of phage_final_log10
  #log10(phage_final) = log10(K/(1+((K-P_0)/P_0)*exp(-r*max_time))) + log10(b)
  log10(K/(1+((K-P_0)/P_0)*exp(-r*max_time))) + log10(b)
}

#Pre-define all the stat_functions into a list to add all at once
# (this is a nested list, the upper level corresponding to different
# r values, and the inner level to different b values)
stat_func_list2 <- list()
i <- 1
j <- 1
for (myr in unique(y_summarized2$r)) {
  j <- 1
  stat_func_list2[[i]] <- list()
  for (myb in unique(y_summarized2$b)) {
    stat_func_list2[[i]][[j]] <- stat_function(fun = phage_final_func2,
                                          args = list(b = as.numeric(as.character(myb)), 
                                                      K = 10**9, P_0 = 10**6, r = myr),
                                          color = colorRampPalette(colors = c("gold", "dark red"))(5)[j])
    j <- j+1
  }
  i <- i+1
}

i <- 1
for (myr in unique(y_summarized2$r)) {
#myr <- 0.00798
  tiff(paste("./run2_statplots/phagefinal_maxtime_r=", myr, ".tiff", sep = ""),
       width = 5, height = 5, units = "in", res = 300)
  print(ggplot(data = y_summarized2[y_summarized2$r == myr, ],
               aes(x = max_time, y = phage_final_log10, color = b, shape = a)) +
          geom_point() +
          stat_func_list2[[i]] +
          scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
          ggtitle(paste("r =", myr)) +
          theme_bw() +
          NULL
        )
  dev.off()
  i <- i+1
}

##Run #3 ----

if (F) {
  run3 <- run_sims(rvals = c(0.04, 0.0179),
                 kvals = c(10**9),
                 avals = 10**seq(from = -12, to = -8, by = 2),
                 tauvals = signif(20**seq(from = 1, to = 1.5, by = 0.5), 3),
                 bvals = signif(5*10**seq(from = 1, to = 2, by = 1), 3),
                 cvals = 1,
                 init_bact_dens_vals = c(10**4, 10**5, 10**6),
                 init_moi_vals = c(10**-2, 10**-1, 1),
                 min_dens = 0.1,
                 init_time = 100,
                 init_stepsize = 1,
                 print_info = TRUE)
  #Save results so they can be re-loaded in future
  write.csv(run3[[1]], "run3_1.csv", row.names = F)
  if (!is.null(run3[[2]])) {write.csv(run3[[2]], "run3_2.csv", row.names = F)}
  if (!is.null(run3[[3]])) {write.csv(run3[[3]], "run3_3.csv", row.names = F)}
} else {
  #Load results previously simulated
  temp1 <- read.csv("run3_1.csv", stringsAsFactors = F)
  if ("run3_2.csv" %in% list.files()) {
    temp2 <- read.csv("run3_2.csv", stringsAsFactors = F)
  } else {temp2 <- NULL}
  if ("run3_3.csv" %in% list.files()) {
    temp3 <- read.csv("run3_3.csv", stringsAsFactors = F)
  } else {temp3 <- NULL}
  run3 <- list(temp1, temp2, temp3)
}

#Check fails/no equils
run3[[2]]

run3[[3]]

#Find peaks & extinction via summarize
ybig3 <- group_by_at(run3[[1]], .vars = 1:9)
ybig3 <- ybig3[complete.cases(ybig3), ]
y_summarized3 <- summarize(ybig3,
                           max_dens = max(Density[Pop == "B"]),
                           max_time = time[Pop == "B" & 
                                             Density[Pop == "B"] == max_dens],
                           extin_index = min(which(Pop == "B" &
                                                     Density <= 10**3)),
                           extin_dens = Density[extin_index],
                           extin_time = time[extin_index],
                           extin_time_sincemax = extin_time-max_time,
                           auc = sum(Density[Pop == "B" & time < extin_time])*
                             extin_time,
                           phage_final = max(Density[Pop == "P"]),
                           phage_extin = Density[Pop == "P" & time == extin_time],
                           phage_r = (log(phage_final)-
                                        log(init_bact_dens[1]*init_moi[1]))/
                             extin_time
)

#Make plots of density against time ----
dens_offset <- 10
if (F) {
  for (run in unique(ybig3$uniq_run)) {
    tiff(paste("./run3_dens_curves/", run, ".tiff", sep = ""),
         width = 5, height = 5, units = "in", res = 300)
    print(
      ggplot(data = ybig3[ybig3$uniq_run == run &
                            ybig3$Pop %in% c("S", "I", "P"),], 
             aes(x = time, y = Density+dens_offset, color = Pop)) +
        geom_line(lwd = 1.5, alpha = 1) + 
        geom_line(data = ybig3[ybig3$uniq_run == run &
                                 ybig3$Pop == "B",], 
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1.1) +
        geom_line(data = ybig3[ybig3$uniq_run == run &
                                 ybig3$Pop == "PI",],
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1, lty = 3) +
        geom_point(data = y_summarized3[y_summarized3$uniq_run == run, ],
                   aes(x = max_time, y = max_dens+dens_offset), color = "black") +
        geom_point(data = y_summarized3[y_summarized3$uniq_run == run, ],
                   aes(x = extin_time, y = extin_dens+dens_offset), color = "black") +
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(breaks = seq(from = 0, to = max(ybig3$time), 
                                        by = round(max(ybig3[ybig3$uniq_run == run &
                                                               ybig3$Pop != "B", 
                                                             "time"])/10))) +
        scale_color_manual(values = my_cols[c(2, 3, 1)]) +
        geom_hline(yintercept = 10, lty = 2) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              title = element_text(size = 9)) +
        ggtitle(paste(ybig3[min(which(ybig3$uniq_run == run)), 2:9],
                      collapse = ", ")) +
        labs(y = paste("Density +", dens_offset)) +
        NULL
    )
    dev.off()
  }
}

## Plot summarized stats ----
y_sum_melt3 <- reshape2::melt(y_summarized3,
                              id.vars = 1:9,
                              variable.name = "sum_stat",
                              value.name = "stat_val")

y_sum_melt3$init_moi <- as.factor(y_sum_melt3$init_moi)
for (myr in unique(y_sum_melt3$r)) {
  for (myb in unique(y_sum_melt3$b)) {
    tiff(paste("./run3_statplots/all_stats_r=", 
               formatC(myr, digits = 5, format = "f"), 
               ",b=", myb, ".tiff", sep = ""),
         width = 5, height = 7, units = "in", res = 300)
    print(ggplot(data = y_sum_melt3[y_sum_melt3$r == myr &
                                      y_sum_melt3$b == myb &
                                      y_sum_melt3$sum_stat %in% 
                                      c("max_dens", "max_time", 
                                        "extin_time", "extin_time_sincemax",
                                        "phage_final", "phage_r"), ],
                 aes(x = init_bact_dens, y = stat_val, color = init_moi, group = init_moi)) +
            geom_point(size = 2, alpha = 0.8) + 
            geom_line(size = 1.1, alpha = 0.6) +
            facet_grid(sum_stat~tau*a, scales = "free_y") +
            scale_y_continuous(trans = "log10") +
            scale_x_continuous(trans = "log10") +
            scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste("r=", myr, " tau", sep = "")) +
            NULL
    )
    dev.off()
  }
}

#Let's focus in on extin_time
ggplot(data = y_summarized3,
       aes(x = init_bact_dens, y = extin_time, color = init_moi,
           shape = a)) +
  geom_point() +
  facet_grid(r~b*tau) +
#  geom_line() +
  theme_bw() +
  NULL

###Plot stats against ea other ----

##First plot all at once

#Calculate log10's
for (col in c("max_dens", "max_time", "extin_time", "extin_time_sincemax",
              "auc", "phage_final", "phage_r")) {
  y_summarized3[, paste(col, "_log10", sep = "")] <- log10(y_summarized3[, col])
}

#Make plots
y_summarized3$init_moi <- as.factor(y_summarized3$init_moi)
y_summarized3$init_bact_dens <- as.factor(y_summarized3$init_bact_dens)
for (myr in unique(y_summarized3$r)) {
  tiff(paste("./run3_statplots/stat_cors_r=", 
             formatC(myr, digits = 5, format = "f"),
             ".tiff", sep = ""),
       width = 15, height = 15, units = "in", res = 300)
  #Make base figure
  p <- GGally::ggpairs(y_summarized3[y_summarized3$r == myr, ],
                       aes(color = init_moi, shape = init_bact_dens),
                       columns = c("max_dens_log10", "max_time_log10",
                                   "extin_time_log10", 
                                   "extin_time_sincemax_log10",
                                   "auc_log10", 
                                   "phage_final_log10", "phage_r_log10"),
                       lower = list(continuous = "points"),
                       upper = list(continuous = "points")) +
    theme_bw() +
    theme(strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 0))
  print(p)
  dev.off()
}

###Run #4 ----

if (F) {
  run4 <- run_sims(rvals = signif(0.04*10**seq(from = 1, to = -2, by = -0.67), 3),
                   kvals = c(10**9),
                   avals = 10**seq(from = -14, to = -6, by = 2),
                   tauvals = signif(10**seq(from = 0, to = 3, by = 0.75), 3),
                   bvals = signif(5*10**seq(from = -1, to = 3, by = 1), 3),
                   cvals = 1,
                   init_bact_dens_vals = 10**6,
                   init_moi_vals = 10**-2,
                   min_dens = 0.1,
                   init_time = 100,
                   init_stepsize = 1,
                   print_info = TRUE)
  #Save results so they can be re-loaded in future
  write.csv(run4[[1]], "run4_1.csv", row.names = F)
  if (!is.null(run4[[2]])) {write.csv(run4[[2]], "run4_2.csv", row.names = F)}
  if (!is.null(run4[[3]])) {write.csv(run4[[3]], "run4_3.csv", row.names = F)}
} else {
  #Load results previously simulated
  temp1 <- read.csv("run4_1.csv", stringsAsFactors = F)
  if ("run4_2.csv" %in% list.files()) {
    temp2 <- read.csv("run4_2.csv", stringsAsFactors = F)
  } else {temp2 <- NULL}
  if ("run4_3.csv" %in% list.files()) {
    temp3 <- read.csv("run4_3.csv", stringsAsFactors = F)
  } else {temp3 <- NULL}
  run4 <- list(temp1, temp2, temp3)
}

#Check fails/no equils
run4[[2]]

run4[[3]]

#Find peaks & extinction via summarize
ybig4 <- group_by_at(run4[[1]], .vars = 1:9)
ybig4 <- ybig4[complete.cases(ybig4), ]
y_summarized4 <- summarize(ybig4,
                           max_dens = max(Density[Pop == "B"]),
                           max_time = time[Pop == "B" & 
                                             Density[Pop == "B"] == max_dens],
                           extin_index = min(which(Pop == "B" &
                                                     Density <= 10**4)),
                           extin_dens = Density[extin_index],
                           extin_time = time[extin_index],
                           extin_time_sincemax = extin_time-max_time,
                           auc = sum(Density[Pop == "B" & time < extin_time])*
                             extin_time,
                           phage_final = max(Density[Pop == "P"]),
                           phage_extin = Density[Pop == "P" & time == extin_time],
                           phage_r = (log(phage_final)-
                                        log(init_bact_dens[1]*init_moi[1]))/
                             extin_time
)

## Plot summarized stats ----
y_sum_melt4 <- reshape2::melt(y_summarized4,
                              id.vars = 1:9,
                              variable.name = "sum_stat",
                              value.name = "stat_val")

y_sum_melt4$b <- as.factor(y_sum_melt4$b)
for (myr in unique(y_sum_melt4$r)) {
  tiff(paste("./run2_statplots/all_stats_r=", 
             formatC(myr, digits = 5, format = "f"), 
             ".tiff", sep = ""),
       width = 5, height = 7, units = "in", res = 300)
  print(ggplot(data = y_sum_melt4[y_sum_melt4$r == myr &
                                    y_sum_melt4$sum_stat %in% 
                                    c("max_dens", "max_time", 
                                      "extin_time", "extin_time_sincemax",
                                      "phage_final", "phage_r"), ],
               aes(x = a, y = stat_val, color = b, group = b)) +
          geom_point(size = 2, alpha = 0.8) + 
          geom_line(size = 1.1, alpha = 0.6) +
          facet_grid(sum_stat~tau, scales = "free_y") +
          scale_y_continuous(trans = "log10") +
          scale_x_continuous(trans = "log10") +
          scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle(paste("r=", myr, " tau", sep = "")) +
          NULL
  )
  dev.off()
}



###Work in progress below: ----

##Relating max dens to extin_time

#TODO: explore other sigmoid functions
# (e.g. geeralized logistic function)
#Note that, because it is logistic-looking in log-space the underlying shape
# is never going to be logistic.
#Could also look into something where the percap growth rate
# grows then decays with distance to midpoint or something like that

#Define squared percent errors function
sq_err_func <- function(params, x_vals, y_vals) {
  #first input is a vector containing all the parameters
  # 1 - r
  # 2 - K
  # 3 - P_0
  #second input is the vector of x values
  r <- params["r"]
  K <- params["K"]
  P_0 <- params["P_0"]
  pred_vals <- K/(1+((K-P_0)/P_0)*exp(-r*x_vals))
 # return(sum((100*(pred_vals-y_vals))**2))
  return(sum((100*(log10(pred_vals)-log10(y_vals)))**2))
  #return(sum((100*(pred_vals-y_vals)/y_vals)**2))
}

#this stuff is producing warnings even when not run:
# Warning messages:
#   1: Unknown or uninitialised column: 'par'

# #Find fits
# fit_output <- data.frame(myr = numeric(), r = numeric(), 
#                          K = numeric(), P_0 = numeric())
# # for (myr in unique(y_summarized2$r)) {
#   myr <- 0.00798
#   temp <- optim(fn = sq_err_func,
#                 #par = c(r = myr, K = 10**9, P_0 = 10**6), 
#                 x_vals = y_summarized2$extin_time[y_summarized2$r == myr],
#                 y_vals = y_summarized2$max_dens[y_summarized2$r == myr],
#                 method = "BFGS")
#   fit_output <- rbind(fit_output,
#                       data.frame(myr = myr,
#                                  r = temp$par["r"],
#                                  K = temp$par["K"],
#                                  P_0 = temp$par["P_0"]))
# # }

#Calc ratio of logistic curve's fit r to the r in the simulations (myr)
fit_output$r_divby_myr <- fit_output$r/fit_output$myr

#Relating max_dens to extin_time
max_dens_func2 <- function(extin_time, K, P_0, r) {
  log10(K/(1+((K-P_0)/P_0)*exp(-r*extin_time)))
}
  
#for (myr in unique(y_summarized2$r)) {
  myr <- 0.00798
  myrow <- which(fit_output$myr == myr)
  print(ggplot(data = y_summarized2[y_summarized2$r == myr, ],
               aes(x = extin_time, y = max_dens, color = b, shape = a)) +
          geom_point() +
          # stat_function(fun = max_dens_func2,
          #               args = list(K = fit_output$K[myrow], 
          #                           P_0 = fit_output$P_0[myrow], 
          #                           r = fit_output$r[myrow]),
          #               color = "black", alpha = 0.1, lwd = 1) +
          # stat_function(fun = max_dens_func2,
          #               args = list(K = 10**9, 
          #                           P_0 = 7*10**5, 
          #                           r = .007),
          #               color = "black", alpha = 0.1, lwd = 1) +
          scale_y_continuous(trans = "log10") +
          scale_x_continuous(trans = "log10") +
          ggtitle(paste("r =", myr)) +
          theme_bw() +
          NULL
        )
#}

##Relating phage_r to extin_time
y_summarized2$tau <- as.factor(y_summarized2$tau)

phage_r_model1 <- lm(phage_r_log10 ~ extin_time_log10 + 
                       a + a:extin_time_log10 + 
                       b + b:extin_time_log10 +
                       tau + tau:extin_time_log10,
                     y_summarized2)
anova(phage_r_model1)
summary(phage_r_model1)

  #for (myr in unique(y_summarized2$r)) {
  myr <- 0.00798
  print(ggplot(data = y_summarized2[y_summarized2$max_dens_log10 < 8.95, ],
               aes(x = extin_time_log10, y = phage_r_log10, 
                   color = a, shape = tau)) +
          geom_point(alpha = 0.5) +
#          facet_grid(~b) +
          # stat_function(fun = max_dens_func2,
          #               args = list(K = 10**9, P_0 = 10**6, r = myr)) +
#          scale_y_continuous(trans = "log10") +
#          ggtitle(paste("r =", myr)) +
          NULL)
  #}

for (myr in unique(y_summarized2$r)) {
  #myr <- 0.00798
  tiff(paste("./run2_statplots/phager_extintime_r=", myr, ".tiff", sep = ""),
       width = 6, height = 5, units = "in", res = 300)
  print(ggplot(data = y_summarized2[y_summarized2$r == myr, ],
               aes(x = extin_time, y = phage_r, 
                   color = a, shape = tau)) +
          geom_point(alpha = 0.8, size = 2) +
          theme_bw() +
          scale_y_continuous(trans = "log10") +
          scale_x_continuous(trans = "log10") +
          ggtitle(paste("r =", myr)) +
          NULL
  )
  dev.off()
}


##Testing whether B decay after max_time can be fit with a
# logistic-like curve
temp1 <- ybig1[ybig1$uniq_run == 4, ]
temp2 <- y_summarized1[y_summarized1$uniq_run == 4, ]

func1 <- function(t, max_dens, max_time, r) {
  #max_dens - exp(r*(t-max_time))
  max_dens - (max_dens/(1+exp(-r*(t-max_time))))
}

func1_log <- function(t, max_dens, max_time, r) {
  log10(max_dens - (max_dens/(1+exp(-r*(t-max_time)))))
}

func1_optim <- function(params, times, density) {
  max_dens = params[["max_dens"]]
  max_time = params[["max_time"]]
  r = params[["r"]]
  fit_data <- func1(t = times, max_dens = max_dens,
                    max_time = max_time, r = r)
  return(sum((density-fit_data)**2))
}

fit1 <- optim(par = list(max_dens = temp2$max_dens,
                        max_time = temp2$max_time,
                        r = 0.15),
             fn = func1_optim,
             times = temp1$time[temp1$time >= temp2$max_time],
             density = temp1$Density[temp1$time >= temp2$max_time])
               

ggplot(data = temp1[temp1$Pop == "B", ],
       aes(x = time, y = Density+10)) +
  geom_line(aes(color = Pop, group = Pop),
            lwd = 1, alpha = 1) +
  geom_hline(yintercept = 10, lty = 2) +
  stat_function(mapping = aes(x = time),
                fun = func1_log,
                args = list(max_dens = 59450913826, #temp2$max_dens,
                            max_time = 19810832468, #temp2$max_time+60,
                            r = 14138914293), # 0.15),
                color = "black") +
  scale_y_continuous(trans = "log10", limits = c(10, NA)) +
  NULL


