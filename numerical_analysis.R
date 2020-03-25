#TODO:
# functionalize the big run
# look at derivative plot
# look at deriv per capita plot
# extract data from above plots
# run curves with 0 phage (- control, so to speak)
# run curves varying initial density & moi
# given control curves and curves varying in density & moi,
#   figure out how one could calculate phage parameters
#   or phage fitness
# can dede itself handle a stop-at-equilibrium condition?
#   Yes, they're called roots. However, it's not clear whether
#     it would really make things faster or not

## Import libraries ----

library(deSolve)
library(reshape2)
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
                     "equil" = NA, "time" = NA, "Pop" = NA, "Density" = NA)
                                        
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
                                   reshape2::melt(data = as.data.frame(yout_list[[2]]), 
                                                  id = c("time"),
                                                  value.name = "Density", 
                                                  variable.name = "Pop"))
                    
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

## Lit review of parameters ----
#r ranges from .04/min (17 min doubling time)
#         to 0.007/min (90 min doubling time)
#   ln(1/2) = -r * doub_time
#K ranges from say 10^7 to 10^9
#adsorption ranges from 1x10^-12 to 1x10^-8
#Lysis time ranges from 10 to 105 mins
#Burst size ranges from 4.5 to 1000

## Run #1 ----
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

#Find peaks & extinction via summarize
ybig <- group_by_at(run1[[1]], .vars = 1:9)
y_summarized <- summarize(ybig[ybig$Pop == "B", ],
                          max_dens = max(Density),
                          max_time = time[Density == max_dens],
                          extin_dens = Density[min(which(Density <= 10**4))],
                          extin_time = time[min(which(Density <= 10**4))])

#Make plots of density against time
dens_offset <- 10
if (F) {
  for (run in unique(ybig$uniq_run)) {
  #for (run in seq(from = 395, to = 399, by = 1)) {
    tiff(paste("./sim_curves_dens/", run, ".tiff", sep = ""),
         width = 5, height = 5, units = "in", res = 300)
    print(
      ggplot(data = ybig[ybig$uniq_run == run &
                           ybig$Pop %in% c("S", "I", "P"),], 
             aes(x = time, y = Density+dens_offset, color = Pop)) +
              geom_line(lwd = 1.5, alpha = 1) + 
        geom_line(data = ybig[ybig$uniq_run == run &
                                ybig$Pop == "B",], 
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1.1) +
        geom_line(data = ybig[ybig$uniq_run == run &
                                ybig$Pop == "PI",],
                  aes(x = time, y = Density+dens_offset),
                  color = "black", alpha = 0.5, lwd = 1, lty = 3) +
        geom_point(data = y_summarized[y_summarized$uniq_run == run, ],
                   aes(x = max_time, y = max_dens+dens_offset), color = "black") +
        geom_point(data = y_summarized[y_summarized$uniq_run == run, ],
                   aes(x = extin_time, y = extin_dens+dens_offset), color = "black") +
        scale_y_continuous(trans = "log10") +
        scale_x_continuous(breaks = seq(from = 0, to = max(ybig$time), 
                                        by = round(max(ybig[ybig$uniq_run == run &
                                                            ybig$Pop != "B", 
                                                            "time"])/10))) +
        scale_color_manual(values = my_cols[c(2, 3, 1)]) +
        geom_hline(yintercept = 10, lty = 2) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              title = element_text(size = 9)) +
        ggtitle(paste(ybig[min(which(ybig$uniq_run == run)), 2:9],
                      collapse = ", ")) +
        labs(y = paste("Density +", dens_offset)) +
        NULL
    )
    dev.off()
  }
}

#Plot summarized statistics
y_summarized$b <- as.factor(y_summarized$b)
y_summarized$tau <- as.factor(y_summarized$tau)
y_summarized$a <- as.factor(y_summarized$a)
#y_summarized$r <- as.character(y_summarized$r)
for (stat in c("max_dens", "max_time", "extin_time")) {
  print(ggplot(data = y_summarized,
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
          NULL
  )
}

y_sum_melt1 <- melt(y_summarized,
                   id.vars = 1:9,
                   variable.name = "sum_stat",
                   value.name = "stat_val")

ggplot(data = y_sum_melt[y_sum_melt$sum_stat != "extin_dens", ],
       aes(x = a, y = stat_val, color = b, group = b)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_line(size = 1.1, alpha = 0.6) +
  facet_grid(sum_stat~tau, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "log10") +
  scale_color_manual(values = colorRampPalette(colors = c("gold", "dark red"))(5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("tau") +
  NULL

ggplot(data = y_summarized,
       aes(x = max_dens, y = max_time, color = a, 
           shape = b, group = b)) +
  geom_point(size = 2.5, alpha = 0.5) +
  geom_line() +
  facet_grid(tau~.) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

ggplot(data = y_summarized,
       aes(x = max_dens, y = extin_time, color = a, 
           shape = b)) +
  geom_point(size = 2.5, alpha = 0.5) +
  facet_grid(tau~.) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

ggplot(data = y_summarized,
       aes(x = max_time, y = extin_time, color = a, 
           shape = b)) +
  geom_point(size = 2.5, alpha = 0.5) +
  facet_grid(tau~.) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")