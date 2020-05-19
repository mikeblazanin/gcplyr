## Import libraries ----

library(deSolve)
#library(reshape2)
library(data.table)
library(ggplot2)
library(dplyr)

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

###Run #4 ----

if (T) {
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