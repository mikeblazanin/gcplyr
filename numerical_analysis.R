library(deSolve)
library(reshape2)
library(ggplot2)

#Okabe and Ito 2008 colorblind-safe qualitative color scale
my_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7", "#000000")
#scales::show_col(my_cols)

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
  dY["S"] <- parms["r"] * y["S"] * ((parms["K"] - y["S"])/parms["K"]) - 
    parms["a"] * y["S"] * y["P"]
  
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

#Based on lit review:
#r ranges from .04/min (17 min doubling time)
#         to 0.007/min (90 min doubling time)
#   ln(1/2) = -r * doub_time
#K ranges from say 10^7 to 10^9
#adsorption ranges from 1x10^-12 to 1x10^-8
#Lysis time ranges from 10 to 105 mins
#Burst size ranges from 4.5 to 1000

rseq <- c(0.04, 0.013, 0.004)
#rseq <- 4*10**seq(from = -2, to = -3, by = -0.5)
aseq <- 10**seq(from = -12, to = -8, by = 1)
bseq <- c(20, 200)
#bseq <- c(5, 50, 500)
tauseq <- c(10, 100)
#tauseq <- c(10, 32, 100)
kseq <- c(10**9)
#kseq <- 10**seq(from = 7, to = 9, by = 1)
cseq <- c(0, 1)
init_dens_seq <- c(10**6)
init_moi_seq <- c(10**-2)

#Number of simulations that will be run
length(rseq)*length(kseq)*length(aseq)*length(tauseq)*length(bseq)*
  length(cseq)*length(init_dens_seq)*length(init_moi_seq)

i <- 1
yfail <- NULL
min_dens <- 0.1 #minimum density to be considered at equilibrium
for (myr in rseq) {
  for (myk in kseq) {
    for (mya in aseq) {
      for (mytau in tauseq) {
        for (myb in bseq) {
          for (myc in cseq) {
            for (my_init_bact in init_dens_seq) {
              for (my_moi in init_moi_seq) {
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
                  times <- seq(0, 100*2**j, 2**j)
                  
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
                                parms = params, hmax = 2**(j-k))))
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
                  
                  #Reshape and combine with other outputs
                  ymelt <- cbind(data.frame(uniq_run = i, r = myr, a = mya, 
                                            b = myb, tau = mytau, K = myk, 
                                            c = myc, init_dens = my_init_bact, 
                                            init_moi = my_moi, equil = at_equil),
                                 reshape2::melt(data = as.data.frame(yout_list[[2]]), 
                                                id = c("time"),
                                                value.name = "Density", 
                                                variable.name = "Pop"))
                  if (i == 1) {ybig <- ymelt #This is the first run
                  } else {
                    ybig <- rbind(ybig, ymelt) #It's a non-first run
                  }
                  
                #If the run failed
                } else {
                  if (is.null(yfail)) { #This is the first failed run
                    yfail <- data.frame(uniq_run = i, r = myr, a = mya, 
                                         b = myb, tau = mytau, K = myk,
                                        c = myc, init_dens = my_init_bact, 
                                         init_moi = my_moi, equil = at_equil)
                  } else { #This is a non-first failed run
                    yfail <- rbind(yfail, 
                                   data.frame(uniq_run = i, r = myr, a = mya, 
                                              b = myb, tau = mytau, K = myk, 
                                              c = myc, init_dens = my_init_bact, 
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


#Check what runs didn't reach equilibrium
y_noequil <- NULL
for (run in unique(ybig$uniq_run[which(!ybig$equil)])) {
  if (is.null(y_noequil)) {
    y_noequil <- ybig[min(which(ybig$uniq_run == run)), 1:9]
  } else {
    y_noequil <- rbind(y_noequil, ybig[min(which(ybig$uniq_run == run)), 1:9])
  }
}
y_noequil

#for troubleshooting
if (F) {
  my_row <- 1
  myr <- y_noequil$r[my_row]
  mya <- y_noequil$a[my_row]
  myb <- y_noequil$b[my_row]
  mytau <- y_noequil$tau[my_row]
  myk <- y_noequil$K[my_row]
  myc <- y_noequil$c[my_row]
}

#Check what runs failed
print(yfail)

#for troubleshooting
if (F) {
  my_row <- 1
  myr <- yfail$r[my_row]
  mya <- yfail$a[my_row]
  myb <- yfail$b[my_row]
  mytau <- yfail$tau[my_row]
  myk <- yfail$K[my_row]
  myc <- yfail$c[my_row]
}

if (F) {
  for (run in unique(ybig$uniq_run)) {
  #for (run in seq(from = 395, to = 399, by = 1)) {
    tiff(paste("./sim_curves/", run, ".tiff", sep = ""),
         width = 5, height = 5, units = "in", res = 300)
    print(
      ggplot(data = ybig[ybig$uniq_run == run &
                           ybig$Pop != "B",], 
             aes(x = time, y = Density+10, color = Pop)) +
              geom_line(lwd = 1.5, alpha = 1) + 
        geom_line(data = ybig[ybig$uniq_run == run &
                                ybig$Pop == "B",], 
                  aes(x = time, y = Density+10),
                  color = "black", alpha = 0.5, lwd = 1.1) +
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
        NULL
    )
    dev.off()
  }
}


#TODO:
# can dede itself handle a stop-at-equilibrium condition?
#   Yes, they're called roots. However, it's not clear whether
#     it would really make things faster or not