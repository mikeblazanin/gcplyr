library(deSolve)
library(reshape2)
library(ggplot2)

derivs <- function(t, y, parms) {
  #The derivs function must return the derivative of all the variables at a
  # given time, in a list
  
  #Issue warning about too small/negative yvals
  if (any(y < 0)) {
    warning(paste("pop(s)",
                  paste(which(y < 0), collapse = ","),
                  "below 0, treating as 0, returning dY = 0"))
  }
  ystart <- y
  
  #Set negative y values to 0 so they don't affect the dN's
  y[y < 0] <- 0
  
  #Create output vector
  dY <- c(S = 0, I = 0, P = 0)
  
  ##Calculate dS
  
  #Old (exponential dS/dt)
  #dS/dt = rS - aSP
  #dS <- parms["r"] * y["S"] - a * y["S"] * y["P"]
  
  #New (logistic dS/dt)
  #dS/dt = rS((K-S/K)) - aSP
  dY["S"] <- parms["r"] * y["S"] * ((parms["K"] - y["S"])/parms["K"]) - 
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
  
  #Issue warning about too large pop
  if (any(y > 10**100)) {
    warning(paste("pop(s)",
                  paste(which(y > 10**100), collapse = ","),
                  "exceed max limit, 10^100, returning dY = 0"))
    dY[y > 10**100] <- 0
  }
  
  #Change dY for too small pops
  if (any(ystart < 0)) {
    dY[ystart < 0] <- 0
    
    #Issue warning about declining pop already at 0
    if (any(ystart == 0 & dY < 0)) {
      warning(paste("pop(s)",
                    paste(which(y == 0 & dY < 0), collapse = ","),
                    "at 0 and declining, returning dY = 0"))
      dY[y == 0 & dY < 0] <- 0
    }
  }
  
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

rseq <- 4*10**seq(from = -2, to = -3, by = -0.5)
kseq <- 10**seq(from = 7, to = 9, by = 1)
aseq <- 10**seq(from = -12, to = -8, by = 1)
tauseq <- c(10, 50, 100)
bseq <- c(5, 50, 500)
init_dens_seq <- c(10**6)
init_moi_seq <- c(10**-2)

i <- 1
for (myr in rseq) {
  for (myk in kseq) {
    for (mya in aseq) {
      for (mytau in tauseq) {
        for (myb in bseq) {
          for (my_init_bact in init_dens_seq) {
            for (my_moi in init_moi_seq) {
              #Define pops & parameters
              yinit <- c(S = my_init_bact,
                         I = 0,
                         P = my_init_bact*my_moi)
              times = seq(0, 1000, .1)
              params <- c(r = myr, a = mya, b = myb, tau = mytau,
                          K = myk)
              
              #Run simulation(s)
              yout <- as.data.frame(
                dede(y = yinit, times = times, func = derivs, parms = params))
              
              #Calculate all bacteria (B)
              yout$B <- yout$S + yout$I
              
              #Reshape and combine with other outputs
              ymelt <- reshape2::melt(data = as.data.frame(yout), id = c("time"),
                                      value.name = "Density", variable.name = "Pop")
              ymelt <- cbind(data.frame(uniq_run = i, r = myr, a = mya, 
                                        b = myb, tau = mytau, K = myk, 
                                        init_dens = my_init_bact, 
                                        init_moi = my_moi),
                             ymelt)
              if (i == 1) {ybig <- ymelt
              } else {
                ybig <- rbind(ybig, ymelt)
              }
              
              i <- i+1
            }
          }
        }
      }
    }
  }
}

ybig$uniq_run <- match(paste(ybig$r, ybig$a, ybig$b, ybig$tau, ybig$K,
                             ybig$init_dens, ybig$init_moi),
                       unique(paste(ybig$r, ybig$a, ybig$b, ybig$tau, ybig$K,
                                    ybig$init_dens, ybig$init_moi)))

ggplot(data = ybig[ybig$uniq_run == 9 &
                     ybig$Pop != "B",], 
       aes(x = time, y = Density+1, color = Pop)) +
        geom_line(lwd = 1.5, alpha = 1) + 
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = seq(from = 0, to = max(ybig$time), by = 100)) +
  geom_hline(yintercept = 1, lty = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  NULL
