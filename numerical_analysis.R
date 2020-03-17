library(deSolve)
library(reshape2)
library(ggplot2)

##TODO: why are phage declining when negative?

derivs <- function(t, y, parms) {
  #The derivs function must return the derivative of all the variables at a
  # given time, in a list
  
  #Set negative y values to 0 so they don't affect the dN's
  y[y < 0] <- 0
  
  #with makes the named elements of a list available by simply calling the name
  # (as if all the names were variables)
  #It requires two arguments: a named list, and an expression to evaluate
  #And will evaluate the expression in the environment where the names of the
  # list are available as variables
  with(as.list(c(y, parms)),
       { 
         #For troubleshooting
         # print(paste("t=", t, sep = ""))
         # print(paste("t-tau=", t-tau, sep = ""))
         
         #Old (exponential dS/dt)
         #dS/dt = rS - aSP
         # if (t < tau) {dS <- 0
         # } else {
         dS <- r*y[1] - a*y[1]*y[3]
         
         #New (logistic dS/dt)
         #dS/dt = rS((K-S/K)) - aSP
         dS <- r*y[1]*((K-y[1])/K) - a*y[1]*y[3]
         
         #dI/dt = aSP - aS(t-tau)P(t-tau)
         if (t < tau) {dI <- a*y[1]*y[3]
         } else {dI <- a*y[1]*y[3] - a*lagvalue(t-tau, 1)*lagvalue(t-tau, 3)}
         #dP/dt = baS(t-tau)P(t-tau) - aSP
         if (t < tau) {dP <- - a*y[1]*y[3]
         } else {dP <- b*a*lagvalue(t-tau, 1)*lagvalue(t-tau, 3) - a*y[1]*y[3]}
         
         dN_vals <- list(c(dS, dI, dP))
         
         #Issue warnings about too small/too large yvals
         if (any(y < 0)) {
           warning(paste("pop(s)",
                         paste(which(y < 0), collapse = ","),
                         "below 0, returning dN = 0"))
         }
         dN_vals[y < 0] <- 0
         
         if (any(y > 10**100)) {
           warning(paste("pop(s)",
                         paste(which(y > 10**100), collapse = ","),
                         "exceed max limit, 10^100, returning dN = 0"))
         }
         dN_vals[y > 10**100] <- 0
         
         return(dN_vals)
       }
  )
}

#myseq <- 10**seq(from = -3, to = 4, by = 1)
myseq <- c(10000)
for (i in 1:length(myseq)) {
  init_dens <- myseq[i]
  yinit <- c(S = init_dens, I = 0, P = init_dens/10)
  times <- seq(0, 1000, 1)
  # ln(1/2) = -r * doub_time
  params <- c(r = 0.025, a = 0.01, b = 3, tau = 60, K = 10**9)
  
  yout <- dede(y = yinit, times = times, func = derivs, parms = params)
  yout <- as.data.frame(yout)
  yout$B <- yout$S + yout$I
  
  ymelt <- reshape2::melt(data = as.data.frame(yout), id = c("time"),
                          value.name = "Density", variable.name = "Pop")
  ymelt$init_dens <- init_dens
  if (i == 1) {ybig <- ymelt
  } else {ybig <- rbind(ybig, ymelt)}
}

ggplot(data = ybig[ybig$init_dens == 10000,], 
       aes(x = time, y = Density+1, color = Pop)) +
        geom_line(lwd = 1.5, alpha = 1) + 
  facet_wrap(~init_dens, scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(breaks = seq(from = 0, to = max(ybig$time), by = 30)) +
  NULL

