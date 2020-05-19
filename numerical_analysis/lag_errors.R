#Minimal reproduction of lag too large error
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
  
  #Save "true" y values for future reference
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
  dY[ystart < 0] <- 0
  
  #From documentation: The return value of func should be a list, whose first 
  #element is a vector containing the derivatives of y with respect to time
  return(list(dY))
}

yinit <- c(S = 10**6,
           I = 0,
           P = 10**5)

#First example
params <- c(r = .04, a = 10**-12, b = 5, tau = 10, K = 10**7)
times <- seq(0, 400, 0.003125)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

#Second example
params <- c(r = .04, a = 10**-12, b = 5, tau = 50, K = 10**8)
times <- seq(0, 204800, 25.6)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

times <- seq(0, 73190, 25.6)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))
tail(yout)
#All pops are still positive at 73141, when it says the lag is too large

#Third example
params <- c(r = .04, a = 10**-8, b = 500, tau = 100, K = 10**8)
times <- seq(0, 400, 2.5*10**-2)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

times <- seq(0, 99, 2.5*10**-2)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

#Code for plotting population sizes over time
ymelt <- reshape2::melt(data = as.data.frame(yout), 
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