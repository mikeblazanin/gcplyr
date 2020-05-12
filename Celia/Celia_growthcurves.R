##Celia TODO List
#1. Read code I've put below in derivs. This code should be sufficient to
#   figure out what the model we're using is. Diagram and write out the
#   equations for the model we're simulating. 
#   For the diagram make sure all arrows are labeled.
#   For any symbols you use, make sure to define what the symbol is.
#   (basically I'd like you to do what you did for Problems 2.5 & 3.12
#    but for this model)
#   As some hints:
#   The model includes the following populations:
#     susceptible
#     infected
#     phage
#   The model includes the following symbols:
#     r
#     K
#     c
#     a
#     tau
#     b
#     
#2. Run the simulation with the following conditions:
#       Starting bacterial density = 10**6
#       Starting infected density = 0
#       Starting phage density = 0
#       r = 0.04
#       b = 50
#       a = 10**-10
#       tau = 10
#       c = 1
#       K = 10**9  
#     (hint: I've given you a skeleton code that you can just fill-in)   
#   
#3. Look at how yout is formatted. Use tidyr::pivot_longer to reshape it
#   and ggplot to plot the density of the 3 populations over time
#     hint: the population sizes are quite different, so you'll probably have
#     to use a log10 y-axis to view them. That can be achieved by:
#     + scale_y_continuous(trans = "log10")
#   
#4. Based on the model equations we talked about Tuesday, which parameters
#   do you expect would change this curve?   
#   
#5. Play around with all the parameters and re-run the simulation a few times, 
#   plotting the results each time (you can vary r,b,a,tau,c,K and the starting 
#   bacterial density, but for now keep the starting infected density & 
#   starting phage density at 0). Which parameters affect the density curve?
#   How does each one affect it?
#     note: if you want to simultaneously save the results of multiplt
#     different parameter runs, simply change where you save the results 
#     of dede() to
#     e.g.  yout2 <- as.data.frame(dede(...
#           yout3 <- as.data.frame(dede(...
#   
#6. Now run the simulation with the following conditions:
#       Starting bacterial density = 10**6
#       Starting infected density = 0
#       Starting phage density = 10**4
#       r = 0.04
#       b = 50
#       a = 10**-10
#       tau = 10
#       c = 1
#       K = 10**9
#
#7. Did both the susceptible and infected populations go extinct?
#   If not, increase the duration of the simulation until they both do go extinct
#   and plot the densities over time.
#   Why do we expect the susceptible and infected populations to always
#   go extinct with this model?
#   (Note that "extinct" can just mean the densities fall below some level,
#   like 10**-5, since the model mathematically allows infinitely small 
#   population sizes)
#
#8. Play around with all the parameters (easiest to leave the starting densities
#   the same for now), plotting the results each time. Are there any
#   parameter combinations that do something unexpected? How do the parameters
#   affect the curve?


## Import libraries ----

library(deSolve)
library(tidyr)
library(ggplot2)

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

##Run simulation ----
yinit <- c(S = fill_here,
           I = fill_here,
           P = fill_here)
params <- c(r = fill_here, 
            a = fill_here, 
            b = fill_here, 
            tau = fill_here,
            K = fill_here,
            c = fill_here,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results ----
