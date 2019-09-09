library(deSolve)
library(reshape2)
library(ggplot2)

derivs <- function(t, y, parms) {
  #The derivs function must return the derivative of all the variables at a
  # given time, in a list
  
  #with makes the named elements of a list available by simply calling the name
  # (as if all the names were variables)
  #It requires two arguments: a named list, and an expression to evaluate
  #And will evaluate the expression in the environment where the names of the
  # list are available as variables
  with(as.list(c(y, parms)),
       { #dS/dt = rS - aSP
         if (t < tau) {dS <- 0
         } else {dS <- r*y[1] - a*y[1]*y[3]}
         #dI/dt = aSP - aS(t-tau)P(t-tau)
         if (t < tau) {dI <- 0
         } else {dI <- a*y[1]*y[3] - a*lagvalue(t-tau, 1)*lagvalue(t-tau, 3)}
         #dP/dt = baS(t-tau)P(t-tau) - aSP
         if (t < tau) {dP <- 0
         } else {dP <- b*a*lagvalue(t-tau, 1)*lagvalue(t-tau, 3) - a*y[1]*y[3]}
         
         return(list(c(dS, dI, dP)))
       }
      )
}

myseq <- 10**seq(from = -3, to = 4, by = 1)
for (i in 1:length(myseq)) {
  init_dens <- myseq[i]
  yinit <- c(S = init_dens, I = 0, P = init_dens)
  times <- seq(0, 1000, 0.1)
  params <- c(r = 0.1, a = 0.01, b = 3, tau = 30)
  
  yout <- dede(y = yinit, times = times, func = derivs, parms = params)
  yout <- as.data.frame(yout)
  yout$B <- yout$S + yout$I
  
  ymelt <- reshape2::melt(data = as.data.frame(yout), id = c("time"),
                          value.name = "Density", variable.name = "Pop")
  ymelt$init_dens <- init_dens
  if (i == 1) {ybig <- ymelt
  } else {ybig <- rbind(ybig, ymelt)}
}

ggplot(data = ybig[ybig$init_dens == 0.1,], 
       aes(x = time, y = Density+1, color = Pop)) +
        geom_line() + facet_wrap(~init_dens, scales = "free_y") +
  scale_y_continuous(trans = "log10")

