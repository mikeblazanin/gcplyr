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
#   ANSWER: Based on what we saw on Tuesday, I’d say that the parameter "a" could change
#           this plot because the three populations depend on it. Plus, "a" is the probability
#           by which the phage successfully infects the bacteria, which means that, if we 
#           increase this parameter, S population will plateau or increase at some point.
#           
#           Then, we have "K", which is the carrying capacity (maximum population size at
#           which the population can sustain itself). Therefore, this parameter will also
#           change the shape of the plot. The curve will be able to grow more if we increase 
#           K, but S population will shrink if we decrease K’s value.
#
#           Also, if we want to change the shape of the susceptible population curve, we could
#           play with "c" (competition coefficient). It measures the strength of competition that
#           does a different specie or, in this case, the infected population, over the susceptible
#           population. Therefore, if "c" is high, the curve will shrink and vice versa.
#    
#           I also think that "r" has something to do with the curve shape. However, I think that
#           "r" depends on the carrying capacity. What I mean is that, if we are in "K" (meaning c
#           = maximum (1?)), "r" will be zero, because competition will be in its maximum, and if 
#           we have no competition (meaning c = 0), "r" will increase. So, the shape of the cure 
#           doesn’t depend on a direct way of "r"’s value. Thus, the value that "r" has is a 
#           consequence of the value that "c" has.
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
#   I was wrong!! The value of "r" does change the shape of the curve! If it
#   increases, the curve reaches plateau sooner. 
#
#   I was wrong again, "a" doesn’t affect the shape of the curve!!
#   
#   TO SUM UP, WE ONLY SEE THE PLOT CHANGING WHEN WE VARY THE FOLLOWING
#   PARAMETERS: K, and r.
#
#   Reflection: I think this is because of the conditions we have here.
#   Since P and I = 0, all the parameters that are multiplying them become 0,
#   therefore, they can’t affect the shape of the curve. And, since K and r
#   aren’t multiplying P and I at any point, they don’t become 0, thus are
#   able to change the shape of the cure with the fluctuation of their value.
#   I suppose that if we had different conditions, where P and I weren’t 0,
#   other parameters would make the graph look different.
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
#       Observations: As we can see, each population has its own starting
#       concentration. We can observe that the infected one increases a lot
#       the first 5 to 8 minutes, and by the 10th it slows down a little bit,
#       even though it's still increasing.
#
#       Phage population doesn’t really grow by time with these conditions,
#       but it maintains its initial concentration, pretty much.
#
#       By these two observations, we can say that phages successfully infect
#       susceptible bacteria and create new phages to maintain their population.
#       However, susceptible population’s rate of growth is higher, because, even
#       though they are infected at some point, they continue to grow and proliferate,
#       more than the phage population.
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
#   ANAWER: None of them went extinct with a duration of 50 (hours?). I tried a
#   longer duration: 250 (hours?) and saw that only the S population went extinct
#   at a time point around 200 (hours?). Then, I tried much longer durations but with
#   none of them I saw I population go extinct!! I was expecting it to go extinct
#   since I thought that with no S population, I oppulation can't be created, thus
#   it's be impossible for them tu survive (if we don't think about resistence to 
#   phages, or sometinh like this).
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
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results ----
head(yout)
tidyr::pivot_longer(yout, c(S, I, P), names_to = "Population", values_to = "Density")
# Fisrt, I tried this command above, but R said that Population and Density didn't exist
# when trying to plot. That's why I tried what's below and it worked.

library(tidyr)
yout_plot <- pivot_longer(yout, c(S, I, P), names_to = "Population", values_to = "Density")


##We've reshaped the data. Now, we'll plot the density of the three populations over time
ggplot(data = yout_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "r"
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.5, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout2 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "r"
library(tidyr)
yout2_plot <- pivot_longer(yout2, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout2_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "S"
yinit <- c(S = 10**2,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout3 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "S"
library(tidyr)
yout3_plot <- pivot_longer(yout3, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout3_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "b"
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 30, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout4 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "b"
library(tidyr)
yout4_plot <- pivot_longer(yout4, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout4_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**2, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout5 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "a"
library(tidyr)
yout5_plot <- pivot_longer(yout5, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout5_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "tau"
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = -50,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout6 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "tau"
library(tidyr)
yout6_plot <- pivot_longer(yout6, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout6_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "K"
yinit <- c(S = 10**6,
           I = 0,
           P = 0)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**3,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout7 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results changing "tau"
library(tidyr)
yout7_plot <- pivot_longer(yout7, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout7_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "P"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 50, by = 1)
yout8 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "P"
library(tidyr)
yout8_plot <- pivot_longer(yout8, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout8_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation with a LONGER DURATION (250)
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout_time <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results with a LONGER DURATION (250)
library(tidyr)
yout_time_plot <- pivot_longer(yout_time, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout_time_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation with a LONGER DURATION (5000)
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 5000, by = 1)
yout_time1 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results with a LONGER DURATION (5000)
library(tidyr)
yout_time1_plot <- pivot_longer(yout_time1, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout_time1_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "r"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.5, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout9 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "r"
library(tidyr)
yout9_plot <- pivot_longer(yout9, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout9_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-15, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout10 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "a"
library(tidyr)
yout10_plot <- pivot_longer(yout10, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout10_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "r" and "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 5, 
            a = 10**-15, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout11 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "r" and "a"
library(tidyr)
yout11_plot <- pivot_longer(yout11, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout11_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "b"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 100, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout12 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "b"
library(tidyr)
yout12_plot <- pivot_longer(yout12, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout12_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "b", "r", and "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 5, 
            a = 10**-15, 
            b = 100, 
            tau = 10,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout13 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "b", "r", and "a"
library(tidyr)
yout13_plot <- pivot_longer(yout13, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout13_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "tau"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 50,
            K = 10**9,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 500, by = 1)
yout14 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "tau"
library(tidyr)
yout14_plot <- pivot_longer(yout14, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout14_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "K"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**-2,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout15 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "K"
library(tidyr)
yout15_plot <- pivot_longer(yout15, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout15_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "K"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**12,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout17 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "K"
library(tidyr)
yout17_plot <- pivot_longer(yout17, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout17_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "K", "a", and "r"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.5, 
            a = 10**-15, 
            b = 50, 
            tau = 10,
            K = 10**12,
            c = 1,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 2000, by = 1)
yout16 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "K", "a", and "r"
library(tidyr)
yout16_plot <- pivot_longer(yout16, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout16_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "c"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.04, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 2,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout18 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "K"
library(tidyr)
yout18_plot <- pivot_longer(yout18, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout18_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "c", "K", "r", and "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.5, 
            a = 10**-15, 
            b = 50, 
            tau = 10,
            K = 10**12,
            c = 0.5,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 3000, by = 1)
yout19 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "c"
library(tidyr)
yout19_plot <- pivot_longer(yout19, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout19_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "c", "K", "r", and "a"
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.5, 
            a = 10**-15, 
            b = 50, 
            tau = 10,
            K = 10**12,
            c = 0,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 3000, by = 1)
yout20 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "c", "K", "r", and "a"
library(tidyr)
yout20_plot <- pivot_longer(yout20, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout20_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")

##Run simulation changing "c", and "r",
yinit <- c(S = 10**6,
           I = 0,
           P = 10**4)
params <- c(r = 0.5, 
            a = 10**-10, 
            b = 50, 
            tau = 10,
            K = 10**9,
            c = 0.5,
            warnings = 0, 
            thresh_min_dens = 10**-100)
times <- seq(from = 0, to = 250, by = 1)
yout21 <- as.data.frame(
  dede(y = yinit, times = times, func = derivs, parms = params))

##Plot results  changing "c", and "r"
library(tidyr)
yout21_plot <- pivot_longer(yout21, c(S, I, P), names_to = "Population", values_to = "Density")

ggplot(data = yout21_plot, aes(x = time, y = Density, color = Population)) +
  geom_line(lwd = 1.5) +
  scale_y_continuous(trans = "log10")
