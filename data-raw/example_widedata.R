#This is the code to generate example_widedata

#Cases included:
# lag time before max percap
# max percap (for max generally)
# max dens in absence of phages
# max dens in presence of phages w/ evol of resis for first_peak
# noise & rounding to realistic values

library(deSolve)
library(ggplot2)
library(gcplyr)

#Define function that returns derivative for simulating pop densities
derivs <- function(t, y, parms) {
  #dS/dt = u_S * S * a(t) * (1 - ((S+R)/k)^v) - a_S * S * P
  
  #S2 is the diauxic-shifted S population growing on the implicit 2nd resource
  # which increases from transitions from S and growth on 2nd res
  #dS2/dt = S * x * ((S+R)/k)^v2 
  #         + u_S2 * S2 * (1 - ((S2 + R2)/k2)) 
  #         - a_S * S2 * P
  
  #dR/dt = u_R * R * a(t) * (1 - ((S+R)/k)^v)
  
  #R2 is the diauxic-shifted R population growing on the implicit 2nd resource
  # which increases from transitions from R and growth on 2nd res
  #dR2/dt = R * x * ((S+R)/k)^v2 
  #         + u_R2 * R2 * (1 - ((S2 + R2)/k2))
  
  #dP/dt = b * a_S * S * P
  
  #Note: a(t) = q0/(q0 + e^(-m*t))
  
  #parms: u_S, u_R, k, a_S, b, q0, m, v, u_S2, u_R2, k2, x, v2
  #y: S, R, P, S2, R2
  
  dY <- c(S = 0, R = 0, P = 0, S2 = 0, R2 = 0)
  
  #For relationship between q0 and a at t0 see:
  # qvals <- seq(from = 0.1, to = 10, by = 0.1)
  # plot(x = qvals, y = qvals/(qvals + 1))
  a <- parms["q0"]/(parms["q0"] + exp(-parms["m"] * t))
  
  #For relationship between (N/k)^v and v see:
  # vvals <- c(0.1, 0.5, 1, 2, 10)
  # nkvals <- seq(from = 0, to = 1, by = 0.1)
  # for(v in vvals) {
  #   print(plot(x = nkvals, y = nkvals**v, main = paste("v =", v)))}
  
  dY["S"] <- 
    a * parms["u_S"] * y["S"] * (1-((y["S"] + y["R"])/parms["k"])**parms["v"]) -
    parms["a_S"] * y["S"] * y["P"]
  
  dY["R"] <- 
    a * parms["u_R"] * y["R"] * (1-((y["S"] + y["R"])/parms["k"])**parms["v"])
  
  dY["P"] <- parms["b"] * parms["a_S"] * y["S"] * y["P"]
  
  dY["S2"] <- 
    y["S"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"] +
    parms["u_S2"] * y["S2"] * (1 - ((y["S2"] + y["R2"])/parms["k2"])) -
    parms["a_S"] * y["S2"] * y["P"]
  
  dY["R2"] <- 
    y["R"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"] +
    parms["u_R2"] * y["R2"] * (1 - ((y["S2"] + y["R2"])/parms["k2"]))
  
  return(list(dY))
}

#Demonstrate simple example of how it works

#Rates in ~/minute, densities in ~/mL
params <- c(u_S = 0.03, u_R = 0.02, k = 10**9, a_S = 5*10**-11, b = 20,
            q0 = 0.1, m = 0.02, v = .5,
            u_S2 = 0.01, u_R2 = 0.0066, k2 = 0.2*10**9, x = 0.0001, v2 = 50)

Y_init <- c(S = 10**6, R = 1, P = 0, S2 = 0, R2 = 0)

times <- seq(from = 0, to = 24*60, by = 15)

out <- as.data.frame(ode(y = Y_init, times = times, func = derivs, parms = params))
out$B <- out$S + out$R + out$S2 + out$R2

out_tdy <- tidyr::pivot_longer(out, cols = -time,
                               names_to = "pop", values_to = "dens")
out_tdy$dens[out_tdy$dens <= 1] <- 1
out_tdy$deriv <- gcplyr::calc_deriv(y = out_tdy$dens,
                                    x = out_tdy$time,
                                    subset_by = out_tdy$pop)
out_tdy$percap <- gcplyr::calc_deriv(y = out_tdy$dens,
                                     x = out_tdy$time,
                                     subset_by = out_tdy$pop,
                                     percap = TRUE, blank = 0)

ggplot(data = out_tdy,
       aes(x = time/60, y = dens, color = pop)) +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  NULL

ggplot(data = out_tdy[out_tdy$pop == "B", ],
       aes(x = time/60, y = dens, color = pop)) +
  geom_line() +
  #scale_y_continuous(trans = "log10") +
  NULL

ggplot(data = out_tdy,
       aes(x = time/60, y = deriv, color = pop)) +
  geom_line()

ggplot(data = out_tdy[out_tdy$pop == "B", ],
       aes(x = time/60, y = deriv, color = pop)) +
  geom_line()

ggplot(data = out_tdy,
       aes(x = time/60, y = percap, color = pop)) +
  geom_line()

ggplot(data = out_tdy[out_tdy$pop == "B", ],
       aes(x = time/60, y = percap, color = pop)) +
  geom_line()


#Generate entire plate of simulated data
example_widedata <- as.data.frame(matrix(NA, nrow = 24*4+1, ncol = 97))
colnames(example_widedata) <- c("Time", 
                                paste(
                                  rep(gcplyr::to_excel(1:8), 12),
                                  rep(1:12, each = 8), sep = ""))
#Time will be saved in seconds, even though simulations are run in
# units of minutes
example_widedata$Time <- seq(from = 0, to = 24*60*60,
                             by = 15*60)

#Make copy with no noise
example_widedata_noiseless <- example_widedata

#Set up df for saving all raw data
times <- seq(from = 0, to = 24*60, by = 15)
all_data <- data.frame(matrix(nrow = 96*length(times), ncol = 27))

#Generate vectors of bacterial growth parameters
set.seed(123)
uS_vector <- rep(runif(48, min = 0.01, 0.05), 2)
uR_vector <- uS_vector * 0.66
k_vector <- rep(10**9, 96)
q0_vector <- rep(0.1, 96)
m_vector <- rep(0.02, 96)
v_vector <- rep(.5, 96)
uS2_vector <- uS_vector/3
uR2_vector <- uR_vector/3
k2_vector <- 0.2*k_vector
x_vector <- rep(0.0001, 96)
v2_vector <- rep(50, 96)
#Generate vectors of parameters for viral growth
aS_vector <- c(rep(0, 48), runif(48, 0.1, 1)*5*10**-11)
b_vector <- c(rep(0, 48), rep(20, 48))
#Generate vectors of initial densities
Sdens_init_vector <- rep(10**6, 96)
Rdens_init_vector <- rep(1, 96)
Pdens_init_vector <- c(rep(0, 48), rep(10, 48))

#Create design for layout of plate
example_design <- make_design(
  wellnames_sep = "",
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 1:6,
    pattern = 1:48,
    byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 7:12,
    pattern = 1:48,
    byrow = TRUE),
  "Phage" = make_designpattern(
    values = c("No Phage"),
    rows = 1:8, cols = 1:6,
    pattern = "1"),
  "Phage" = make_designpattern(
    values = c("Phage Added"),
    rows = 1:8, cols = 7:12,
    pattern = "1"))


#Calculate growth (bacteria alone in first 48 wells, bact + phage in next 48)
for (i in 1:96) {
  #Set up parameters
  params <- c(u_S = uS_vector[i], u_R = uR_vector[i], 
              k = k_vector[i], a_S = aS_vector[i], b = b_vector[i],
              q0 = q0_vector[i], m = m_vector[i], v = v_vector[i],
              u_S2 = uS2_vector[i], u_R2 = uR2_vector[i], k2 = k2_vector[i],
              x = x_vector[i], v2 = v2_vector[i])
  
  Y_init <- c(S = Sdens_init_vector[i], 
              R = Rdens_init_vector[i], 
              P = Pdens_init_vector[i],
              S2 = 0, R2 = 0)
  
  #Run simulation
  out <- as.data.frame(ode(y = Y_init, times = times, func = derivs, parms = params))
  out$B <- out$S + out$R + out$S2 + out$R2
  
  #Convert to OD
  out$OD <- out$B/10**9
  
  #Add noise
  out$OD_noised <- out$OD + 
    runif(n = nrow(out), min = 0, max = 0.002) +
    sample(x = c(0, 1), nrow(out), replace = TRUE, prob = c(0.9, 0.1)) *
    rexp(n = nrow(out), rate = 40)
  out$OD <- out$OD + 0.001
  
  #plot(out$time, log10(out$OD))
  #lines(out$time, log10(out$OD_noised))
  
  #Round
  out$OD <- round(out$OD, 3)
  out$OD_noised <- round(out$OD_noised, 3)
  
  example_widedata_noiseless[, i+1] <- out$OD
  example_widedata[, i+1] <- out$OD_noised
  all_data[((i-1)*length(times)+1):(i*length(times)), 1:9] <- out
  all_data[((i-1)*length(times)+1):(i*length(times)), 10:22] <-
    data.frame(matrix(params, nrow = 1, dimnames = list(NULL, names(params))))
  all_data[((i-1)*length(times)+1):(i*length(times)), 23:27] <-
    data.frame(
      matrix(Y_init, nrow = 1, 
             dimnames = list(NULL, paste(names(Y_init), "_init", sep = ""))))
      
      
      params <- c(u_S = uS_vector[i], u_R = uR_vector[i], 
                  k = k_vector[i], a_S = aS_vector[i], b = b_vector[i],
                  q0 = q0_vector[i], m = m_vector[i], v = v_vector[i],
                  u_S2 = uS2_vector[i], u_R2 = uR2_vector[i], k2 = k2_vector[i],
                  x = x_vector[i], v2 = v2_vector[i])
      
      Y_init <- c(S = Sdens_init_vector[i], 
                  R = Rdens_init_vector[i], 
                  P = Pdens_init_vector[i],
                  S2 = 0, R2 = 0)
}
colnames(all_data) <-
  c(colnames(out), names(params), paste(names(Y_init), "_init", sep = ""))

if(F) {
  #This code visualizes differences between the newly generated example_widedata
  #and the example_widedata from the currently-installed version of gcplyr
  old_widedata <- gcplyr::example_widedata
  par(mfrow = c(2, 2))
  for(col in colnames(example_widedata)[2:ncol(example_widedata)]) {
    plot(example_widedata[, col], type = "l", main = col, col = "red")
    lines(old_widedata[, col])
  }
}

#Code to visualize example data
ex_lng <- trans_wide_to_tidy(example_widedata, id_cols = "Time")
ex_lng <- merge_dfs(example_design, ex_lng)

if(F) {
  png("example_wide.png", width = 10, height = 10, units = "in", res = 150)
  ggplot(ex_lng, aes(x = Time, y = Measurements, color = Bacteria_strain)) +
    geom_line(aes(lty = Phage)) +
    guides(color = "none", lty = "none") +
    facet_wrap(~Bacteria_strain) +
    #scale_y_continuous(trans = "log10") +
    NULL
  dev.off()
  
  png("example_wide_log.png", width = 10, height = 10, units = "in", res = 150)
  ggplot(ex_lng, aes(x = Time, y = Measurements, color = Bacteria_strain)) +
    geom_line(aes(lty = Phage)) +
    guides(color = "none", lty = "none") +
    facet_wrap(~Bacteria_strain) +
    scale_y_continuous(trans = "log10") +
    NULL
  dev.off()
}

#Save
usethis::use_data(example_widedata, overwrite = TRUE)
usethis::use_data(example_widedata_noiseless, overwrite = TRUE)

#Also save the raw data
write.csv(all_data, file = "./data-raw/all_data.csv",
          row.names = FALSE)
