# Setup ----

library(gcplyr)
library(ggplot2)
library(dplyr)
library(deSolve)

#Values from old run using example_widedata
#uS 0.038
#uR 0.025
#uS2 0.0125
#uR2 0.0083
#aS 1.6e-11

# Simulate bacteria & phage growth ----
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
    parms["a_S"] * y["S"] * y["P"] -
    y["S"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"]
  
  dY["R"] <- 
    a * parms["u_R"] * y["R"] * (1-((y["S"] + y["R"])/parms["k"])**parms["v"]) -
    y["R"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"]
  
  dY["P"] <- parms["b"] * parms["a_S"] * y["P"] * (y["S"] + y["S2"])
  
  dY["S2"] <- 
    y["S"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"] +
    parms["u_S2"] * y["S2"] * (1 - ((y["S2"] + y["R2"])/parms["k2"])) -
    parms["a_S"] * y["S2"] * y["P"]
  
  dY["R2"] <- 
    y["R"] * parms["x"] * ((y["S"] + y["R"])/parms["k"])**parms["v2"] +
    parms["u_R2"] * y["R2"] * (1 - ((y["S2"] + y["R2"])/parms["k2"]))
  
  return(list(dY))
}

params <- c(u_S = 0.04, u_R = 0.02, k = 10**9, a_S = 2*10**-11, b = 20,
            q0 = 0.1, m = 0.02, v = .5,
            u_S2 = 0.016, u_R2 = 0.008, k2 = 0.2*10**9, x = 0.0005, v2 = 50)

Y_init <- c(S = 10**6, R = 1, P = 0, S2 = 0, R2 = 0)
Y_init2 <- c(S = 10**6, R = 1, P = 10, S2 = 0, R2 = 0)

times <- seq(from = 0, to = 24*60, by = 15)

out <- as.data.frame(ode(y = Y_init, times = times, func = derivs, parms = params))
out$Phage <- "No Phage"
out2 <- as.data.frame(ode(y = Y_init2, times = times, func = derivs, parms = params))
out2$Phage <- "Phage Added"
out <- rbind(out, out2)
colnames(out)[1] <- "Time"
out$B <- out$S + out$R + out$S2 + out$R2

# tidy and analyze ----

dat <- trans_wide_to_tidy(out, id_cols = c("Time", "Phage"), names_to = "Pop")
dat$Measurements[dat$Measurements < 0] <- 0
dat$Time <- dat$Time/60

ggplot(data = dat, aes(x = Time, y = Measurements, color = Pop)) +
  geom_line() +
  facet_grid(~Phage) +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.1, NA))

ggplot(data = filter(dat, Pop == "B"), 
       aes(x = Time, y = Measurements, color = Pop)) +
  geom_line() +
  facet_grid(~Phage)

dat <- select(filter(dat, Pop == "B"), !Pop)
dat$Measurements <- dat$Measurements/10**9
dat$Measurements <- round(dat$Measurements, 3)

dat <- mutate(group_by(dat, Phage),
              mov_avg = smooth_data(y = Measurements, sm_method = "moving-average",
                                    window_width_n = 5, x = Time),
              deriv = calc_deriv(y = Measurements, x = Time),
              deriv_percap = calc_deriv(y = mov_avg, x = Time,
                                        percapita = TRUE,
                                        blank = 0, trans_y = 'log',
                                        window_width_n = 7),
              deriv3 = calc_deriv(y = Measurements, x = Time,
                                  window_width_n = 3))

dat_sum <- 
  summarize(group_by(dat, Phage),
            diauxie_time = first_minima(y = deriv, x = Time,
                                        window_width = 5,
                                        return_endpoints = FALSE,
                                        return = "x"),
            first_maxima_x = first_maxima(y = Measurements, x = Time,
                                          window_width = 5,
                                          return = "x"),
            first_maxima_y = first_maxima(y = Measurements, x = Time,
                                          window_width = 5,
                                          return = "y"),
            max_percap = max(deriv_percap, na.rm = TRUE),
            init_dens = first_minima(Measurements, return = "y"),
            lag_time = lag_time(x = Time, y = Measurements,
                                deriv = deriv_percap, y0 = init_dens),
            max_percap_time = Time[which.max(deriv_percap)],
            max_percap_dens = Measurements[which.max(deriv_percap)],
            max_dens = max(Measurements, na.rm = TRUE),
            extin_time = first_below(Measurements, x = Time, threshold = 0.1,
                                     return_endpoints = FALSE, return = "x"))

#Dens & derivs ----
ggplot(dat, aes(x = Time, y = Measurements, color = Phage)) +
  geom_point()

ggplot(dat, aes(x = Time, y = deriv, color = Phage)) +
  geom_line()

ggplot(dat, aes(x = Time, y = deriv_percap, color = Phage)) +
  geom_line()

p1 <- ggplot(dat, aes(x = Time/3600, y = Measurements)) +
  geom_point(size = 0.75) +
  facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()

p2 <- ggplot(dat, aes(x = Time/3600, y = deriv)) +
  geom_line() +
  facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "Derivative (OD600/hr)") +
  coord_cartesian(ylim = c(-0.2, NA)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

p3 <- ggplot(dat, aes(x = Time/3600, y = deriv_percap)) +
  geom_line()  +
  facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "Per-capita\nDerivative (/hr)") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

png("./manuscript/dens_derivs.png", width = 4, height = 5,
    units = "in", res = 150)
cowplot::plot_grid(
  ncol = 1, rel_heights = c(1.1, 1, 1), align = "v",
  p1, p2, p3)
dev.off()

# max percap, lag, max dens ----
png("./manuscript/maxgrowth_lag_maxdens.png", width = 4, height = 4,
    units = "in", res = 150)
ggplot(data = filter(dat, Phage == "No Phage"),
       aes(x = Time, y = log(Measurements))) +
  geom_point() +
  geom_abline(data = filter(dat_sum, Phage == "No Phage"),
              color = "red",
              aes(slope = max_percap,
                  intercept = log(max_percap_dens) -
                    max_percap*max_percap_time)) +
  geom_vline(data = filter(dat_sum, Phage == "No Phage"),
             aes(xintercept = lag_time), lty = 2) +
  # geom_hline(data = filter(dat_sum, Phage == "No Phage"),
  #            aes(yintercept = log(init_dens))) +
  geom_hline(data = filter(dat_sum, Phage == "No Phage"),
             aes(yintercept = log(max_dens)), lty = 2) +
  theme_bw() +
  labs(x = "Time (hr)", y = "log(OD600)")
dev.off()

#Diauxie ----
p1 <- ggplot(filter(dat, Phage == "No Phage"), 
             aes(x = Time/3600, y = Measurements)) +
  geom_point(size = 0.75) +
  geom_vline(data = filter(dat_sum, Phage == "No Phage"),
             aes(xintercept = diauxie_time/3600), lty = 2) +
  facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()

p2 <- ggplot(filter(dat, Phage == "No Phage"), 
             aes(x = Time/3600, y = deriv)) +
  geom_line() +
  geom_vline(data = filter(dat_sum, Phage == "No Phage"),
             aes(xintercept = diauxie_time/3600), lty = 2) +
  facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "Derivative (OD600/hr)") +
  coord_cartesian(ylim = c(-0.1, NA)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

png("./manuscript/diauxie.png", width = 4, height = 5,
    units = "in", res = 150)
cowplot::plot_grid(
  ncol = 1, rel_heights = c(1.1, 1), align = "v",
  p1, p2)
dev.off()

# first maxima & extinction ----
png("./manuscript/first_maxima.png", width = 4, height = 3.5,
    units = "in", res = 150)
ggplot(filter(dat, Phage == "Phage Added"), 
             aes(x = Time/3600, y = Measurements)) +
  geom_point(size = 0.75) +
  geom_point(data = filter(dat_sum, Phage == "Phage Added"),
             aes(x = first_maxima_x/3600, y = first_maxima_y), 
             pch = 4, size = 2, color = "red", stroke = 2, alpha = 0.5) +
  geom_vline(data = filter(dat_sum, Phage == "Phage Added"),
             aes(xintercept = extin_time/3600), lty = 2) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

#Noisy data ----
set.seed(1)
datnoisy <- dat
datnoisy$Measurements <-
  round(datnoisy$Measurements +
          0.02*c(arima.sim(model = list(order = c(0, 0, 0)),
                           n = length(datnoisy$Measurements))),
        3)

# fitting during deriv ----
#Define segments
halflength <- 1
datnoisy <- mutate(group_by(datnoisy, Phage),
                   deriv3 = calc_deriv(y = Measurements, x= Time,
                                       window_width_n = 9),     
                   xstart = Time - halflength,
                   xend = Time + halflength,
                   ystart = Measurements - halflength*deriv3,
                   yend = Measurements + halflength*deriv3)

png("./manuscript/deriv_fitting.png", width = 4, height = 4,
    units = "in", res = 150)
ggplot(filter(datnoisy, Phage == "No Phage"), 
       aes(x = Time, y = Measurements)) +
  geom_point() +
  geom_segment(aes(x = xstart, xend = xend, y = ystart, yend = yend),
               color = "red", alpha = 0.5, lwd = 0.5) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

#Add more noise ----
datnoisy <- dat
set.seed(1)
datnoisy$Measurements <-
  round(datnoisy$Measurements +
          0.01*c(arima.sim(model = list(order = c(0, 0, 0)),
                           n = length(datnoisy$Measurements))) +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.8, 0.2)) *
          rexp(n = length(datnoisy$Measurements), rate = 20) +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.85, 0.15)) *
          rexp(n = length(datnoisy$Measurements), rate = 10) +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.9, 0.1)) *
          rexp(n = length(datnoisy$Measurements), rate = 5),
        3)

ggplot(datnoisy, aes(x = Time, y = Measurements, color = Phage)) +
  geom_point() +
  facet_grid(~Phage)

# smoothing ----
datnoisy <- 
  mutate(group_by(datnoisy, Phage),
         sm_med5 = smooth_data(y = Measurements, x = Time, 
                               sm_method = "moving-median",
                                window_width_n = 5),
         sm_avg5 = smooth_data(y = Measurements, x = Time,
                               sm_method = "moving-average",
                               window_width_n = 5),
         sm_loess = smooth_data(y = Measurements, x = Time,
                               sm_method = "loess",
                               span = 0.2),
         sm_gam = smooth_data(y = Measurements, x = Time,
                              sm_method = "gam", k = 20))

p1 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
       aes(x = Time/3600, y = Measurements)) +
  geom_point() +
  geom_line(aes(y = sm_med5), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving median (n = 5)")

p2 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
       aes(x = Time/3600, y = Measurements)) +
  geom_point() +
  geom_line(aes(y = sm_avg5), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving average (n = 5)")

p3 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
       aes(x = Time/3600, y = Measurements)) +
  geom_point() +
  geom_line(aes(y = sm_loess), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "loess (span = 0.2)")

p4 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
       aes(x = Time/3600, y = Measurements)) +
  geom_point() +
  geom_line(aes(y = sm_gam), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "GAM (k = 20)")

png("./manuscript/smoothing.png", width = 5, height = 5,
    units = "in", res = 150)
cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
dev.off()
