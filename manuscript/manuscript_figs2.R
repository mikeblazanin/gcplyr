# Setup ----

library(gcplyr)
library(ggplot2)
library(dplyr)
library(deSolve)

# Create sim data for diauxie ----

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

params <- c(u_S = 0.04, u_R = 0.02, k = 0.7*10**9, a_S = 2*10**-11, b = 20,
            q0 = 0.1, m = 0.01, v = .5,
            u_S2 = 0.016, u_R2 = 0.008, k2 = 0.15*10**9, x = 0.0005, v2 = 50)

Y_init <- c(S = 10**8, R = 1, P = 0, S2 = 0, R2 = 0)
times <- seq(from = 0, to = 24*60, by = 15)

out <- as.data.frame(ode(y = Y_init, times = times, func = derivs, parms = params))
colnames(out)[1] <- "Time"
out$B <- out$S + out$R + out$S2 + out$R2
dat_sim <- trans_wide_to_tidy(out, id_cols = "Time", names_to = "Pop")
dat_sim <- select(filter(dat_sim, Pop == "B"), !Pop)
dat_sim <- 
  mutate(dat_sim, 
         Measurements = round(ifelse(Measurements < 0, 0, Measurements)/10**9, 3),
         Time = Time/60,
         type = "Simulated", Well = "Z1", init_moi = "0")

# Load experimental data ----
dat <-
  read_wides(
    files = "./manuscript/2021-10-15_Emma_Growth_Curve.csv",
    startrow = 29, startcol = "B")

dat <- trans_wide_to_tidy(dat, id_cols = c("file", "Time", "T 600"))
dat <- mutate(dat,
              Time = lubridate::time_length(lubridate::hms(Time), unit = "hour"),
              type = "Experimental")

design_diftconcs <- 
  make_design(
    nrows = 8, ncols = 12,
    output_format = "tidy",
    init_bact = make_designpattern(c(10**5, 5*10**4, 10**4, 0),
                                   rows = 2:7, cols = 2:4,
                                   pattern = "444111222333222222",
                                   byrow = TRUE),
    init_bact = make_designpattern(c(10**5, 10**4),
                                   rows = 2:6, cols = 5:7,
                                   pattern = "111111111222222",
                                   byrow = TRUE),
    init_moi = make_designpattern(c(0, 0.1, 0.01),
                                  rows = 2:7, cols = 2:4,
                                  pattern = "111111111111222333",
                                  byrow = TRUE),
    init_moi = make_designpattern(c(0.1, 0.01, 0.001),
                                  rows = 2:6, cols = 5:7,
                                  pattern = "111222333111222",
                                  byrow = TRUE),
    #Row 7 cols 5:7 is actually empty but df rows will be dropped anyway
    bacteria = make_designpattern("PF",
                                  rows = 2:7, cols = 2:7,
                                  pattern = "1")
  )

dat <- merge_dfs(dat, design_diftconcs)
dat <- dplyr::filter(dat, init_bact != "NA", init_moi != "NA", bacteria != "NA")

ggplot(data = dat, aes(x = Time, y = Measurements)) +
  geom_line(aes(group = Well, color = init_moi)) +
  facet_wrap(~ init_bact)

#subtract blank
dat$Measurements <- dat$Measurements - 
  min(dplyr::filter(dat, init_bact == 0)$Measurements, na.rm = TRUE)

dat <- dplyr::filter(dat, init_bact == 10**5, init_moi %in% c(0, 0.01))

ggplot(data = dat, aes(x = Time, y = Measurements)) +
  geom_point() +
  facet_wrap(~ Well, scales = "free")

dat <- dplyr::filter(dat, Well %in% c("C2", "C7"))
dat <- select(dat, Time, Well, Measurements, init_moi, type)

dat <- merge_dfs(dat, dat_sim)

dat <- mutate(group_by(dat, Well, init_moi),
              Phage = ifelse(init_moi == 0, "No Phage", "Phage Added"),
              deriv = calc_deriv(y = Measurements, x = Time, 
                                 window_width_n = 5),
              deriv_percap = calc_deriv(y = Measurements, x = Time,
                                        percapita = TRUE,
                                        blank = 0, trans_y = 'log',
                                        window_width_n = 5))

dat_sum <- 
  summarize(group_by(dat, Well, init_moi, type, Phage),
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
                                     return_endpoints = FALSE, return = "x"),
            max_percap_time1 = Time[which.max(deriv_percap)-2],
            max_percap_time2 = Time[which.max(deriv_percap)+2])


#Dens & derivs ----
ggplot(dat, aes(x = Time, y = Measurements, color = Well)) +
  geom_point()

ggplot(dat, aes(x = Time, y = deriv, color = Well)) +
  geom_line()

ggplot(dat, aes(x = Time, y = deriv_percap, color = Well)) +
  geom_line()

p1 <- ggplot(filter(dat, Phage == "No Phage", type == "Experimental"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.75) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw()

p2 <- ggplot(filter(dat, Phage == "No Phage", type == "Experimental"),
             aes(x = Time, y = deriv)) +
  geom_line() +
  labs(x = "Time (hr)", y = "Derivative (OD600/hr)") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

p3 <- ggplot(filter(dat, Phage == "No Phage", type == "Experimental"),
             aes(x = Time, y = deriv_percap)) +
  geom_line()  +
  labs(x = "Time (hr)", y = "Per-capita\nDerivative (/hr)") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

png("./manuscript/dens_derivs.png", width = 3, height = 5,
    units = "in", res = 150)
cowplot::plot_grid(
  ncol = 1, rel_heights = c(1.1, 1, 1), align = "v",
  p1, p2, p3)
dev.off()


# max percap, lag, max dens, diauxie ----

# max percap, lag, max dens
temp_sum <- filter(dat_sum, Phage == "No Phage", type == "Experimental")
temp_dat <- filter(dat, Phage == "No Phage", type == "Experimental")
p1 <- ggplot(data = temp_dat,
       aes(x = Time, y = log(Measurements))) +
  geom_point() +
  geom_point(data = filter(temp_dat, Time >= temp_sum$max_percap_time1,
                           Time <= temp_sum$max_percap_time2),
             color = "red") +
  geom_abline(data = temp_sum,
              color = "red",
              aes(slope = max_percap,
                  intercept = log(max_percap_dens) -
                    max_percap*max_percap_time)) +
  geom_vline(data = temp_sum,
             aes(xintercept = lag_time), lty = 2) +
  # geom_hline(data = temp_sum,
  #            aes(yintercept = log(init_dens))) +
  geom_hline(data = temp_sum,
             aes(yintercept = log(max_dens)), lty = 2) +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  labs(x = "Time (hr)", y = "log(OD600)")


#Diauxie
temp_sum <- filter(dat_sum, Phage == "No Phage", type == "Simulated")
temp_dat <- filter(dat, Phage == "No Phage", type == "Simulated")
p2 <- ggplot(temp_dat, 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.75) +
  geom_vline(data = temp_sum,
             aes(xintercept = diauxie_time), lty = 2) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw()

p3 <- ggplot(temp_dat, 
             aes(x = Time, y = deriv)) +
  geom_line() +
  geom_vline(data = temp_sum,
             aes(xintercept = diauxie_time), lty = 2) +
  labs(x = "Time (hr)", y = "Derivative (OD600/hr)") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

png("./manuscript/bact_growth.png", width = 6, height = 4,
    units = "in", res = 150)
cowplot::plot_grid(
  p1, cowplot::plot_grid(p2, p3, ncol = 1, align = "hv"),
  nrow = 1, labels = "AUTO")
dev.off()

# first maxima & extinction ----
temp_dat <- filter(dat, Phage == "Phage Added", type == "Experimental")
temp_sum <- filter(dat_sum, Phage == "Phage Added", type == "Experimental")

png("./manuscript/first_maxima.png", width = 4, height = 3.5,
    units = "in", res = 150)
ggplot(temp_dat, 
       aes(x = Time, y = Measurements)) +
  geom_point(size = 0.75) +
  geom_point(data = temp_sum,
             aes(x = first_maxima_x, y = first_maxima_y), 
             pch = 4, size = 2, color = "red", stroke = 2, alpha = 0.5) +
  geom_vline(data = temp_sum,
             aes(xintercept = extin_time), lty = 2) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

# Create noisy data ----
datnoisy <- filter(dat, type == "Experimental", Phage == "No Phage")
set.seed(2)
datnoisy$Measurements <-
  round(datnoisy$Measurements +
          0.015*c(arima.sim(model = list(order = c(0, 0, 0)),
                           n = length(datnoisy$Measurements))) +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.8, 0.2)) *
          rexp(n = length(datnoisy$Measurements), rate = 20)  +
          sample(x = c(0, 1), length(datnoisy$Measurements), 
                 replace = TRUE, prob = c(0.85, 0.15)) *
          rexp(n = length(datnoisy$Measurements), rate = 19),
        3)

ggplot(data = datnoisy, aes(x = Time, y = Measurements)) +
  geom_point()

datnoisy <- mutate(group_by(datnoisy, Well),
                   deriv = calc_deriv(y = Measurements, x = Time),
                   deriv5 = calc_deriv(y = Measurements, x = Time,
                                       window_width_n = 5),
                   deriv9 = calc_deriv(y = Measurements, x = Time,
                                       window_width_n = 9),
                   deriv13 = calc_deriv(y = Measurements, x = Time,
                                       window_width_n = 13),
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

#Smoothing raw data ----
p1 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_med5), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving median")

p2 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_avg5), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving average")

p3 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_loess), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "LOESS")

p4 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_gam), color = "red", lwd = .8, alpha = 0.8) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "GAM")

#Fitting during deriv calculation ----
p5 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv)) +
  geom_line() +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 2")

p6 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv5)) +
  geom_line() +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 5")

p7 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv9)) +
  geom_line() +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 9")

p8 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv13)) +
  geom_line() +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 13")

png("./manuscript/noise.png", width = 8, height = 4,
    units = "in", res = 150)
cowplot::plot_grid(ncol = 2, labels = "AUTO",
  cowplot::plot_grid(p1, p2, p3, p4, ncol = 2, align = "hv"),
  cowplot::plot_grid(p5, p6, p7, p8, ncol = 2, align = "hv")
)
dev.off()
