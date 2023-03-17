# Setup ----

library(gcplyr)
library(ggplot2)
library(dplyr)

#This code was run to filter down all the Blazanin et al Travisano:
# growth curve data into just the example data for this paper
# temp <- read.csv("./manuscript/Isolate_growth_curves.csv")
# temp <- dplyr::filter(temp, Date == "2019-09-10", Proj == "125",
#                       Pop == "Anc", Rep_Well == "1", Media %in% c("50", "25-50"))
# write.csv(temp, "./manuscript/Isolate_growth_curves.csv",
#           row.names = FALSE)

# Load experimental data ----
dat <-
  read_wides(
    files = "./manuscript/2021-10-15_Emma_Growth_Curve.csv",
    startrow = 29, startcol = "B")

dat <- trans_wide_to_tidy(dat, id_cols = c("file", "Time", "T 600"))
dat <- mutate(dat,
              Time = lubridate::time_length(lubridate::hms(Time), unit = "hour"),
              type = "nondiaux")

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

# Load diauxie experimental data ----
dat_diaux <- read.csv("./manuscript/Isolate_growth_curves.csv")
dat_diaux$Well <- "Z1"
dat_diaux$init_moi <- "0"
dat_diaux$type <- "diauxic"
dat_diaux <- mutate(dat_diaux, Time = Time_s/3600)
#Subtract blank
dat_diaux$Measurements <- 
  dat_diaux$OD600 - lm(OD600 ~ cfu_ml, dat_diaux)$coefficients[1]
dat_diaux <- select(dat_diaux, Time, Well, Measurements, init_moi, type)

# Merge & analyze ----
dat <- dplyr::full_join(dat, dat_diaux)

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
            init_dens = first_minima(Measurements, return = "y",
                                     window_width_n = 5),
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

p1 <- ggplot(filter(dat, Phage == "No Phage", type == "nondiaux"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.75) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw()

p2 <- ggplot(filter(dat, Phage == "No Phage", type == "nondiaux"),
             aes(x = Time, y = deriv)) +
  geom_line() +
  labs(x = "Time (hr)", y = "Derivative (OD600/hr)") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

p3 <- ggplot(filter(dat, Phage == "No Phage", type == "nondiaux"),
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
temp_sum <- filter(dat_sum, Phage == "No Phage", type == "nondiaux")
temp_dat <- filter(dat, Phage == "No Phage", type == "nondiaux")
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
temp_sum <- filter(dat_sum, Phage == "No Phage", type == "diauxic")
temp_dat <- filter(dat, Phage == "No Phage", type == "diauxic")
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
temp_dat <- filter(dat, Phage == "Phage Added", type == "nondiaux")
temp_sum <- filter(dat_sum, Phage == "Phage Added", type == "nondiaux")

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
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

# Create noisy data ----
datnoisy <- filter(dat, type == "nondiaux", Phage == "No Phage")
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
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving median")

p2 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_avg5), color = "red", lwd = .8, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "Moving average")

p3 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_loess), color = "red", lwd = .8, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "LOESS")

p4 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = Measurements)) +
  geom_point(size = 0.8) +
  geom_line(aes(y = sm_gam), color = "red", lwd = .8, alpha = 0.8) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "OD600", title = "GAM")

#Fitting during deriv calculation ----
p5 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 2")

p6 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv5)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 5")

p7 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv9)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 9")

p8 <- ggplot(data = filter(datnoisy, Phage == "No Phage"), 
             aes(x = Time, y = deriv13)) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  labs(x = "Time (hr)", y = "Derivative\n(OD600/hr)", title = "n = 13")

png("./manuscript/noise.png", width = 8, height = 4,
    units = "in", res = 150)
cowplot::plot_grid(ncol = 2, labels = "AUTO",
  cowplot::plot_grid(p1, p2, p3, p4, ncol = 2, align = "hv"),
  cowplot::plot_grid(p5, p6, p7, p8, ncol = 2, align = "hv")
)
dev.off()
