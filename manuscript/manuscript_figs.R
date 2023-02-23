library(gcplyr)
library(ggplot2)
library(dplyr)

dat <- trans_wide_to_tidy(example_widedata_noiseless, id_cols = "Time")
example_design <- make_design(
  output_format = "tidy",
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
dat <- merge_dfs(dat, example_design)
dat$Well <- 
  factor(dat$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))

ggplot(dat, aes(x = Time, y = Measurements, color = Phage)) +
  geom_line() +
  facet_wrap(~Bacteria_strain, ncol = 12)

#Pick strain 33 to work with for our example wells
dat <- dplyr::filter(dat, Bacteria_strain == "Strain 33")

dat <- mutate(group_by(dat, Well, Bacteria_strain, Phage),
              deriv = calc_deriv(y = Measurements, x = Time,
                                 x_scale = 3600),
              deriv_percap = calc_deriv(y = Measurements, x = Time,
                                        x_scale = 3600, percapita = TRUE,
                                        blank = 0, trans_y = 'log',
                                        window_width_n = 11),
              deriv3 = calc_deriv(y = Measurements, x= Time,
                                  window_width_n = 3))

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
  coord_cartesian(ylim = c(-0.1, NA)) +
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

dat_sum <- summarize(group_by(dat, Well, Bacteria_strain, Phage),
                     diauxie_time = first_minima(y = deriv, x = Time,
                                                 window_width = 5*3600,
                                                 return_endpoints = FALSE,
                                                 return = "x"),
                     first_maxima_x = first_maxima(y = Measurements, x = Time,
                                                 window_width = 5*3600,
                                                 return = "x"),
                     first_maxima_y = first_maxima(y = Measurements, x = Time,
                                                 window_width = 5*3600,
                                                 return = "y"))


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

png("./manuscript/first_maxima.png", width = 4, height = 4,
    units = "in", res = 150)
ggplot(filter(dat, Phage == "Phage Added"), 
             aes(x = Time/3600, y = Measurements)) +
  geom_point(size = 0.75) +
  geom_point(data = filter(dat_sum, Phage == "Phage Added"),
             aes(x = first_maxima_x/3600, y = first_maxima_y), 
             pch = 4, size = 2, color = "red", stroke = 2, alpha = 0.5) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

#Define segments
halflength <- 900*6
dat <- mutate(group_by(dat, Well, Bacteria_strain, Phage),
              xstart = Time - halflength,
              xend = Time + halflength,
              ystart = Measurements - halflength*deriv3,
              yend = Measurements + halflength*deriv3)

png("./manuscript/deriv_fitting.png", width = 4, height = 4,
    units = "in", res = 150)
ggplot(filter(dat, Phage == "No Phage", Time %% 1800 == 0), 
       aes(x = Time/3600, y = Measurements)) +
  geom_point() +
  geom_segment(aes(x = xstart/3600, xend = xend/3600, y = ystart, yend = yend),
               color = "red", alpha = 0.5, lwd = 0.5) +
  #facet_grid(~Phage) +
  labs(x = "Time (hr)", y = "OD600") +
  theme_bw()
dev.off()

#Noisy data
datnoisy <- trans_wide_to_tidy(example_widedata, id_cols = "Time")
datnoisy <- merge_dfs(datnoisy, example_design)
datnoisy$Well <- 
  factor(datnoisy$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))
#Pick strain 33 to work with for our example wells
datnoisy <- dplyr::filter(datnoisy, Bacteria_strain == "Strain 33")
#Add more noise
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

png("test.png", width = 10, height = 10, units = "in", res = 300)
ggplot(datnoisy, aes(x = Time, y = Measurements, color = Phage)) +
  geom_line() +
  facet_wrap(~Bacteria_strain)
dev.off()

datnoisy <- 
  mutate(group_by(datnoisy, Well, Bacteria_strain, Phage),
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
