# Setup ----

library(gcplyr)
library(ggplot2)
library(dplyr)

#This code was run to filter down all the Blazanin et al Travisano:
# growth curve data into just the example data for this paper
# temp <- read.csv("./manuscript/Isolate_growth_curves.csv")
# temp <- dplyr::filter(temp, Date == "2019-09-10", Proj == "125",
#                       Pop == "Anc", Rep_Well == "1", Media %in% c("50", "25-50"))
# temp$OD600_norm <- temp$OD600 - 0.07895 #subtract blank
# temp <- dplyr::select(temp, Time_s, OD600_norm)
# write.csv(temp, "./manuscript/Isolate_growth_curves.csv",
#           row.names = FALSE)

# Load experimental data ----
dat <- read.csv("./manuscript/Isolate_growth_curves.csv")
dat$Well <- "A1"
dat$Time <- dat$Time_s/3600

dat <- mutate(group_by(dat, Well),
              deriv = calc_deriv(y = OD600_norm, x = Time, 
                                 window_width_n = 5),
              deriv_percap = calc_deriv(y = OD600_norm, x = Time,
                                        percapita = TRUE,
                                        blank = 0, trans_y = 'log',
                                        window_width_n = 5))

dat_sum <- 
  summarize(group_by(dat, Well),
            max_percap = max(deriv_percap, na.rm = TRUE),
            max_percap_time = Time[which.max(deriv_percap)],
            max_percap_dens = OD600_norm[which.max(deriv_percap)],
            init_dens = first_minima(OD600_norm, return = "y",
                                     window_width_n = 5),
            lag_time = lag_time(x = Time, y = OD600_norm,
                                deriv = deriv_percap, y0 = init_dens),
            max_percap_time1 = Time[which.max(deriv_percap)-2],
            max_percap_time2 = Time[which.max(deriv_percap)+2],
            max_dens = max(OD600_norm, na.rm = TRUE),
            auc = auc(x = Time, y = OD600_norm))


#Max density, AUC, max percap, lag time, max percap ----
p1 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw() +
  geom_hline(yintercept = dat_sum$max_dens, lty = 2, col = "red4") +
  geom_text(aes(x = 13, y = dat_sum$max_dens + 0.02,
                label = paste("Maximum density =", dat_sum$max_dens)),
            col = "red4")

p2 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75) +
  geom_ribbon(aes(ymin = 0, ymax = OD600_norm)) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  theme_bw()

p3 <- ggplot(dat, aes(x = Time, y = deriv_percap)) +
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


