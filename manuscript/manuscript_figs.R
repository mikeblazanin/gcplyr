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


#Max density, AUC, lag time, max percap ----
p1 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  ylim(NA, 0.63) +
  theme_bw() +
  geom_hline(data = dat_sum, aes(yintercept = max_dens), lty = 2, col = "red3") +
  geom_text(data = dat_sum,
            aes(x = 8.5, y = max_dens + 0.04,
                label = paste("Max. density =", round(max_dens, 2))),
            col = "red3")

p2 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75) +
  geom_ribbon(aes(ymin = 0, ymax = OD600_norm), alpha = 0.2, 
              linewidth = 0, fill = "red3") +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  ylim(NA, 0.63) +
  theme_bw() +
  geom_text(data = dat_sum,
            aes(x = 11.8, y = 0.1, 
                label = paste("AUC =", round(auc, 2), "OD hrs")),
            col = "red3")

p3 <- ggplot(data = dat, aes(x = Time, y = log(OD600_norm))) +
  geom_point(size = 1.2) +
  geom_point(data = filter(dat, Time >= dat_sum$max_percap_time1,
                           Time <= dat_sum$max_percap_time2),
             color = "blue3") +
  geom_segment(data = dat_sum,
               aes(x = lag_time, y = log(init_dens), xend = 6,
                   yend = log(init_dens) + max_percap*(6 - lag_time)),
               lty = 2, color = "blue3") +
  geom_vline(data = dat_sum,
             aes(xintercept = lag_time), lty = 2, color = "red3") +
  geom_hline(data = dat_sum,
             aes(yintercept = log(init_dens)),
             lty = 2) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 9), ylim = c(NA, -0.9)) +
  scale_x_continuous(breaks = c(0, 6)) +
  labs(x = "Time (hr)", y = "log(OD600)") +
  geom_text(data = dat_sum,
            aes(x = 5.7, y = -1, 
                label = paste("Lag time =", round(lag_time, 2), "hrs")),
            col = "red3")

p4 <- ggplot(dat, aes(x = Time, y = deriv_percap)) +
  geom_point(size = 1.2)  +
  labs(x = "Time (hr)", y = "Cellular growth rate\n(/hr)") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  ylim(NA, .95) +
  geom_hline(aes(yintercept = dat_sum$max_percap),
             col = "red3", lty = 2) +
  geom_text(aes(x = 12, y = dat_sum$max_percap + 0.07, 
                label = paste("Max. growth rate =", round(dat_sum$max_percap, 2))),
            col = "red3", size = 3.5) +
  theme_bw()

png("./manuscript/ex_metrics.png", width = 6, height = 5,
    units = "in", res = 300)
cowplot::plot_grid(
  nrow = 2, rel_heights = c(1.1, 1, 1), align = "hv", labels = "AUTO",
  p1, p2, p3, p4)
dev.off()


