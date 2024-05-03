# Setup ----

library(gcplyr)
library(ggplot2)
library(dplyr)

#This code was run to filter down all the Blazanin et al Travisano:
# growth curve data into just the example data for this paper
# temp <- read.csv("./manuscript/Isolate_growth_curves2.csv")
# temp <- dplyr::filter(temp, Date == "2019-09-10", Proj == "125",
#                       Media == "25-50", 
#                       (Treat == "Anc" & Rep_Well == 3) |
#                       (Pop == "A" & Treat == "C" & Rep_Well == 1))
# temp$OD600_norm <- temp$OD600 - 0.07895 #subtract blank
# temp <- dplyr::select(temp, Treat, Time_s, OD600_norm)
# write.csv(temp, "./manuscript/Isolate_growth_curves.csv",
#           row.names = FALSE)

# Load experimental data ----
dat <- read.csv("./manuscript/Isolate_growth_curves.csv")
dat$Time <- dat$Time_s/3600

dat <- mutate(group_by(dat, Treat),
              deriv = calc_deriv(y = OD600_norm, x = Time, 
                                 window_width_n = 5),
              deriv_percap = calc_deriv(y = OD600_norm, x = Time,
                                        percapita = TRUE,
                                        blank = 0, trans_y = 'log',
                                        window_width_n = 5))

dat_sum <- 
  summarize(group_by(dat, Treat),
            max_percap = max(deriv_percap, na.rm = TRUE),
            max_dens = max(OD600_norm, na.rm = TRUE),
            auc = auc(x = Time, y = OD600_norm))


#Plots ----
#Max density
p1 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75, alpha = 0.8) +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  facet_grid(~ Treat,
             labeller = labeller(Treat = c("Anc" = "Ancestor", "C" = "Evolved"))) +
  ylim(NA, 0.63) +
  theme_bw() +
  geom_hline(data = dat_sum, aes(yintercept = max_dens), lty = 2, col = "#CD0057") +
  geom_text(data = dat_sum,
            aes(x = 8.5, y = max_dens + 0.05,
                label = paste("Max. density =", round(max_dens, 2))),
            col = "#CD0057") +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 16))

#AUC
p2 <- ggplot(dat, aes(x = Time, y = OD600_norm)) +
  geom_point(size = 0.75, alpha = 0.8) +
  geom_ribbon(aes(ymin = 0, ymax = OD600_norm), alpha = 0.15, 
              linewidth = 0, fill = "#CD0057") +
  labs(x = "Time (hr)", y = "OD600") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  facet_grid(~ Treat,
             labeller = labeller(Treat = c("Anc" = "Ancestor", "C" = "Evolved"))) +
  theme_bw() +
  geom_text(data = dat_sum,
            aes(x = 11.65, y = 0.04, 
                label = paste("AUC =", round(auc, 2), "OD hrs")),
            col = "#CD0057", size = 3.7) +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 16))

#Max growth rate
p3 <- ggplot(dat, aes(x = Time, y = deriv_percap)) +
  geom_point(size = 1.2, alpha = 0.8)  +
  labs(x = "Time (hr)", y = "Cellular growth\nrate (/hr)") +
  facet_grid(~ Treat,
             labeller = labeller(Treat = c("Anc" = "Ancestor", "C" = "Evolved"))) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24)) +
  ylim(NA, .8) +
  geom_hline(data = dat_sum, aes(yintercept = max_percap),
             col = "#CD0057", lty = 2) +
  geom_text(data = dat_sum, aes(x = 10.5, y = max_percap + 0.08, 
                label = paste("Max. growth rate =", round(max_percap, 2))),
            col = "#CD0057", size = 4) +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 16))

png("./manuscript/ex_metrics.png", width = 5, height = 7,
    units = "in", res = 300)
cowplot::plot_grid(
  ncol = 1, align = "hv", labels = "AUTO",
  p1, p2, p3)
dev.off()


