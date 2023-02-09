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
                                        window_width_n = 11))

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
  labs(x = "Time (hr)", y = "Per-capita Derivative (/hr)") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

png("./
cowplot::plot_grid(
  ncol = 1, rel_heights = c(1.1, 1, 1),
  p1, p2, p3)
