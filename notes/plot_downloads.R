library(cranlogs)
library(gcplyr)
library(dplyr)
library(ggplot2)

dat <- cran_downloads(
  package = c("gcplyr", "QurvE", "growthcurver", "growthrates",
              "opm", "growr", "biogrowth"), 
  from = "2023-01-01", to = Sys.Date())
dat$package <- relevel(factor(dat$package), ref = "gcplyr")

dat <- mutate(group_by(dat, package),
              sm_count = smooth_data(x = date, y = count,
                                     sm_method = "moving-average",
                                     window_width_n = 5))

plotdat <- filter(dat, 
                  package %in% c("gcplyr", "growthcurver", "QurvE", "growthrates"))
colorfunc <- colorRampPalette(c("lightpink3", "lightblue3", "darkseagreen1"))
colors <- colorfunc(length(unique(plotdat$package))-1)

ggplot(data = plotdat, aes(x = date)) +
  # geom_point(data = filter(plotdat, package != "gcplyr"), 
  #            aes(y = count, color = package), alpha = 0.5) +
  geom_line(data = filter(plotdat, package != "gcplyr"), 
            aes(y = sm_count, color = package), alpha = 0.8) +
  scale_color_manual(values = colors) +
  geom_point(data = filter(plotdat, package == "gcplyr"), 
             aes(y = count)) +
  geom_line(data = filter(plotdat, package == "gcplyr"), 
            aes(y = sm_count), lwd = 1.25) +
  theme_bw()
