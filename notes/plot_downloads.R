library(cranlogs)
library(gcplyr)
library(dplyr)
library(ggplot2)

#Okabe and Ito 2008 colorblind-safe qualitative color scale
my_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7", "#000000")
scales::show_col(my_cols)

dat <- cran_downloads(
  package = c("gcplyr", "QurvE", "growthcurver", "growthrates",
              "opm", "growr", "biogrowth"), 
  from = "2023-01-01", to = Sys.Date())
dat$package <- relevel(factor(dat$package), ref = "gcplyr")

dat <- mutate(group_by(dat, package),
              sm_count = smooth_data(x = date, y = count,
                                     sm_method = "moving-average",
                                     window_width_n = 5))

print(summarize(group_by(dat, package), downloads = mean(count)))

plotdat <- filter(
  dat, 
  package %in% c("gcplyr", "growthcurver", "QurvE", "growthrates", "biogrowth"))

# colorfunc <- colorRampPalette(c("lightpink3", "lightblue3", "darkseagreen1"))
# colors <- colorfunc(length(unique(plotdat$package))-1)
colors = my_cols

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
