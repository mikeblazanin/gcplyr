#Let's walk through a basic workflow using gcplyr

#First, we need to install gcplyr, if it hasn't been already
install.packages("devtools")
devtools::install_github("mikeblazanin/gcplyr")

#We also need to install a couple other packages
install.packages(c("lubridate", "ggplot2", "dplyr"))

#Then, we need to load gcplyr
library(gcplyr)

#Finally, we need to make sure that our working directory is set to
# the directory where our files are located. 

#ENTER YOUR DIRECTORY PATH HERE
setwd("C:\\Users\\mikeb\\Documents\\Code\\gcplyr\\notes\\example_for_labmeeting_files\\")

#Let's load in our growth curves
mydata_wd <- read_wides(files = c("Rep1.csv", "Rep2.csv"),
                        startrow = 29, startcol = "B",
                        metadata = list("Date" = c(6, "B")))

#Let's transform our growth curves into a tidy-format for merging with
# design information and carrying out analyses
mydata_td <- trans_wide_to_tidy(mydata_wd, 
                                id_cols = c("file", "Date", "Time", "T° 600"))

#Let's combine our growth curves data into a single data.frame
mydata_mrg <- merge_dfs(mydata_td, collapse = TRUE)

#Great! We've got all our data in a nice format, now let's load in our
# design information
mydesign <- import_blockdesigns(c("design_bact.csv", "design_phg.csv"), 
                                wellnames_sep = "")

#Now lets merge our design and growth curves data
mydata <- merge_dfs(mydata_mrg, mydesign)

#Note that the Time column is currently in text form, and we'd like it
# as a numeric. We'll use lubridate to convert it
library(lubridate)
mydata$Time <- time_length(hms(mydata$Time))

#And now let's drop all the empty wells
library(dplyr)
mydata <- dplyr::filter(mydata, design_bact != "NA", design_phg != "NA")

#Now let's visualize!
library(ggplot2)

ggplot(data = mydata, 
       aes(x = Time, y = Measurements, 
           color = design_bact, lty = design_phg,
           group = paste(file, Well))) +
  geom_line()

#Easier to see what's going on in this plot
ggplot(data = mydata, 
       aes(x = Time, y = Measurements, 
           color = design_bact, lty = design_phg,
           group = paste(file, Well))) +
  geom_line() +
  facet_wrap(~design_bact)

#Now let's see what sort of aggregated statistics we can get out
# of this data

#Two things we might be interested in calculating are the:
# maximum per-capita growth rate
# area-under-the-curve

#First we subtract the negative control
mydata$Measurements <- mydata$Measurements - min(mydata$Measurements)

#Then we drop the Blank data
mydata <- filter(mydata, design_bact != "Blank")

#Then we do some smoothing
mydata$sm_od <- smooth_data(x = mydata$Time,
                            y = mydata$Measurements,
                            method = "moving-average",
                            subset_by = paste(mydata$Date, mydata$Well),
                            window_width_n = 5)

#Then we calculate the per-capita growth rate
mydata$percap <- calc_deriv(x = mydata$Time,
                            y = mydata$sm_od,
                            x_scale = 3600,
                            percapita = TRUE,
                            subset_by = paste(mydata$Date, mydata$Well))

#Let's plot the per-capita growth rates to see what they look like
ggplot(data = mydata, 
       aes(x = Time, y = percap, 
           color = design_bact, lty = design_phg,
           group = paste(file, Well))) +
  geom_line() +
  facet_wrap(~design_bact, scales = "free_y")

#Finally, we use dplyr and summarize to calculate the maximum
# per-capita growth rate and area-under-the-curve

#First we group by all the columns identifying each well
grouped_data <- group_by(mydata, file, Date, Well, design_bact,
                         design_phg)

mydata_sum <- summarize(grouped_data,
                        max_percap = max(percap, na.rm = TRUE),
                        auc = auc(x = Time, y = sm_od))

#Let's plot the summarized results
ggplot(data = mydata_sum,
       aes(x = design_bact, y = max_percap, shape = design_phg)) +
  geom_point()

ggplot(data = mydata_sum,
       aes(x = design_bact, y = auc, shape = design_phg)) +
  geom_point()


