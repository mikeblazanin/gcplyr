#Read in files
library(reshape2)
library(dplyr)
library(ggplot2)

setwd("C:/Users/mikeb/Google Drive/Research Projects/12 Growth Curves/1_2019-07-03_MOIDensityGrowthCurve")

read_blockcurves <- function(files, extension = NULL, 
                             startrow, endrow, startcol, endcol) {
  #Inputs: a list of relative filepaths
  #Outputs: a list of named blockcurves
  
  require(tools)
  require(readxl)
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Determine file extension
  if (is.null(extension)) {extension <- file_ext(files[1])}
  
  #Read files
  i <- 1
  if (extension == "csv") {
    for (fil in files) {
      outputs[[i]] <- as.numeric(read.csv(fil, colClasses = "character")
                                 [startrow:endrow, startcol:endcol])
      i <- i+1
    }
  } else if (extension == "xls") {
    for (fil in files) {
      suppressMessages(outputs[[i]] <- 
        read_xls(fil, col_names = FALSE, col_types = "text")
        [startrow:endrow, startcol:endcol])
      i <- i+1
    }
  } else if (extension == "xlsx") {
    for (fil in files) {
      suppressMessages(outputs[[i]] <- 
        read_xlsx(fil, col_names = FALSE, col_types = "text")
        [startrow:endrow, startcol:endcol])
      i <- i+1
    }
  }
  #Save timepoint information (without file extension)
  names(outputs) <- sub("^([^.]*).*", "\\1", files)

  return(outputs)
}

my_blockcurves <- read_blockcurves(list.files(), extension = "xlsx",
                                   startrow = 25, endrow = 32,
                                   startcol = 2, endcol = 13)

#Make into wide format
block_to_widecurves <- function(blockcurves) {
  #Inputs: a list of named blockcurves, each block being a dataframe
  #Outputs: a single widecurve data frame
  output <- data.frame(matrix(NA, nrow = length(blockcurves),
                              ncol = (nrow(blockcurves[[1]])*ncol(blockcurves[[1]])+1)))
  
  #Save timepoint info
  output[ , 1] <- names(blockcurves)
  
  i <- 1
  for (curve in blockcurves) {
    #Save data
    output[i, 2:ncol(output)] <- as.vector(t(curve))
    i <- i+1
  }
  colnames(output) <- c("Timepoint", as.character(1:(ncol(output)-1)))
  return(output)
}

my_widecurves <- block_to_widecurves(my_blockcurves)

#Make into tidy format
tidycurves <- melt(my_widecurves, id.vars = 1, variable.name = "Well", 
                   value.name = "OD")

#smooth
moving_average <- function(my_data, window_width, subset_by) {
  #data must be sorted sequentially before fed into function
  #my_data is a vector of the data to be smoothed
  #smooth over is how many sequential entries to average
  #the unique values of subset_by will be what is iterated over
  out_list <- rep(NA, length(my_data))
  cntr = 1
  for (my_uniq in unique(subset_by)) {
    my_sub <- as.numeric(subset(my_data, subset_by == my_uniq))
    out_list[cntr:(cntr+length(my_sub)-window_width)] <- 0
    for (i in 1:window_width) {
      out_list[(cntr):(cntr+length(my_sub)-window_width)] <-
        out_list[(cntr):(cntr+length(my_sub)-window_width)] + 
        my_sub[i:(length(my_sub)-window_width+i)]
    }  
    cntr <- cntr+length(my_sub)
  }
  out_list <- out_list/window_width
  return(out_list)
}

tidycurves$smoothed <- smooth_data(tidycurves$OD, window_width = 2,
                              subset_by = tidycurves$Well)
tidycurves$Timepoint <- as.POSIXct(tidycurves$Timepoint,
                                            format = "%Y-%m-%d %H-%M-%S")
tidycurves$Timepoint <- as.numeric(difftime(tidycurves$Timepoint, 
                                            min(tidycurves$Timepoint),
                                            units = "mins"))

#Extract peaks
#extraction function: to get OD peak height and time
analyze_curves <- function(od_data, time_data, bandwidth = 10, return) {
  #Takes vectors of the od_data and time_data
  #Bandwidth is how wide the window should be to look for a peak
  #Narrower bandwidth will get you an earlier local maxima
  #Wider bandwidth will get you a later more-global maxima
  #Designed to be run w/ group_by
  prev_max_pos <- 0
  cnt_max_pos <- bandwidth
  while (cnt_max_pos != prev_max_pos) {
    prev_max_pos <- cnt_max_pos
    search_start <- pmax(cnt_max_pos-bandwidth, 1) #start of the search window
    search_end <- pmin(search_start + 2*bandwidth, length(od_data)) #end of the search window
    cnt_max_pos <- which.max(od_data[search_start:search_end]) + search_start - 1
  }
  if (return == "max") {return(od_data[cnt_max_pos])
  } else if (return == "maxtime") {return(time_data[cnt_max_pos])}
}

#Group data by indiv growth curves
grp_data1 <- dplyr::group_by(tidycurves[!is.na(tidycurves$smoothed), ], 
                             Well)

#Get OD peak height & time for each growth curve
out_data1 <- dplyr::summarize(grp_data1, 
                              max = analyze_curves(smoothed, Timepoint, 
                                                   bandwidth = 9, return = "max"),
                              maxtime = analyze_curves(smoothed, Timepoint, 
                                                       bandwidth = 9, 
                                                       return = "maxtime"))

#Define layout of the plate
layout_plt <- data.frame("Well" = 1:96, "Bacteria" = NA, "Phage" = "U136B",
                         "Init_Dens" = NA, "MOI" = NA)
layout_plt$Bacteria[layout_plt$Well %in% c(1:4, 13:16, 25:28, 37:40)] <- "BW"
layout_plt$Bacteria[layout_plt$Well %in% c(49:52, 61:64, 73:76, 85:88)] <- "JW"
layout_plt$Bacteria[layout_plt$Well %in% c(5:8, 17:20, 29:32, 41:44)] <- "RGB036"
layout_plt$Bacteria[layout_plt$Well %in% c(53:56, 65:68, 77:80, 89:92)] <- "RGB058"
layout_plt$Bacteria[layout_plt$Well %in% c(9:12, 21:24, 33:36, 45:48)] <- "RGB071"
layout_plt$Bacteria[layout_plt$Well == 57] <- "LB Only"
layout_plt$MOI <- rep(c(0, 0.01, 0.1, 1), times = 2, each = 12)
layout_plt$Init_Dens <- rep(c(1, 0.1, 0.01, 0.001), times = 24)

tidymerge <- merge(tidycurves, layout_plt, by = "Well")
tidymerge <- tidymerge[complete.cases(tidymerge), ]
tidymerge$OD <- as.numeric(tidymerge$OD)
tidymerge <- tidymerge[order(tidymerge$Well), ]
                                
outmerge <- merge(out_data1, layout_plt, by = "Well")
outmerge <- outmerge[complete.cases(outmerge), ]

#Plot peak points
outmerge$Init_Dens <- as.factor(outmerge$Init_Dens)
outmerge$MOI <- as.factor(outmerge$MOI)
outmerge$Bacteria <- factor(outmerge$Bacteria,
                               levels = c("BW", "JW", "RGB036",
                                           "RGB058", "RGB071", "LB Only"))

setwd("C:/Users/mikeb/Google Drive/Research Projects/12 Growth Curves/")
png("peakheights.png", width = 10, height = 7, units = "in", res = 300)
ggplot(data = outmerge, aes(x = Init_Dens, y = max, color = MOI,
                            group = MOI)) +
  facet_grid(~Bacteria) + geom_point(size = 3) + geom_line() +
  theme_bw() +
  labs(y = "Height of Peak Bacterial Density",
       x = "Initial Density of Bacteria & Phage") +
  theme(strip.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
dev.off()

ggplot(data = outmerge, aes(x = MOI, y = max, color = Init_Dens,
                            group = Init_Dens)) +
  facet_grid(~Bacteria) + geom_point() + geom_line()

ggplot(data = outmerge, aes(x = Init_Dens, y = maxtime, color = MOI,
                            group = MOI)) +
  facet_grid(~Bacteria) + geom_point() + geom_line()

ggplot(data = outmerge, aes(x = MOI, y = maxtime, color = Init_Dens,
                            group = Init_Dens)) +
  facet_grid(~Bacteria) + geom_point() + geom_line()


#Plot curves & peaks
view_peaks <- function(time_data, dens_data, well_contents, plt_point = TRUE,
                       peak_time = NULL, peak_dens = NULL, peak_well_contents,
                       numplots = 9, lwd = 1) {
  #Inputs:  time_data - vector of time values
  #         dens_data - vector of density values, matched with time_data
  #         well_contents - vector of well contents, matched with time_data
  #         peak_time - vector of peaks' time values
  #         peak_dens - vector of peaks' density values, matched with peak_time
  #         peak_well_contents - vectore of peaks' well contents, matched with peak_time
  require(ggplot2)
  for (start_group in seq(from = 1, to = length(unique(well_contents)),
                          by = numplots)) {
    my_wells <- unique(well_contents)[start_group:(start_group+numplots-1)]
    myplot <- ggplot(data = data.frame(
          "dens" = dens_data[well_contents %in% my_wells],
          "time" = time_data[well_contents %in% my_wells],
          "contents" = well_contents[well_contents %in% my_wells]),
        aes(x = time, y = dens)) +
      geom_line(lwd = lwd) +
      facet_wrap(~contents)
    if (plt_point) {
      myplot <- myplot + 
        geom_point(data = data.frame(
          "dens" = peak_dens[peak_well_contents %in% my_wells],
          "time" = peak_time[peak_well_contents %in% my_wells],
          "contents" = peak_well_contents[peak_well_contents %in% my_wells]),
          aes(x = time, y = dens),
          size = 3, pch = 13)
    }
    print(myplot)
  }
}
          
view_peaks(tidymerge$Timepoint, tidymerge$OD, tidymerge$Well,
           peak_time = outmerge$maxtime, peak_dens = outmerge$max, 
           peak_well_contents = outmerge$Well)

setwd("C:/Users/mikeb/Google Drive/Research Projects/12 Growth Curves/")
for (bact in unique(tidymerge$Bacteria)) {
  my_sub <- tidymerge[tidymerge$Bacteria == bact, ]
  out_sub <- outmerge[outmerge$Bacteria == bact, ]
  my_sub$MOI <- as.factor(paste("MOI:", my_sub$MOI))
  out_sub$MOI <- as.factor(paste("MOI:", out_sub$MOI))
  my_sub$Init_Dens <- as.factor(paste("Init Dens:", my_sub$Init_Dens))
  out_sub$Init_Dens <- as.factor(paste("Init Dens:", out_sub$Init_Dens))
  png(filename = paste(bact, "_growcurves.png", sep = ""),
      width = 10, height = 10, units = "in", res = 300)
  print(ggplot(my_sub, aes(x = Timepoint, y = OD)) +
          geom_line(lwd = 2) +
          facet_grid(MOI~Init_Dens, drop = FALSE) + 
          geom_point(data = out_sub, aes(x = maxtime, y = max),
                     size = 5, pch = 19, col = "red", alpha = 0.6) +
          labs(title = bact, x = "Time (mins)", 
               y = "Bacterial Density (OD600)") + 
          theme_bw() +
          theme(strip.text = element_text(size = 18),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 14),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16),
                title = element_text(size = 20)))
  dev.off()
  png(filename = paste(bact, "_growcurves_flip.png", sep = ""),
      width = 10, height = 10, units = "in", res = 300)
  print(ggplot(my_sub, aes(x = Timepoint, y = OD)) +
          geom_line(lwd = 2) +
          facet_grid(Init_Dens~MOI, drop = FALSE) + 
          geom_point(data = out_sub, aes(x = maxtime, y = max),
                     size = 5, pch = 19, col = "red", alpha = 0.6) +
          labs(title = bact) + theme_bw() +
          theme(strip.text = element_text(size = 18),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 14),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16),
                title = element_text(size = 20)))
  
  dev.off()
}


###Plot EOP
setwd("C:/Users/mikeb/Google Drive/Research Projects/12 Growth Curves/")
eopdata <- read.csv("EOP_data.csv")
set.seed(7)
png("eop_data.png", width = 10, height = 7, units = "in", res = 300)
ggplot(eopdata, aes(x = Bacteria, y = log10(EOP))) +
  geom_jitter(aes(shape = BD), cex = 5, width = 0.2, height = 0) +
  scale_shape_manual(name = "Below Detection Limit",
                     labels = c("No", "Yes"),
                     values = c(16, 1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  labs(y = "Log(Efficiency of Plaquing)")
dev.off()
