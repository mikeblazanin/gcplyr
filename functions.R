#TODO: actually test & check that functions work
#       figure out where well names are brought in and put them
#        in the column names

#Change smoothing to include other functions
#LOESS (which can simplify to a weighted moving average)
#LOWESS
#Spline models

#wide-curves: dataframe with each column corresponding to a single well
#block-curves: dataframe where rows and columns match literally to a plate

check_diminputs <- function(diminput, dimname, files) {
  #A function that adjusts inputs to be lists if they're not already
  if (length(diminput) != length(files)) {
    if(length(diminput) != 1) {
      stop(paste("More than one", dimname, "value is supplied, but the number
                   of", dimname, "values is not equal to the number of files"))
    } else {
      return(rep(diminput, length(files)))
    }
  }
}

read_blockcurves <- function(files, extension = NULL, 
                             startrow = NULL, endrow = NULL, 
                             startcol = NULL, endcol = NULL,
                             sheet = NULL) {
  #A function that reads blockcurves into the R environment
  
  #Inputs:  a list of filepaths relative to the current working directory,
  #           where each one is a single plate read
  #         (optional) the extension of the files,"csv", "xls", or "xlsx"
  #           (will attempt to infer extension if none is provided)
  #         (optional) the row & columns where the density data is located
  #         startrow, endrow, startcol, endcol, sheet and extension can either
  #          be vectors or lists the same length as files, or a single value
  #          that applies for all files
  # 
  #Outputs: a list of blockcurves named by filename
  
  #Note if sheet is NULL defaults to first sheet
  
  require(tools)
  #Note later we make a require call for readxl
  
  if (!is.null(startrow)) {
    startrow <- check_diminputs(startrow, "startrow", files)
  }
  if (!is.null(endrow)) { 
    endrow <- check_diminputs(endrow, "endrow", files)
  }
  if (!is.null(startcol)) {
    startcol <- check_diminputs(startcol, "startcol", files)
  }
  if (!is.null(endcol)) {
    endcol <- check_diminputs(endcol, "endcol", files)
  }
  if (!is.null(sheet)) {
    sheet <- check_diminputs(sheet, "sheet", files)
  }
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Determine file extension
  if (is.null(extension)) {
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
                        USE.NAMES = FALSE)
  } else {
    extension <- check_diminputs(extension, "extension", files)
  }
  
  if (sum(extension == "xls" | extension == "xlsx") > 0) {require(readxl)}

  #Read files
  for (i in 1:length(files)) {
    if (extension[i] == "csv") {
        outputs[[i]] <- as.numeric(read.csv(files[i], colClasses = "character")
                                   [startrow[i]:endrow[i], startcol[i]:endcol[i]])
    } else if (extension[i] == "xls") {
        suppressMessages(outputs[[i]] <- 
                           readxl::read_xls(files[i], col_names = FALSE, 
                                    col_types = "text", sheet = sheet[i])
                         [startrow[i]:endrow[i], startcol[i]:endcol[i]])
    } else if (extension[i] == "xlsx") {
        suppressMessages(outputs[[i]] <- 
                           readxl::read_xlsx(files[i], col_names = FALSE, 
                                     col_types = "text", sheet = sheet[i])
                         [startrow[i]:endrow[i], startcol[i]:endcol[i]])
    }
  }
  #Save timepoint information (without file extension)
  names(outputs) <- sub("^([^.]*).*", "\\1", files)
  
  #Error checking for output dataframe dimensions
  if (var(apply(outputs, MARGIN = 1, dim)[1]) != 0) {
    warning("Not all blockcurves have the same number of rows of data")
  }
  if (var(apply(outputs, MARGIN = 1, dim)[2]) != 0) {
    warning("Not all blockcurves have the same number of columns of data")
  }
  
  return(outputs)
}

uninterleave <- function(interleaved_list, n) {
  #Inputs: a list of R objects
  #        how many sub-lists there should be (i.e. how many groups should
  #          the interleaved list be divided into)
  #Output: a list of lists of R objects
  #This function takes a list of R objects (e.g. a list of blockcurves)
  #and separates them into separate sub-lists (e.g. where each sub-list
  # corresponds to all blockcurves for a single plate from a multi-plate 
  # set of blockcurves)
  
  #Input checking
  if ((length(interleaved_list) %% n) != 0) {
    stop("Length of list must be divisible by n")
  }
  
  output <- rep(list(NA), n)
  for (i in 1:n) {
    output[[i]] <- interleaved_list[seq(from = i, to = length(interleaved_list), 
                                        by = n)]
  }
  
  return(output)
}

widen_blockcurves <- function(blockcurves) {
  #Inputs: a [list of] blockcurve[s] (optionally, named)
  #Outputs: a single widecurve dataframe
  
  if(class(blockcurves) != "list") {
    blockcurves <- list(blockcurves)
  }
  
  #Check that all blockcurves have same dimensions
  if (length(blockcurves) > 1) {
    if (var(apply(blockcurves, MARGIN = 1, dim)[1]) != 0) {
      stop("Not all blockcurves have the same number of rows of data")
    }
    if (var(apply(blockcurves, MARGIN = 1, dim)[2]) != 0) {
      stop("Not all blockcurves have the same number of columns of data")
    }
  }
  
  ##Pretty sure these rows don't do anything
  # output <- data.frame(matrix(nrow = length(blockcurves,
  #                                           ncol = nrow(blockcurves[1]) *
  #                                             ncol(blockcurves[1]))))
  # colnames(output) <- 1:ncol(output)
  # rownames(output) <- names(blockcurves) #doesn't change if blockcurves are unnamed
  
  #convert list of dataframes to single dataframe
  #where each column is a well and each row is a plate read
  #(each column is a row-column combination from the blockcurve)
  #(each row is a single dataframe from blockcurves)
  output <- data.frame(t(sapply(blockcurves, FUN = function(x) {c(t(x))})))
  colnames(output) <- 1:ncol(output)
  rownames(output) <- names(blockcurves) #doesn't change if blockcurves are unnamed
  
  return(output)
}

import_blockcurves <- function(files, num_plates = 1, ...) {
  blockcurves1 <- read_blockcurves(files = files, ...)
  blockcurves2 <- uninterleave(blockcurves1, n = num_plates)
  widecurves <- rep(list(NA), num_plates)
  for (i in 1:length(blockcurves2)) {
    widecurves[[i]] <- widen_blockcurves(blockcurves2[[i]])
  }
  names(widecurves) <- paste("plate_", 1:length(widecurves), sep = "")
  return(widecurves)
}

import_widecurves <- function(files, extension = NULL, 
                              startrow = NULL, endrow = NULL, 
                              startcol = NULL, endcol = NULL,
                              timecol = NULL, headrow = NULL,
                              sheet = NULL) {
  #Inputs:  a list of filepaths relative to the current working directory,
  #           where each one is a widecurves set of data
  #         (optional) the extension of the files,"csv", "xls", or "xlsx"
  #           (will attempt to infer extension if none is provided)
  #         (optional) the row & columns where the data is located
  #         (optional) the column where timestamp info is located
  #           (if none provided by default the column immediately to the left 
  #           of startcol is assumed to be the time column)
  #         (optional) the sheet where data is located (if xls or xlsx)
  #           if none provided, defaults to first sheet
  #         startrow, endrow, startcol, endcol, timecol, sheet and extension 
  #           can either be vectors or lists the same length as files, 
  #           or a single value that applies for all files
  # 
  #Outputs: a list of blockcurves named by filename
  
  if (!is.null(startrow)) {
    startrow <- check_diminputs(startrow, "startrow", files)
  }
  if (!is.null(endrow)) { 
    endrow <- check_diminputs(endrow, "endrow", files)
  }
  if (!is.null(startcol)) {
    startcol <- check_diminputs(startcol, "startcol", files)
  }
  if (!is.null(endcol)) {
    endcol <- check_diminputs(endcol, "endcol", files)
  }
  if (!is.null(timecol)) {
    timecol <- check_diminputs(timecol, "timecol", files)
  } else if (as.numeric(startcol) > 1) {
    timecol <- rep(as.numeric(startcol)-1, length(files))
  } else {timecol <- NA}
  if (!is.null(sheet)) {
    sheet <- check_diminputs(sheet, "sheet", files)
  }
  if (!is.null(headrow)) {
    headrow <- check_diminputs(headrow, "headrow", files)
  } else if (as.numeric(startrow) > 1) {
    headrow <- rep(as.numeric(startrow)-1, length(files))
  } else {headrow <- NA}
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Determine file extension
  if (is.null(extension)) {
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
                        USE.NAMES = FALSE)
  } else {
    extension <- check_diminputs(extension, "extension", files)
  }
  
  if (sum(extension == "xls" | extension == "xlsx") > 0) {require(readxl)}
  
  #Read files (adding time info as row names)
  for (i in 1:length(files)) {
    if (extension[i] == "csv") {
      outputs[[i]] <- as.numeric(read.csv(files[i], colClasses = "character")
                                 [startrow[i]:endrow[i], startcol[i]:endcol[i]])
      if (!is.na(timecol[i])) {
        row.names(outputs[[i]]) <- as.numeric(read.csv(files[i], colClasses = "character")
                                              [startrow[i]:endrow[i], timecol[i]])
      }
      if (!is.na(headrow[i])) {
        colnames(outputs[[i]]) <- (read.csv(files[i], colClasses = "character")
        [headrow[i], startcol[i]:endcol[i]])
      }
    } else if (extension[i] == "xls") {
      suppressMessages(outputs[[i]] <- 
                         readxl::read_xls(files[i], col_names = FALSE, 
                                           col_types = "text", sheet = sheet[i])
                          [startrow[i]:endrow[i], startcol[i]:endcol[i]])
      if (!is.na(timecol[i])) {
        suppressMessages(row.names(outputs[[i]]) <- 
                           readxl::read_xls(files[i], col_names = FALSE, 
                                            col_types = "text", sheet = sheet[i])
                         [startrow[i]:endrow[i], timecol[i]])
      }
      if (!is.na(headrow[i])) {
        suppressMessages(colnames(outputs[[i]]) <-
                           readxl::read_xls(files[i], col_names = FALSE, 
                                            col_types = "text", sheet = sheet[i])
                         [headrow[i], startcol[i]:endcol[i]])
      }
    } else if (extension[i] == "xlsx") {
      suppressMessages(outputs[[i]] <- 
                         readxl::read_xlsx(files[i], col_names = FALSE, 
                                           col_types = "text", sheet = sheet[i])
                       [startrow[i]:endrow[i], startcol[i]:endcol[i]])
      if (!is.na(timecol[i])) {
        suppressMessages(row.names(outputs[[i]]) <- 
                           readxl::read_xlsx(files[i], col_names = FALSE, 
                                             col_types = "text", sheet = sheet[i])
                         [startrow[i]:endrow[i], timecol[i]])
      }
      if (!is.na(headrow[i])) {
        suppressMessages(colnames(outputs[[i]]) <-
                           readxl::read_xlsx(files[i], col_names = FALSE, 
                                            col_types = "text", sheet = sheet[i])
                         [headrow[i], startcol[i]:endcol[i]])
      }
    }
  }
  names(outputs) <- files
  
  return(outputs)
}

pivot_widecurves_longer <- function(widecurves, timestamps = NULL) {
  #If timestamps is not provided, it is assumed to be in the rownames
  # of each widecurves dataframe
  #If timestamps is a vector the same length as the number of rows
  # in widecurves
  
  require(tidyr)
  
  
  
  tidyr::pivot_longer()
  
}

layout_cleanup <- function(layout) {
  #This function takes a layout dataframe with ...'s where info needs to be 
  #iterated in and does so, while adding _A, _B etc for replicate wells
  #with the same contents
  #Wells labeled NA are not filled in (they are empty)
  
  #Define a matrix to track well contents we've seen before
  #So that when they come up again the replicate number can resume
  #where it left off
  vals_and_cntr <- matrix(nrow = 0, ncol = 2)
  for (i in 1:nrow(layout)) {
    for (j in 2:ncol(layout)) {
      if(!is.na(layout[i, j])) { #Non-empty well
        if (nchar(layout[i, j]) > 1) { #Non ... well
          current_val <- layout[i, j]
          if (!(current_val %in% vals_and_cntr[, 1])) {
            #This is the first time we've seen these well contents
            #So we should start numbering replicates at the beginning
            vals_and_cntr <- rbind(vals_and_cntr, c(current_val, 1))
            row <- nrow(vals_and_cntr)
          } else { 
            #this isn't the first time we've seen these contents
            #resume replicate numbering where we left off
            row <- which(vals_and_cntr[, 1] == current_val)
          }
        }
        layout[i, j] <- paste(current_val, 
                              LETTERS[as.numeric(vals_and_cntr[row, 2])], sep = "_")
        vals_and_cntr[row, 2] <- as.numeric(vals_and_cntr[row, 2]) + 1
      }
    }
  }
  return(layout)
}

#Merge layout & data
merge_lay_data <- function(layout, data) {
  #Tidies (melts) the layout dataframe
  #Then merges the now-tidy layout & data dataframes
  layout_mlt <- reshape2::melt(layout, id.vars = 1, value.name = "Contents", 
                               variable.name = "column")
  layout_mlt$Well <- paste(layout_mlt[, 1], substr(layout_mlt$column, 2, 
                                                nchar(as.character(layout_mlt$column))), 
                           sep = "")
  
  data_mlt <- reshape2::melt(data, id.vars = c("Time", "Temperature"), 
                             variable.name = "Well", value.name = "OD600")
  data_mlt$Contents <- layout_mlt$Contents[match(as.character(data_mlt$Well), 
                                                 as.character(layout_mlt$Well))]
  data_mlt$format <- paste(colnames(layout)[1], "_Rep", sep = "")
  return(data_mlt)
}

smooth_data <- function(algorithm = "loess", x, y,
                        formula = NULL, 

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
grp_data1 <- dplyr::group_by(spl_data1[!is.na(spl_data1$sm_od), ], 
                     bacteria, phage, phageshock, bactshock, mediashock, Rep, Contents)

#Get OD peak height & time for each growth curve
out_data1 <- dplyr::summarize(grp_data1, 
                      max = analyze_curves(sm_od, Time, 
                                           bandwidth = 20, return = "max"),
                      maxtime = analyze_curves(sm_od, Time, 
                                               bandwidth = 20, return = "maxtime"))


#Plots to visually inspect peak designation accuracy
view_peaks <- function(data_mlt, data_out, plt_point = TRUE,
                       numplots = 9, lwd = 1) {
  for (start_group in seq(from = 1, to = length(unique(data_mlt$Contents)), by = numplots)) {
    my_groups <- unique(data_mlt$Contents)[start_group:(start_group+numplots-1)]
    myplot <- ggplot(data = data_mlt[data_mlt$Contents %in% my_groups, ],
                 aes(x = Time, y = sm_od)) + geom_line(lwd = lwd) +
            facet_wrap(~Contents) + ylab("Smoothed OD600")
    if (plt_point) {myplot <- myplot + 
                              geom_point(data = data_out[data_out$Contents %in% my_groups, ],
                                                  aes(x = maxtime, y = max),
                                                  size = 3, pch = 13)
    }
    print(myplot)
    # ggsave(filename = paste(start_group, "_growcurves.pdf", sep = ""),
    #        device = "pdf", width = 8, height = 8, units = "in")
  }
}


#### Kenichi's script:
#### # The script reads optical density data from  the microplate reader in the xls spreadsheets generated by evoware

require(xlsx)

# cd into the directory with the excel spreadsheets
plate_reader_spreads <- list.files()
# kv <- matrix(ncol=96,nrow=length(plate_reader_spreads))



microplate_output <- list()

for (i in 1:length(plate_reader_spreads))
	{
	# By evoware default, the spreadsheet format begins the microplate reader on row #27, col #B:M (note there are 12 columns)
	# with shaking by incubator:
	z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=27, endRow=35, colIndex=2:14)
	# without shaking by incubator:
	#z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=24, endRow=32, colIndex=2:14)
	microplate_output[[i]] <- z
	}

# To visualize dynamics in a given well:
retrieve_well_timestep <- function(step,x,y)
	{
	return(microplate_output[[step]][x,y])
	}

# TODO: fix this to use mapply
visualize_dynamics <- function(x,y)
	{
	retrieve_this_well <- function(step)
		{
		return(retrieve_well_timestep(step,x,y))
		}
		
	od_measures <- sapply(1:length(plate_reader_spreads), retrieve_this_well)
	#od_measures <- sapply(1:281, retrieve_this_well)
	plot(od_measures,xlab="measurement point", ylab="OD",t="l",ylim=range(microplate_output))
	}

dev.new(height=8,width=10)
par(mfrow=c(8,12),mar=c(1,2,1,1))
for (i in 1:8)
	for (j in 1:12)
		visualize_dynamics(i,j)

matrixAll <- matrix(nrow=length(plate_reader_spreads), ncol=96)
for (i in 1:length(plate_reader_spreads))
 {
 matrixAll[i,] <- c(as.matrix(microplate_output[[i]]))
 }


## Code taken from Trav phage paper 3-22-2020 ----
#Define function that calculates derivatives
calc_deriv <- function(density, percapita = FALSE,
                       subset_by = NULL, time = NULL,
                       time_normalize = NULL) {
  #Note! density values must be sorted sequentially into their unique sets already
  
  #Provided a vector of density values, this function returns (by default) the
  # difference between sequential values
  #if percapita = TRUE, the differences of density are divided by density
  #if subset_by is provided, it should be a vector (same length as density),
  # the unique values of which will separate calculations
  #if time_normalize is specified, time should be provided as a simple 
  # numeric (e.g. number of seconds) in some unit
  #Then the difference will be normalized for the time_normalize value
  #(e.g. if time is provided in seconds and the difference per hour is wanted,
  # time_normalize should = 3600)
  
  #Check inputs
  if (!is.numeric(time)) {
    stop("time is not numeric")
  }
  if (!is.null(time_normalize)) {
    if (!is.numeric(time_normalize)) {
      stop("time_normalize is not numeric")
    } else if (is.null(time)) {
      stop("time_normalize is specified, but time is not provided")
    }
  }
  
  #Calc derivative
  ans <- c(density[2:length(density)]-density[1:(length(density)-1)])
  #Percapita (if specified)
  if (percapita) {
    ans <- ans/density[1:(length(density)-1)]
  }
  #Time normalize (if specified)
  if (!is.null(time_normalize)) {
    ans <- ans/
      (c(time[2:length(time)]-time[1:(length(time)-1)])/time_normalize)
  }
  #Subset by (if specified)
  if (!is.null(subset_by)) {
    ans[subset_by[2:length(subset_by)] != subset_by[1:(length(subset_by)-1)]] <- NA
  }
  return(c(ans, NA))
}

#Smooth data
gc_data$sm_loess <- NA
for (my_well in unique(gc_data$uniq_well)) {
  my_rows <- which(gc_data$uniq_well == my_well)
  #Smooth with loess
  gc_data$sm_loess[my_rows] <- loess(cfu_ml ~ Time_s, span = 0.4,
                                     data = gc_data[my_rows, ])$fitted
}

#Calculate growth per hour from loess curve
gc_data$deriv_sm_loess <- calc_deriv(gc_data$sm_loess,
                                     subset_by = gc_data$uniq_well,
                                     time = gc_data$Time_s,
                                     time_normalize = 3600)

#Calculate per capita growth per hour from loess curve
gc_data$percap_deriv_sm_loess <- calc_deriv(gc_data$sm_loess,
                                            percapita = TRUE,
                                            subset_by = gc_data$uniq_well,
                                            time = gc_data$Time_s,
                                            time_normalize = 3600)

find_local_extrema <- function(values, 
                               return_maxima = TRUE,
                               return_minima = TRUE,
                               width_limit = NULL,
                               height_limit = NULL,
                               remove_endpoints = TRUE,
                               na.rm = FALSE) {
  #Takes a vector of values and returns a vector of the indices
  # of all local value extrema (by default, returns both maxima and minima)
  # To only return maxima or minima, change return_maxima/return_minima to FALSE
  
  #width_limit and/or height_limit must be provided
  #Width is how wide the window will be to look for a maxima/minima
  # Narrower width will be more sensitive to narrow local maxima/minima
  # Wider width will be less sensitive to narrow local maxima/minima
  #Height is how high or low a single step is allowed to take
  # e.g. a maxima-finding function will not pass a valley deeper
  # than height_limit
  #Note that this also limits approaches to extrema, so if set too small
  # function may converge on non-peaks
  #If both width_limit and height_limit are provided, steps are limited
  # conservatively (a single step must meet both criteria)
  
  #This function is designed to be compatible with dplyr::group_by and summarize
  
  #Check inputs
  if (!return_maxima & !return_minima) {
    stop("Both return_maxima and return_minima are FALSE, at least one must be TRUE")
  }
  if (is.null(width_limit) & is.null(height_limit)) {
    stop("Either width_limit or height_limit must be provided")
  }
  if (!is.null(width_limit)) {
    if (width_limit%%2 == 0) {
      warning("width_limit must be odd, will use ", width_limit-1, " as width_limit")
      width_limit <- width_limit - 1
    }
  }
  if (is.null(width_limit) & !is.null(height_limit)) {
    warning("height_limit alone tends to be sensitive to height_limit parameter, use with caution")
  }
  if (na.rm == TRUE & sum(is.na(values)) > 0) {
    if (!all(is.na(values[(1+length(values)-sum(is.na(values))):length(values)]))) {
      warning("Removing NAs found within values vector, returned indices will refer to non-NA values")
      print(values)
    }
    values <- values[!is.na(values)]
  } else if(any(is.na(values))) {
    stop("Some provided values are NA and na.rm = FALSE")
  }
  
  #Define sub-function to find limits of the window
  get_window_limits <- function(cnt_pos,
                                width_limit = NULL,
                                height_limit = NULL,
                                looking_for = c("minima", "maxima"),
                                values = NULL) {
    #Check inputs
    if (length(looking_for) > 1) {stop("looking_for must be specified")}
    if (!is.null(height_limit) & is.null(values)) {
      stop("height_limit is specified, but no values are provided")
    }
    if (is.null(width_limit) & is.null(height_limit)) {
      stop("Either width_limit or height_limit must be provided")
    }
    
    #Define window limits
    window_start <- c(NA, NA)
    if (!is.null(width_limit)) { #using width limit
      window_start[1] <- max(c(1, cnt_pos-floor(width_limit/2)))
    }
    if (!is.null(height_limit)) { #using height limig
      #For startpoint height, we want the latest point that is
      #behind of our current point and
      #either:
      # below current height - height limit
      # or above current height + height limit
      #Then we move one place forward 
      # (so it's the last value w/in height limit)
      window_start[2] <- max(c(1,
                               1+which(1:length(values) < cnt_pos &
                                         (values >= (values[cnt_pos] + height_limit) |
                                            values <= (values[cnt_pos] - height_limit)))))
      #Make sure we're going at least 1 point backwards
      if(window_start[2] >= cnt_pos) {window_start[2] <- cnt_pos-1}
    }
    window_end <- c(NA, NA)
    if (!is.null(width_limit)) { #using width limit
      window_end[1] <- min(c(length(values), cnt_pos+floor(width_limit/2)))
    }
    if (!is.null(height_limit)) { #using height limit
      #For endpoint height, we want the earliest point that is
      #forward of our current point and
      #either:
      # below current height - height limit
      # or above current height + height limit
      #Then we move one place back 
      # (so it's the last value w/in height limit)
      window_end[2] <- min(c(length(values),
                             -1+which(1:length(values) > cnt_pos & #not backwards
                                        (values <= (values[cnt_pos] - height_limit) |
                                           values >= (values[cnt_pos] + height_limit)))))
      #Make sure we're going at least one point forwards
      if (window_end[2] <= cnt_pos) {window_end[2] <- cnt_pos+1}
    }
    return(c(max(window_start, na.rm = T), min(window_end, na.rm = T)))
  }
  
  find_next_extrema <- function(cnt_pos, values,
                                width_limit = NULL,
                                height_limit = NULL,
                                looking_for = c("minima", "maxima")) {
    if (cnt_pos == length(values)) {best_pos <- cnt_pos-1
    } else {best_pos <- cnt_pos+1}
    
    #Save the starting position so we never go backwards
    start_pos <- cnt_pos
    
    ##Looking for next maxima
    if(looking_for == "maxima") {
      while (cnt_pos != best_pos) {
        #Move the previous best pointer to current pointer location
        best_pos <- cnt_pos
        #Get next window limits
        window_lims <- get_window_limits(cnt_pos = cnt_pos,
                                         width_limit = width_limit,
                                         height_limit = height_limit,
                                         looking_for = "maxima",
                                         values = values)
        #Make sure we're not going backwards
        window_lims <- c(max(start_pos, window_lims[1]),
                         max(start_pos, window_lims[2]))
        #Then move current pointer to highest point within window
        # (making sure not to check non-integer indices, or indices below 1 or
        #  higher than the length of the vector)
        cnt_pos <- window_lims[1]-1+which.max(values[window_lims[1]:window_lims[2]])
      }
      ##Looking for next minima
    } else if (looking_for == "minima") {
      while (cnt_pos != best_pos) {
        #Move the previous best pointer to current pointer location
        best_pos <- cnt_pos
        #Get next window limits
        window_lims <- get_window_limits(cnt_pos = cnt_pos,
                                         width_limit = width_limit,
                                         height_limit = height_limit,
                                         looking_for = "minima",
                                         values = values)
        #Make sure we're not going backwards
        window_lims <- c(max(start_pos, window_lims[1]),
                         max(start_pos, window_lims[2]))
        #Then move current pointer to lowest point within window
        # (making sure not to check non-integer indices, or indices below 1 or
        #  higher than the length of the vector)
        cnt_pos <- window_lims[1]-1+which.min(values[window_lims[1]:window_lims[2]])
      }
    }
    return(best_pos)
  }
  
  cnt_pos <- 1
  ##Find first maxima
  maxima_list <- c(find_next_extrema(cnt_pos, values,
                                     width_limit = width_limit,
                                     height_limit = height_limit,
                                     looking_for = "maxima"))
  ##Find first minima
  minima_list <- c(find_next_extrema(cnt_pos, values,
                                     width_limit = width_limit,
                                     height_limit = height_limit,
                                     looking_for = "minima"))
  
  ##Check for next extrema until...
  while (TRUE) {
    #we're finding repeats
    if (any(duplicated(c(minima_list, maxima_list)))) {break}
    #or we hit the end of the values
    if (length(values) %in% c(maxima_list, minima_list)) {
      break
    }
    #Since maxima & minima must alternate, always start with furthest one 
    # we've found so far
    cnt_pos <- max(c(minima_list, maxima_list))
    #we're looking for a maxima next
    if (cnt_pos %in% minima_list) {
      maxima_list <- c(maxima_list,
                       find_next_extrema(cnt_pos, values,
                                         width_limit = width_limit,
                                         height_limit = height_limit,
                                         looking_for = "maxima"))
      #we're looking for a minima next
    } else if (cnt_pos %in% maxima_list) {
      minima_list <- c(minima_list,
                       find_next_extrema(cnt_pos, values,
                                         width_limit = width_limit,
                                         height_limit = height_limit,
                                         looking_for = "minima"))
    }
  }
  
  #Combine maxima & minima values & remove duplicates
  output <- c()
  if (return_maxima) {output <- c(output, maxima_list)}
  if (return_minima) {output <- c(output, minima_list)}
  #If remove endpoints is true, remove first or last values from return
  if (remove_endpoints) {
    if (1 %in% output) {output <- output[-which(output == 1)]}
    if (length(values) %in% output) {
      output <- output[-which(output == length(values))]}
  }
  #Remove duplicates
  output <- unique(output)
  #Order
  output <- output[order(output)]
  
  return(output)
}