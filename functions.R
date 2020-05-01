#TODO:  actually test & check that functions work
#       figure out where well names are brought in and put them
#        in the column names
#       Compare setup to other packages for similar analyses, eg growthcurver
#       Get this in package form before the quarantine ends!!!
#       in growthcurver they keep the timestamps in a column named "time",
#         I should follow a similar convention
#         This is especially useful if there are multiple "variable" type
#           columns (e.g. Temperature) that are also output and that
#           the users want to keep (because rownames can only include
#           one thing)
#       add support for providing only startcol and startrow, with automatic
#         inference that the rest of the dataframe is data

#Change smoothing to include other functions
#LOESS (which can simplify to a weighted moving average)
#LOWESS
#Spline models
#General additive model

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
                             sheet = NULL, metadata = NULL,
                             block_names = NULL,
                             infer_colnames = TRUE,
                             infer_rownames = TRUE) {
  #A function that reads blockcurves into the R environment
  
  #Inputs:  a list of filepaths relative to the current working directory,
  #           where each one is a single plate read
  #         (optional) the extension of the files,"csv", "xls", or "xlsx"
  #           (will attempt to infer extension if none is provided)
  #         (optional) the row & columns where the density data is located
  #         startrow, endrow, startcol, endcol, sheet and extension can either
  #          be vectors or lists the same length as files, or a single value
  #          that applies for all files
  #         infer_colnames - a logical for whether a header should be inferred
  #           if TRUE
  #             if startrow is provided then the row immediately above it
  #               (assuming startrow > 1) is used as a header
  #         infer_rownames - a logical for whether rownames should be inferred
  #           if TRUE
  #             if startcol is provided then the col immediately to its left
  #               (assuming startcol > 1) is used as rownames
  #         if both infer_colnames and infer_rownames are TRUE and if
  #           the top leftmost element is blank (i.e. the first row has
  #           one fewer non-blank entry than the number of columns and
  #           the first column has one fewer non-blank entry than the
  #           number of rows) then the first row will be used for colnames
  #           and the first column will be used for rownames
  #         if FALSE
  #            columns will simply be numbered with a "C" prefix
  #            rows will simply be numbered with a "R" prefix
  #         metadata is a list where each item in the list is a vector of length 2
  #           where each vector is a row,column pair for where in the blockcurve
  #           input file the metadata information is located.
  #           Metadata is returned in the 2nd element of the output nested list
  #           with the first element of Metadata being the blockcurve_name and
  #           subsequent elements being those specified by the metadata argument
  #           (by default blockcurve_name is inferred from the filename, but
  #           a list of blockcurve_names can be specifically provided if desired)
  #         
  #Outputs: a list of blockcurves named by filename
  
  #Note if sheet is NULL defaults to first sheet
  
  ##TODO
  ##      change read_blockcurves so it can handle an arbitrary number of additional
  ##      pieces of information to extract from each blockcurve file
  ##      (we can call this metadata or something like that)
  ##      these pieces of information can be passed as a list of (named) vectors
  ##      where each vector is the c(row, column) where the information should be
  ##      pulled from. Then have read_blockcurves return this information
  ##      as additional (named) entries of each entry in the list
  ##      e.g. [[1]] [1] data #1 [2] time #1 [3] temp #1 [4] Abs #1
  ##           [[2]] [1] data #2 [2] time #2 [3] temp #2 [4] Abs #2
  ##           ...
  ##      Then uninterleave should still work the same, because it will rearrange
  ##      the sub-lists (leaving the meta-data intact)
  ##      Then widen_blockcurves will have to take this metadata out and
  ##      put it as the first, second, third, etc columns before the data columns
  ##      alternatively perhaps it should be:
  ##      [[1]] [1] data #1 [2] "meta-data" [2][1] name #1 
  ##                                        [2][2] time #1 
  ##                                        [2][3] temp #1 
  ##                                        etc...
  
  if (!sum(is.null(startrow), is.null(endrow), 
          is.null(startcol), is.null(endcol)) %in% c(0, 4)) {
    stop("either all or none of startrow, endrow, startcol, and endcol must be provided")
  }
  
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
  infer_colnames <- check_diminputs(infer_colnames, "infer_colnames", files)
  infer_rownames <- check_diminputs(infer_rownames, "infer_rownames", files)
  
  if (!is.null(block_names)) {
    stopifnot(length(block_names) == length(files))
  }
  
  #Determine file extension(s)
  if (is.null(extension)) {
    require(tools)
    #Note later we make a require call for readxl if needed
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
                        USE.NAMES = FALSE)
  } else {
    extension <- check_diminputs(extension, "extension", files)
    stopifnot(extension %in% c("csv", "xls", "xlsx"))
  }
  
  if (sum(extension == "xls" | extension == "xlsx") > 0) {require(readxl)}
  
  #Create empty list for read-in blockcurves
  if (is.null(metadata)) { #there is no user-specified metadata
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = c("block_name" = "NA"))), length(files))
  } else { #there is user-specified metadata
    metadata_vector <- rep(NA, times = length(metadata)+1)
    names(metadata_vector) <- c("block_name", names(metadata))
    #Okay so the goal here is to have each blockcurve returned as an item in a big list
    #each item will itself be a named list with 2 things: "data" and "metadata"
    #data is just the dataframe (with colnames & rownames inferred or not)
    #within metadata there will at least be "name" for the blockcurve filename
    #but users can specify other metadata they would like extracted 
    # (with a named list of c(row, column) combinations)
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = metadata_vector)), length(files))
  }

  #Import data
  for (i in 1:length(files)) {
    temp_rownames <- NULL
    temp_colnames <- NULL
    ##Read file & save in temp
    if (extension[i] == "csv") {
        temp <- read.csv(files[i], colClasses = "character", 
                         header = FALSE)
    } else if (extension[i] == "xls") {
      suppressMessages(
        temp <- 
          as.data.frame(
            readxl::read_xls(files[i], col_names = FALSE, col_types = "text", 
                             sheet = sheet[i])))
    } else if (extension[i] == "xlsx") {
      suppressMessages(
        temp <- 
          as.data.frame(
            readxl::read_xlsx(files[i], col_names = FALSE, col_types = "text", 
                              sheet = sheet[i])))
    }
    
    ##Infer rownames & colnames/take subsets as needed
    if (is.null(startrow[i])) { #startrow etc are not provided
      if (infer_colnames[i] & infer_rownames[i] & temp[1, 1] == "") {
        #we're inferring colnames & rownames as the first row & col
        outputs[[i]]$data <- temp[2:nrow(temp), 2:ncol(temp)]
        temp_rownames <- temp[2:nrow(temp), 1]
        temp_colnames <- temp[1, 2:ncol(temp)]
      } else {
        #we're not inferring any rownames nor colnames
        outputs[[i]]$data <- temp
      }
    } else { #startrow etc are provided
      outputs[[i]]$data <- temp[startrow[i]:endrow[i], startcol[i]:endcol[i]]
      if (infer_colnames[i] & startrow[i] > 1) {
        temp_colnames <- temp[startrow[i]-1, startcol[i]:endcol[i]]
      }
      if (infer_rownames[i] & startcol[i] > 1) {
        temp_rownames <- temp[startrow[i]:endrow[i], startcol[i]-1]
      }
    }
    #If temp_colnames or temp_rownames haven't been inferred, number them
    if (is.null(temp_colnames)) {
      temp_colnames <- paste("C", 1:ncol(outputs[[i]]$data), sep = ".")
    }
    if (is.null(temp_rownames)) {
      if (nrow(outputs[[i]]$data) > 26) {
        stop("Automatic rownames for blockcurves with more than 26 rows is not supported")
      }
      temp_rownames <- paste("R", LETTERS[1:nrow(outputs[[i]]$data)], sep = ".")
    }
    #Assign rownames and colnames from temp_variables
    colnames(outputs[[i]]$data) <- temp_colnames
    rownames(outputs[[i]]$data) <- temp_rownames
    
    ##Add metadata
    #Add filenames to metadata
    if (!is.null(block_names)) { #block_names were provided
      outputs[[i]]$metadata["block_name"] <- block_names[i]
    } else { #block_names were not provided, infer from filename
      #infer the names from filenames, stripping off the extension from end
      # and the dot at the beginning (if any)
      outputs[[i]]$metadata["block_name"] <- 
        sub("^\\./(.*)\\.[[:alnum:]]+$", "\\1", files[i])
    }
    #Add user-specified metadata (if any)
    if (!is.null(metadata)) {
      for (j in 1:length(metadata)) {
        outputs[[i]]$metadata[j+1] <- temp[metadata[[j]][1], metadata[[j]][2]]
      }
    }
  }
  
  ##Error checking for output dataframe dimensions
  if (var(sapply(outputs, simplify = TRUE, 
                 FUN = function(x) {dim(x$data)[1]})) != 0) {
    warning("Not all blockcurves have the same number of rows of data")
  }
  if (var(sapply(outputs, simplify = TRUE,
                 FUN = function(x) {dim(x$data)[2]})) != 0) {
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

widen_blockcurves <- function(blockcurves, wellnames_sep = "_", 
                              nested_metadata = NULL) {
  #Inputs: a [list of] blockcurve[s] (optionally, named)
  #         blockcurves should be data.frames
  #         nested_metadata can be TRUE, FALSE, or NULL (if null, will infer TRUE or FALSE)
  #Outputs: a single widecurve dataframe
  
  if(class(blockcurves) != "list") {
    blockcurves <- list(blockcurves)
  }
  
  #Infer nestedness if nested_metadata is set to NULL
  if (is.null(nested_metadata)) {
    if (all(sapply(blockcurves, simplify = TRUE, FUN = class) == "data.frame")) {
      nested_metadata <- FALSE
      warning("Inferring nested_metadata to be FALSE")
    } else if (all(sapply(blockcurves, simplify = TRUE, FUN = class) == "list")) {
      nested_metadata <- TRUE
      warning("Inferring nested_metadata to be TRUE")
    } else {
      stop("Unable to infer nested_metadata, this may be because blockcurves vary in nestedness or are not data.frame's")
    }
  }
  
  #Check that all blockcurves have same dimensions
  if (nested_metadata) { #there is nested metadata
    if (var(sapply(blockcurves, simplify = TRUE, 
                   FUN = function(x) {dim(x[[1]])[1]})) != 0) {
      stop("Not all blockcurves have the same number of rows of data")
    }
    if (var(sapply(blockcurves, simplify = TRUE,
                   FUN = function(x) {dim(x[[1]])[2]})) != 0) {
      stop("Not all blockcurves have the same number of columns of data")
    }
  } else { #there is not nested metadata
    if (var(sapply(blockcurves, simplify = TRUE, 
                   FUN = function(x) {dim(x)[1]})) != 0) {
      stop("Not all blockcurves have the same number of rows of data")
    }
    if (var(sapply(blockcurves, simplify = TRUE,
                   FUN = function(x) {dim(x)[2]})) != 0) {
      stop("Not all blockcurves have the same number of columns of data")
    }
  }
    
  #convert list of dataframes to single dataframe
  #where each column is a well and each row is a plate read
  #(each column is a row-column combination from the blockcurve)
  #(each row is a single dataframe from blockcurves)
  #Adding the metadata as the first n columns
  if (nested_metadata) { #There is nested metadata
    #Reshape
    output <- data.frame(t(sapply(blockcurves, simplify = TRUE, 
                                  function(x) {c(x[[2]],
                                                 t(x[[1]]))})))
    #Assign column names
    colnames(output) <- c(names(blockcurves[[1]][[2]]),
                          paste(rep(rownames(blockcurves[[1]][[1]]),
                                    each = ncol(blockcurves[[1]][[1]])),
                                colnames(blockcurves[[1]][[1]]),
                                sep = wellnames_sep))
  } else { #There is not nested metadata
    #Reshape
    output <- data.frame(t(sapply(blockcurves, simplify = TRUE, 
                                  function(x) {c(t(x))})))
    #Assign column names
    colnames(output) <- paste(rep(rownames(blockcurves[[1]]),
                                  each = ncol(blockcurves[[1]])),
                              colnames(blockcurves[[1]]),
                              sep = wellnames_sep)
  }

  return(output)
}

import_blockcurves <- function(files, num_plates = 1,
                               wellnames_sep = "_", ...) {
  blockcurves1 <- read_blockcurves(files = files, ...)
  blockcurves2 <- uninterleave(blockcurves1, n = num_plates)
  widecurves <- rep(list(NA), num_plates)
  for (i in 1:length(blockcurves2)) {
    widecurves[[i]] <- widen_blockcurves(blockcurves2[[i]],
                                         wellnames_sep = wellnames_sep)
  }
  names(widecurves) <- paste("plate_", 1:length(widecurves), sep = "")
  return(widecurves)
}

import_widecurves <- function(files, extension = NULL, 
                              startrow = NULL, endrow = NULL, 
                              startcol = NULL, endcol = NULL,
                              infer_header = TRUE,
                              sheet = NULL, wide_names = NULL) {
  #CLEAN THIS UP LATER
  #Logic:   if infer_header TRUE
  #           if startrow is provided, header is startrow-1
  #           if startrow NULL header is row 1
  #         if infer_header FALSE
  #           columns are simply numbered well_1, well_2, etc
  #         
  #         if timecol provided
  #           timecol is timecol
  #         if timecol NULL
  #           if infer_timecol TRUE
  #             if startcol is provided, timecol is startcol-1
  #             if startcol is NULL, timecol is col 1
  #           if infer_timecol FALSE
  #             rows are numbered timepoint_1, timepoint_2, etc
 
  
  
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
  
  if (!sum(is.null(startrow), is.null(endrow), 
           is.null(startcol), is.null(endcol)) %in% c(0, 4)) {
    stop("either all or none of startrow, endrow, startcol, and endcol must be provided")
  }
  
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
  if (!is.null(timecol)) {
    timecol <- check_diminputs(timecol, "timecol", files)
  }
  infer_header <- check_diminputs(infer_header, "infer_header", files)
  infer_timecol <- check_diminputs(infer_timecol, "infer_timecol", files)
  
  #Determine file extension(s)
  if (is.null(extension)) {
    require(tools)
    #Note later we make a require call for readxl if needed
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings",
                        USE.NAMES = FALSE)
  } else {
    extension <- check_diminputs(extension, "extension", files)
    stopifnot(extension %in% c("csv", "xls", "xlsx"))
  }
  
  if (sum(extension == "xls" | extension == "xlsx") > 0) {require(readxl)}
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Import data
  for (i in 1:length(files)) {
    #Read file & save in temp
    if (extension[i] == "csv") {
       temp <- read.csv(files[i], colClasses = "character", header = FALSE)
    } else if (extension[i] == "xls") {
      suppressMessages(temp <- 
                         as.data.frame(
                           readxl::read_xls(files[i], col_names = FALSE, 
                                           col_types = "text", sheet = sheet[i])))
    } else if (extension[i] == "xlsx") {
      suppressMessages(temp <- 
                         as.data.frame(
                         readxl::read_xlsx(files[i], col_names = FALSE, 
                                           col_types = "text", sheet = sheet[i])))
    }
    
    #Infer colnames/take subsets as needed
    if (infer_header[i] == TRUE) {
      if (is.null(startrow[i])) { #startrow etc is not provided
        outputs[[i]] <- temp[2:nrow(temp), ]
        colnames(outputs[[i]]) <- temp[1, ]
      } else { #startrow is provided
        outputs[[i]] <- temp[startrow[i]:endrow[i], startcol[i]:endcol[i]]
        colnames(outputs[[i]]) <- temp[startrow[i]-1, startcol[i]:endcol[i]]
      }
    } else { #we're not to infer header
      outputs[[i]] <- temp
      colnames(outputs[[i]]) <- paste("well_", 1:ncol(temp))
    }
  }
  
  #Add filenames to widecurves
  if (!is.null(wide_names)) {
    stopifnot(length(wide_names) == length(files))
    names(outputs) <- wide_names
  } else {
    #infer the names from filenames, stripping off the extension from end
    # and the dot at the beginning (if any)
    names(outputs) <- sub("^\\./(.*)\\.[[:alnum:]]+$", "\\1", files)
  }
  
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

# smooth_data <- function(algorithm = "loess", x, y,
#                         formula = NULL, )

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

##Example uses:

# #Group data by indiv growth curves
# grp_data1 <- dplyr::group_by(spl_data1[!is.na(spl_data1$sm_od), ], 
#                      bacteria, phage, phageshock, bactshock, mediashock, Rep, Contents)
# 
# #Get OD peak height & time for each growth curve
# out_data1 <- dplyr::summarize(grp_data1, 
#                       max = analyze_curves(sm_od, Time, 
#                                            bandwidth = 20, return = "max"),
#                       maxtime = analyze_curves(sm_od, Time, 
#                                                bandwidth = 20, return = "maxtime"))


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


#### Kenichi's script: ----
#### # The script reads optical density data from  the microplate reader in the xls spreadsheets generated by evoware

#require(xlsx)

# cd into the directory with the excel spreadsheets

#plate_reader_spreads <- list.files()
# kv <- matrix(ncol=96,nrow=length(plate_reader_spreads))



# microplate_output <- list()
# 
# for (i in 1:length(plate_reader_spreads))
# 	{
# 	# By evoware default, the spreadsheet format begins the microplate reader on row #27, col #B:M (note there are 12 columns)
# 	# with shaking by incubator:
# 	z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=27, endRow=35, colIndex=2:14)
# 	# without shaking by incubator:
# 	#z <- read.xlsx(plate_reader_spreads[i],sheetIndex=1, startRow=24, endRow=32, colIndex=2:14)
# 	microplate_output[[i]] <- z
# 	}
# 
# # To visualize dynamics in a given well:
# retrieve_well_timestep <- function(step,x,y)
# 	{
# 	return(microplate_output[[step]][x,y])
# 	}
# 
# # TODO: fix this to use mapply
# visualize_dynamics <- function(x,y)
# 	{
# 	retrieve_this_well <- function(step)
# 		{
# 		return(retrieve_well_timestep(step,x,y))
# 		}
# 		
# 	od_measures <- sapply(1:length(plate_reader_spreads), retrieve_this_well)
# 	#od_measures <- sapply(1:281, retrieve_this_well)
# 	plot(od_measures,xlab="measurement point", ylab="OD",t="l",ylim=range(microplate_output))
# 	}
# 
# dev.new(height=8,width=10)
# par(mfrow=c(8,12),mar=c(1,2,1,1))
# for (i in 1:8)
# 	for (j in 1:12)
# 		visualize_dynamics(i,j)
# 
# matrixAll <- matrix(nrow=length(plate_reader_spreads), ncol=96)
# for (i in 1:length(plate_reader_spreads))
#  {
#  matrixAll[i,] <- c(as.matrix(microplate_output[[i]]))
#  }


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

##Example workflow

# #Smooth data
# gc_data$sm_loess <- NA
# for (my_well in unique(gc_data$uniq_well)) {
#   my_rows <- which(gc_data$uniq_well == my_well)
#   #Smooth with loess
#   gc_data$sm_loess[my_rows] <- loess(cfu_ml ~ Time_s, span = 0.4,
#                                      data = gc_data[my_rows, ])$fitted
# }
# 
# #Calculate growth per hour from loess curve
# gc_data$deriv_sm_loess <- calc_deriv(gc_data$sm_loess,
#                                      subset_by = gc_data$uniq_well,
#                                      time = gc_data$Time_s,
#                                      time_normalize = 3600)
# 
# #Calculate per capita growth per hour from loess curve
# gc_data$percap_deriv_sm_loess <- calc_deriv(gc_data$sm_loess,
#                                             percapita = TRUE,
#                                             subset_by = gc_data$uniq_well,
#                                             time = gc_data$Time_s,
#                                             time_normalize = 3600)

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