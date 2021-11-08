#General utilities ----

#' Check dimension of inputs
#' 
#' Check if values are a vector of given length, if not coerce to be so
#' Returns error messages using appropriate names
#' 
#' @param input Values to be used
#' @param input_name Name of the input used for error messages
#' @param needed_len Desired length of output vector
#' @param needed_name What the desired length corresponds to (e.g. number of files)
#' @return The values of \code{input} coerced to a vector of length \code{needed_len}
#' 
checkdim_inputs <- function(input, input_name, needed_len,
                            needed_name = "the number of files") {
  if (length(input) != needed_len) {
    if(length(input) != 1) {
      stop(paste("More than one", input_name, "value is supplied, but the number
                   of", input_name, "values is not equal to", needed_name))
    } else {
      return(rep(input, needed_len))
    }
  } else {
    return(input)
  }
}

#' Uninterleave list
#' 
#' Takes a list that is actually interleaved elements from multiple sources
#' and uninterleaves them into the separate sources
#' For instance, a list of blockmeasures that actually corresponds to two
#' different plates can be split into two lists, each of the blockmeasures
#' corresponding to a single plate
#' 
#' @param interleaved_list A list of R objects
#' @param n How many output sub lists there should be (i.e. how many groups
#'          the interleaved list should be divided into)
#' @return A list of lists of R objects
#' 
uninterleave <- function(interleaved_list, n, ...) {
  # Note that the ... is just so that import_blockmeasures can call
  # it with generic passing of arguments
  
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

#TODO: move name inference logic into this function to make testing easier
infer_names <- function(startrow = NULL, endrow = NULL, 
                        startcol = NULL, endcol = NULL,
                        infer_colnames = TRUE,
                        infer_rownames = TRUE) {
  
  #TODO
}

#Importing block-shaped ----

#' Read blockmeasures
#' 
#' A function that reads block measures into the R environment
#' 
#' @param files A vector of filepaths relative to the current working directory
#'              where each filepath is a single plate read
#' @param extension (optional) the extension of the files:
#'                  "csv", "xls", or "xlsx"
#'                  If none provided, \code{read_blocks} will infer file extension from
#'                  provided filenames
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the measures data are in \code{files},
#'                 can be a vector or list the same length as \code{files}, or
#'                 a single value that applies to all \code{files}
#'                 If not provided \code{read_blocks} will try to infer it
#' @param sheet (optional) If data is in .xls or .xlsx files, which sheet it 
#'                 is located on. Defaults to the first sheet if not specified
#' @param metadata (optional) non-spectrophotometric data that should be associated
#'                 with each read blockmeasures. A list where each item in the
#'                 list is a vector of length 2. Each vector should provide the
#'                 row and column where the metadata is located in the blockmeasures
#'                 input file. If the list is named those names will be inherited
#'                 to the output metadata.
#' @param block_names (optional) vector of names corresponding to each plate
#'                 in \code{files}. If not provided, block_names are inferred
#'                 from the filenames
#' @param infer_colnames,infer_rownames Booleans for whether column names
#'                 and rownames should be inferred.
#'                 If TRUE and startrow or startcol are specified, respectively, 
#'                 column or row names will be inferred as the previous column
#'                 or row, respectively.
#'                 If both \code{infer_colnames} and \code{infer_rownames} are
#'                 TRUE and startrow or startcol are not specified,
#'                 the first row and first column will be taken as column names
#'                 and row names, respectively, if the row 1 column 1 cell is 
#'                 empty
#'                 If FALSE, rows will be numbered with an "R" prefix and columns
#'                 will be numbered with a "C" prefix
#' @return A list where each entry is a list containing the block measures data
#'         followed by the block_names (or filenames, if block_names is not 
#'         provided) and any specified metadata.
#'         
read_blocks <- function(files, extension = NULL, 
                             startrow = NULL, endrow = NULL, 
                             startcol = NULL, endcol = NULL,
                             sheet = NULL, metadata = NULL,
                             block_names = NULL,
                             infer_colnames = TRUE,
                             infer_rownames = TRUE,
                             ...) {
  #         Note that the ... is just so that this function can be called
  #          by other functions with generic passing of arguments
  #         TODO: check if this is actually necessary
  #         
  #         For metadata, read_blocks  can handle an arbitrary number of additional
  #         pieces of information to extract from each blockcurve file as metadata
  #         These pieces of information are specified as a list of (named) vectors
  #         where each vector is the c(row, column) where the information is to be
  #         pulled from in the input files.
  #         
  #         This metadata is returned as the second list element of each 
  #         blockcurve, e.g.:
  #         [[1]] [1] "data" #1 [2] "metadata"  [2][1] name #1
  #                                             [2][2] date-time #1
  #                                             [2][3] temp #1
  #         [[2]] [1] "data" #2 [2] "metadata"  [2][1] name #2
  #                                             [2][2] date-time #2
  #                                             [2][3] temp #2
  #         ...
  #         
  #         Calling uninterleave on such an outputted list will still work
  #         because it will operate on the highest level entries of the list
  #         (the [[1]] [[2]] level items), leaving the meta-data intact
  #         
  #         Widen_blocks integrates this metadata into the dataframe during
  #         the pivot_wider process

  if (is.null(startrow)) {
    startrow <- rep(NA, length(files))
  } else {
    startrow <- checkdim_inputs(startrow, "startrow", length(files))
  }
  if (is.null(endrow)) {
    endrow <- rep(NA, length(files))
  } else {
    endrow <- checkdim_inputs(endrow, "endrow", length(files))
  }
  if (is.null(startcol)) {
    startcol <- rep(NA, length(files))
  } else {
    startcol <- checkdim_inputs(startcol, "startcol", length(files))
  }
  if (is.null(endcol)) {
    endcol <- rep(NA, length(files))
  } else {
    endcol <- checkdim_inputs(endcol, "endcol", length(files))
  }
  if (!is.null(sheet)) {
    sheet <- checkdim_inputs(sheet, "sheet", length(files))
  }
  infer_colnames <- checkdim_inputs(infer_colnames, "infer_colnames", length(files))
  infer_rownames <- checkdim_inputs(infer_rownames, "infer_rownames", length(files))
  
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
    extension <- checkdim_inputs(extension, "extension", length(files))
  }
  stopifnot(extension %in% c("csv", "xls", "xlsx"))
  
  if (any(extension == "xls" | extension == "xlsx")) {require(readxl)}
  
  #Create empty list for read-in block measures
  if (is.null(metadata)) { #there is no user-specified metadata
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = c("block_name" = "NA"))), 
                   length(files))
  } else { #there is user-specified metadata
    metadata_vector <- rep(NA, times = length(metadata)+1)
    names(metadata_vector) <- c("block_name", names(metadata))
    #Okay so the goal here is to have each block measures returned as an item in a big list
    #each item will itself be a named list with 2 things: "data" and "metadata"
    #data is just the dataframe (with colnames & rownames inferred or not)
    #within metadata there will at least be "name" for the block measures filename
    #but users can specify other metadata they would like extracted 
    # (with a named list of c(row, column) combinations)
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = metadata_vector)), 
                   length(files))
  }

  #Import data
  for (i in 1:length(files)) {
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
    
    #Infer endrow/endcol if they're not provided to be the last row/col
    if (is.na(endrow[i])) {endrow[i] <- nrow(temp)}
    if (is.na(endcol[i])) {endcol[i] <- ncol(temp)}
    
    #Inferring startrow/startcol & rownames/colnames is complex:
    
    temp_startcol <- 0
    temp_startrow <- 0
    
    #If infer_colnames is true...
    if (infer_colnames[i] == TRUE) {
      #...and startrow is provided, colnames is the row before startrow
      if (!is.na(startrow[i])) {
        colnames_row <- startrow[i]-1
        temp_startrow <- startrow[i]
      #...and startrow is not provided
      } else { 
        if (!is.na(startcol[i])) {
          #if startcol is provided
          if (temp[1, startcol[i]-1] == "") {
            #colnames row is 1 if [1, startcol-1] is empty, 
            colnames_row <- 1
            temp_startrow <- 2
          } else {
            #otherwise colnames will be auto
            colnames_row <- 0
            temp_startrow <- 1
          }
        } else {
          #if startcol is not provided 
          # (AKA niether startrow nor startcol are provided)
          # colnames row is 1 if [1,1] is empty
          if (temp[1,1] == "") {
            colnames_row <- 1
            temp_startrow <- 2
          } else {
          #otherwise colnames will be auto
            colnames_row <- 0
            temp_startrow <- 1
          }
        }
      }
    } else {
      #If infer_colnames is false then we auto-generate colnames
      colnames_row <- 0
      if (is.na(startrow[i])) {
        #If startrow is not provided, start on row 1
        temp_startrow <- 1
      } else {
        temp_startrow <- startrow[i]
      }
    }
    
    #If infer_rownames is true...
    if (infer_rownames[i] == TRUE) {
      #...and startcol is provided, rownames is the col before startcol
      if (!is.na(startcol[i])) {
        rownames_col <- startcol[i]-1
        temp_startcol <- startcol[i]
      #...and startcol is not provided
      } else {
        if (!is.na(startrow[i])) {
        #if startrow is provided
          if (temp[startrow[i]-1, 1] == "") {
            #rownames col is 1 if [startrow-1, 1] is empty, 
            # otherwise rownames will be auto
            rownames_col <- 1
            temp_startcol <- 2
          } else {
          #otherwise rownames will be auto (no code needed)
            rownames_col <- 0
            temp_startcol <- 1
          }
        } else {
        #if startrow is not provided
        # (AKA niether startrow nor startcol are provided)
        # colnames row is 1 if [1,1] is empty, otherwise colnames will be auto
          if (temp[1,1] == "") {
            rownames_col <- 1
            temp_startcol <- 2
          } else {
            rownames_col <- 0
            temp_startcol <- 1
          }
        }
      }
    } else {
      #If infer_rownames is false then we auto-generate rownames
      rownames_col <- 0
      if(is.na(startcol[i])) {
        #If startcol is not provided, start on col 1
        temp_startcol <- 1
      } else {temp_startcol <- startcol[i]}
    }
    
    ##For debugging
    # paste(infer_rownames[i], infer_colnames[i], startrow[i], startcol[i],
    #       temp_startrow, temp_startcol, colnames_row, rownames_col, sep = "|")
    
    #Save inferred info about startrow and startcol
    if (temp_startrow == 0) {stop("temp_startrow = 0, this shouldn't happen")
      } else {startrow[i] <- temp_startrow}
    if (temp_startcol == 0) {stop("temp_startcol = 0, this shouldn't happen")
    } else {startcol[i] <- temp_startcol}
    
    #If temp_colnames or temp_rownames haven't been inferred, number them
    if (colnames_row == 0) {
      temp_colnames <- paste("C", 1:ncol(outputs[[i]]$data), sep = ".")
    } else {
      temp_colnames <- temp[colnames_row, startcol[i]:endcol[i]]
    }
    if (rownames_col == 0) {
      if (length(startrow[i]:startcol[i]) > 26) {
        stop("Automatic rownames for blockmeasures with more than 26 rows is not supported")
      }
      temp_rownames <- paste("R", LETTERS[1:nrow(outputs[[i]]$data)], sep = ".")
    } else {
      temp_rownames <- temp[startrow[i]:endrow[i], rownames_col]
    }
    
    #Save information to outputs
    outputs[[i]]$data <- temp[startrow[i]:endrow[i],startcol[i]:endcol[i]]
    
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
        sub("^\\.?/?(.*)\\.[[:alnum:]]+$", "\\1", files[i])
    }
    #Add user-specified metadata (if any)
    if (!is.null(metadata)) {
      for (j in 1:length(metadata)) {
        outputs[[i]]$metadata[j+1] <- temp[metadata[[j]][1], metadata[[j]][2]]
      }
    }
  }
  
  ##Error checking for output dataframe dimensions
  if (length(outputs) > 1 &
      var(sapply(outputs, simplify = TRUE, 
                 FUN = function(x) {dim(x$data)[1]})) != 0) {
    warning("Not all blockmeasures have the same number of rows of data")
  }
  if (length(outputs) > 1 &
      var(sapply(outputs, simplify = TRUE,
                 FUN = function(x) {dim(x$data)[2]})) != 0) {
    warning("Not all blockmeasures have the same number of columns of data")
  }
  
  return(outputs)
}

#' Widen blockmeasures
#' 
#' Takes blockmeasures and returns them in a widemeasure format
#' 
#' @param blockmeasures Blockmeasures, either a single data.frame or a list of
#'                      data.frames
#' @param wellnames_sep String to use as separator for well names between 
#'                      rowname and column name
#' @param nested_metadata A Boolean indicating the existence of nested metadata
#'                        in the \code{blockmeasures} list, e.g. as is typically
#'                        output by \code{read_blocks}. If NULL, will attempt to
#'                        infer existence of nested metadata
#' @return A single widemeasures data.frame
#' 
widen_blocks <- function(blockmeasures, wellnames_sep = "_", 
                              nested_metadata = NULL, ...) {
  #         Note that the ... is just so that import_blockmeasures can call
  #         it with generic passing of arguments
  
  if(class(blockmeasures) != "list") {
    blockmeasures <- list(blockmeasures)
  }
  
  #Infer nestedness if nested_metadata is set to NULL
  if (is.null(nested_metadata)) {
    if (all(sapply(blockmeasures, simplify = TRUE, FUN = class) == "data.frame")) {
      nested_metadata <- FALSE
      warning("Inferring nested_metadata to be FALSE")
    } else if (all(sapply(blockmeasures, simplify = TRUE, FUN = class) == "list")) {
      nested_metadata <- TRUE
      warning("Inferring nested_metadata to be TRUE")
    } else {
      stop("Unable to infer nested_metadata, this may be because blockmeasures vary in nestedness or are not data.frame's")
    }
  }
  
  #Check that all blockmeasures have same dimensions
  if (length(blockmeasures) > 1) {
    if (nested_metadata) { #there is nested metadata
      if (var(sapply(blockmeasures, simplify = TRUE, 
                     FUN = function(x) {dim(x[[1]])[1]})) != 0) {
        stop("Not all blockmeasures have the same number of rows of data")
      }
      if (var(sapply(blockmeasures, simplify = TRUE,
                     FUN = function(x) {dim(x[[1]])[2]})) != 0) {
        stop("Not all blockmeasures have the same number of columns of data")
      }
    } else { #there is not nested metadata
      if (var(sapply(blockmeasures, simplify = TRUE, 
                     FUN = function(x) {dim(x)[1]})) != 0) {
        stop("Not all blockmeasures have the same number of rows of data")
      }
      if (var(sapply(blockmeasures, simplify = TRUE,
                     FUN = function(x) {dim(x)[2]})) != 0) {
        stop("Not all blockmeasures have the same number of columns of data")
      }
    }
  }
    
  #convert list of dataframes to single dataframe
  #where each column is a well and each row is a plate read
  #(each column is a row-column combination from the blockcurve)
  #(each row is a single dataframe from blockmeasures)
  #Adding the metadata as the first n columns
  #
  #
  #At some point I may need to re-implement this avoiding use of
  # t() (because t calls as.matrix which can add white space to numeric values
  # See convert_plate_to_column in plater R package
  # If so, this code may be useful
  #   output <- sapply(blockmeasures, simplify = TRUE,
              # function(x) {c(x[[2]],
              #                unlist(lapply(X = 1:nrow(x[[1]]),
              #                              function(i) {unname(x[[1]][i, ])}))
              # )
              # }
              # )
  
  if (nested_metadata) { #There is nested metadata
    #Reshape
    output <- data.frame(t(sapply(blockmeasures, simplify = TRUE, 
                                  function(x) {c(x[[2]],
                                                 t(x[[1]]))})),
                         stringsAsFactors = FALSE)
    #Assign column names
    colnames(output) <- c(names(blockmeasures[[1]][[2]]),
                          paste(rep(rownames(blockmeasures[[1]][[1]]),
                                    each = ncol(blockmeasures[[1]][[1]])),
                                colnames(blockmeasures[[1]][[1]]),
                                sep = wellnames_sep))
  } else { #There is not nested metadata
    #Reshape
    output <- data.frame(t(sapply(blockmeasures, simplify = TRUE, 
                                  function(x) {c(t(x))})),
                         stringsAsFactors = FALSE)
    #Assign column names
    colnames(output) <- paste(rep(rownames(blockmeasures[[1]]),
                                  each = ncol(blockmeasures[[1]])),
                              colnames(blockmeasures[[1]]),
                              sep = wellnames_sep)
  }

  return(output)
}

#Get blockmeasures ----

#' Import blockmeasures
#' 
#' Function to import blockmeasures from files and return widemeasures
#' This function acts as a wrapper to call read_blocks, uninterleave, 
#' then widen_blocks in one go
#' 
#' @param files Vector of filenames (as strings), each of which is a blockmeasures
#'              formatted file. Inputs can be .csv, .xls, or .xlsx
#' @param num_plates Number of plates. If multiple plates uninterleave will be
#'                   used to separate blockmeasures into those plates accordingly
#' @param plate_names (optional) Names to put onto the plates when output
#' @param ... Other arguments to pass to \code{read_blocks}, \code{uninterleave},
#'            or \code{widen_blocks}
#'            
import_blockmeasures <- function(files, num_plates = 1, 
                                 plate_names = NULL,
                                 ...) {
  blockmeasures <- uninterleave(read_blocks(files = files, ...),
                                n = num_plates, ...)
  widemeasures <- rep(list(NA), num_plates)
  for (i in 1:length(blockmeasures)) {
    widemeasures[[i]] <- widen_blocks(blockmeasures[[i]],
                                         wellnames_sep = wellnames_sep)
  }
  if (is.null(plate_names)) { #no plate_names provided
    names(widemeasures) <- paste("plate_", 1:length(widemeasures), sep = "")
  } else { #plate_names are provided
    names(widemeasures) <- plate_names
  }
  
  if (num_plates == 1) {
    return(widemeasures[[1]])
  } else {
    return(widemeasures)
  }
}

#Get widemeasures ----

#' Import widemeasures
#' 
#' A function that imports widemeasures in files into the R environment
#' 
#' @details
#' startrow, endrow, startcol, endcol, timecol, sheet and extension 
#' can either be vectors or lists the same length as files, 
#' or a single value that applies for all files
#' 
#' @param files A vector of filepaths (relative to current working directory)
#'              where each one is a widemeasures set of data
#' @param extension (optional) the extension of the files,"csv", "xls", or "xlsx"
#'                  (will attempt to infer extension if none is provided)
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where
#'                  the data is located. If none provided assumes the entire
#'                  file is data
#' @param header Boolean for whether there is a header to the data. If FALSE
#'               columns are simple numbered. If TRUE is the row above
#'               \code{startrow} (if startrow is specified) or the first row
#'               of the input files (if startrow is not specified)
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#'              sheet
#' @param wide_names Names to give the widemeasures read in. By default uses the
#'                   file names if not specified
#' @return A list of widemeasures named by filename
#'
import_widemeasures <- function(files, extension = NULL, 
                              startrow = NULL, endrow = NULL, 
                              startcol = NULL, endcol = NULL,
                              header = TRUE,
                              sheet = NULL, 
                              wide_names = NULL) {
  #CLEAN THIS UP LATER
  #Logic 2.0: if header TRUE
  #             if startrow provided, header is startrow-1
  #             if startrow not provided, header is 1
  #           if header FALSE
  #             columns numbered V1...Vn
  #
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
 
  
  
  #Inputs:
  #         (optional) the column where timestamp info is located
  #           (if none provided by default the column immediately to the left 
  #           of startcol is assumed to be the time column)

  
  if (!sum(is.null(startrow), is.null(endrow), 
           is.null(startcol), is.null(endcol)) %in% c(0, 4)) {
    stop("either all or none of startrow, endrow, startcol, and endcol must be provided")
  }
  
  if (!is.null(startrow)) {
    startrow <- checkdim_inputs(startrow, "startrow", length(files))
  }
  if (!is.null(endrow)) { 
    endrow <- checkdim_inputs(endrow, "endrow", length(files))
  }
  if (!is.null(startcol)) {
    startcol <- checkdim_inputs(startcol, "startcol", length(files))
  }
  if (!is.null(endcol)) {
    endcol <- checkdim_inputs(endcol, "endcol", length(files))
  }
  if (!is.null(sheet)) {
    sheet <- checkdim_inputs(sheet, "sheet", length(files))
  }
  
  if (!is.null(startrow) & header == TRUE & startrow <= 1) {
    warning("startrow <= 1 but header is TRUE, treating header as FALSE")
    header <- FALSE
  }
  
  #Determine file extension(s)
  if (is.null(extension)) {
    require(tools)
    #Note later we make a require call for readxl if needed
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings",
                        USE.NAMES = FALSE)
  } else {
    extension <- checkdim_inputs(extension, "extension", length(files))
    stopifnot(all(extension %in% c("csv", "xls", "xlsx")))
  }
  
  if (any(extension == "xls" | extension == "xlsx")) {require(readxl)}
  
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
    if (header == TRUE) {
      if (is.null(startrow[i])) { #startrow etc is not provided
        outputs[[i]] <- temp[2:nrow(temp), ]
        colnames(outputs[[i]]) <- temp[1, ]
      } else { #startrow etc is provided
        outputs[[i]] <- temp[startrow:endrow, startcol:endcol]
        colnames(outputs[[i]]) <- temp[startrow-1, startcol:endcol]
      }
    } else { #header is false
      if (is.null(startrow[i])) { #startrow etc is not provided
        outputs[[i]] <- temp
      } else { #startrow etc is provided
        outputs[[i]] <- temp[startrow:endrow, startcol:endcol]
      }
      colnames(outputs[[i]]) <- paste("V", 1:ncol(temp), sep = "")
    }
  }
  
  #Add filenames to widemeasures
  if (!is.null(wide_names)) {
    stopifnot(length(wide_names) == length(files))
    names(outputs) <- wide_names
  } else {
    #infer the names from filenames, stripping off the extension from end
    # and the dot at the beginning (if any)
    names(outputs) <- sub("^\\.?/?(.*)\\.[[:alnum:]]+$", "\\1", files)
  }
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}

#tidy widemeasures ----

pivot_wide_longer <- function(widemeasures, 
                              data_cols = NA,
                              id_cols = NA,
                              names_to = "Well",
                              values_to = "Measurements",
                              ...) {
  #Basically just a wrapper for tidyr:pivot_longer
  # so that we can pivot_longer a whole list of widemeasures
  #Data_cols or id_cols can be a single vector 
  # (which will be used for all widemeasures)
  # Or can be a list of vectors, with each vector
  # corresponding to the same index widemeasure dataframe
  # Entries that are NA in the list will be not used
  #names_to and values_to can be vectors, same length as widemeasures
  # Note that if neither data_cols nor id_cols are provided, user must
  #   provide arguments to pivot_longer via ... for at least cols argument
  #   And if so, same rules are applied to all widemeasures
  
  #Reformat cols inputs as needed
  if (!is.list(id_cols)) {
    id_cols <- list(id_cols)
  }
  id_cols <- checkdim_inputs(id_cols, "id_cols", length(widemeasures))
  
  if (!is.list(data_cols)) {
    data_cols <- list(data_cols)
  }
  data_cols <- checkdim_inputs(data_cols, "data_cols", length(widemeasures))
  
  
  #Check cols inputs
  if (any(!is.na(data_cols) & !is.na(id_cols))) {
    warning("Cannot provide both data_cols and id_cols for a given widemeasures, using data_cols only")
  }
  
  require(tidyr)
  
  if (is.data.frame(widemeasures)) {
    widemeasures <- list(widemeasures)
  }
  names_to <- checkdim_inputs(names_to, "names_to", length(widemeasures))
  values_to <- checkdim_inputs(values_to, "values_to", length(widemeasures))
  
  #Empty list for outputs
  outputs <- rep(list(NA), length(widemeasures))
  for (i in 1:length(widemeasures)) {
    if (!is.na(data_cols[i])) { #user specified which columns are data columns
      outputs[[i]] <- as.data.frame(
        tidyr::pivot_longer(widemeasures[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = data_cols[[i]],
                            ...))
    } else if (!is.na(id_cols[i])) { #user specified which columns are id columns
      outputs[[i]] <- as.data.frame(
        tidyr::pivot_longer(widemeasures[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = which(!colnames(widemeasures[[i]]) %in% id_cols[[i]]),
                            ...))
    } else { #User must be providing their own arguments to pivot_longer
      outputs[[i]] <- as.data.frame(tidyr::pivot_longer(widemeasures[[i]],
                                                        names_to = names_to[i],
                                                        values_to = values_to[i],
                                                        ...))
    }
  }
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}

#Get designs ----

make_tidydesign <- function(nrows = NULL, ncols = NULL,
                        block_row_names = NULL, block_col_names = NULL,
                        wellnames_sep = "_", wellnames_colname = "Well",
                        pattern_split = "", ...) {
  #This is a function to easily input experimental design elements
  #Either nrows or block_row_names must be provided
  #Either ncols or block_col_names must be provided
  #For formatting ... arguments, each must be a list with four elements:
  # the values, 
  # the rows the pattern should be applied to,
  # the columns the pattern should be applied to, 
  # the pattern itself (in string)
  #   numbers refer to the indexes in the values vector
  #   0's refer to NA
  #   the pattern will be split using pattern_split
  #     (defaults to every character)
  # whether the pattern should be filled by row (Boolean)
  #For example:
  # my_example <- make_tidydesign(nrows = 8, ncols = 12,
  #         design_element_name = list(c("Value1", "Value2", "Value3"),
  #                           rowstart:rowend, colstart:colend,
  #                           "111222333000", TRUE)
  #To make it easier to pass arguments, use make_designpattern:
  # my_example <- make_tidydesign(nrows = 8, ncols = 12,
  #       design_element_name = make_designpattern(values = c("L", "G", "C"),
  #                                                 rows = 2:7, cols = 2:11,
  #                                                 pattern = "11223300",
  #                                                 byrow = TRUE))
  
  
  #Do we need to include a plate_name argument?
    #(old comment) the plates have to be identified uniquely
  
  #(old comment on other ways inputs could be taken)
  #       Vectors can be formatted in several ways:
  #         they can simply be numbers of the wells
  #         they can be row-column based
  #         they can be pattern based (within limits)
  #   example:
  #   make_layout("Treatment" = list("Local" = 1:5, "Global" = 6:10,
  #                                   "Control" = 11:15),
  #               "Isolate" = list("A" = c(1, 6, 11),
  #                                 "B" = c(2, 7, 12), etc))
  #               "Rep" = list("1" = 
  
  #Check inputs
  stopifnot(!(is.null(nrows) & is.null(block_row_names)),
            !(is.null(ncols) & is.null(block_col_names)))
  if (is.null(block_row_names)) {block_row_names <- paste("R", 1:nrows, sep = ".")}
  if (is.null(block_col_names)) {block_col_names <- paste("C", 1:ncols, sep = ".")}
  if (is.null(nrows)) {nrows <- length(block_row_names)}
  if (is.null(ncols)) {ncols <- length(block_col_names)}
  
  dot_args <- list(...)
  
  #Make base output dataframe
  output <- as.data.frame(matrix(NA, nrow = nrows*ncols, 
                                 ncol = 1+length(unique(names(dot_args)))))
  output[,1] <- paste(rep(block_row_names, each = ncols),
                      block_col_names, sep = wellnames_sep)
  colnames(output)[1] <- wellnames_colname
  
  #Note dot_args structure
  #dot_args[[i]] = list(values = c("A", "B", "C"),
  #                     rows = rowstart:rowend, cols = colstart:colend
  #                     pattern = "111222333000", byrow = TRUE)
  
  #Loop through input arguments & fill into output dataframe
  for (i in 1:length(dot_args)) {
    pattern_list <- as.numeric(strsplit(dot_args[[i]][[4]], 
                                        split = pattern_split)[[1]])
    
    if (((length(dot_args[[i]][[2]])*length(dot_args[[i]][[3]])) %% 
         length(pattern_list)) != 0) {
      warning(paste("Total number of wells is not a multiple of pattern length for",
                    names(dot_args)[i]))
    }
    
    #Byrow is optional, if not provided default is byrow = TRUE
    if (length(dot_args[[i]]) < 5) {
      dot_args[[i]][[5]] <- TRUE
    }
    
    #0 in pattern is NA
    pattern_list[pattern_list==0] <- NA
    
    #Create list of values following pattern (which is replicated as needed
    # to reach the necessary length)
    vals_list <- dot_args[[i]][[1]][
      rep(pattern_list, length.out = (length(dot_args[[i]][[2]])*
                                        length(dot_args[[i]][[3]])))]
    
    #Fill values into "blockcurve"
    block_out <- matrix(NA, nrow = nrows, ncol = ncols)
    block_out[dot_args[[i]][[2]], dot_args[[i]][[3]]] <-
      matrix(vals_list, 
             nrow = length(dot_args[[i]][[2]]), 
             ncol = length(dot_args[[i]][[3]]),
             byrow = dot_args[[i]][[5]])
    block_mlt <- c(t(block_out))
    #Put values into output dataframe
    mycol <- match(names(dot_args)[i], unique(names(dot_args)))+1
    output[which(!is.na(block_mlt)), mycol] <- block_mlt[which(!is.na(block_mlt))]
    colnames(output)[mycol] <- names(dot_args)[i]
  }
  return(output)
}

make_designpattern <- function(values, rows, cols, pattern, byrow = TRUE) {
  #This function simply makes it easier to use make_tidydesign
  #For example:
  # my_example <- make_tidydesign(nrows = 8, ncols = 12,
  #       design_element_name = make_designpattern(values = c("L", "G", "C"),
  #                                                 rows = 2:7, cols = 2:11,
  #                                                 pattern = "11223300",
  #                                                 byrow = TRUE))
  stopifnot(is.vector(values), is.vector(rows), is.vector(cols),
            is.character(pattern), is.logical(byrow))
  
  return(list(values, rows, cols, pattern, byrow))
}
  
block_tidydesign <- function(tidydesign, collapse = NULL,
                                wellnames_sep = "_", wellnames_colname = "Well") {
  #This function is primarily so that users can use make_tidydesign
  # to make designs then output them to csv for inclusion in
  # lab notebooks, etc.
  #If collapse = NULL, each design column will be put into it's own block
  #If collapse = some character, all design columns will be put into one block
  # with that character as the seperator
  #Either way the function returns a list of blockdesign matrices
  # (although if collapse is not NULL, the list is of length 1)
  
  #Get rownames & colnames from well column
  rownames <- sapply(strsplit(tidydesign[, wellnames_colname],
                              split = wellnames_sep),
                     simplify = TRUE,
                     FUN = function(x) {x[1]})
  colnames <- sapply(strsplit(tidydesign[, wellnames_colname],
                              split = wellnames_sep),
                     simplify = TRUE,
                     FUN = function(x) {x[2]})
  #Make empty output
  output <- rep(list(matrix(NA, nrow = length(unique(rownames)),
                            ncol = length(unique(colnames)))), 
                length(which(colnames(tidydesign) != wellnames_colname)))
  
  #Iterate through each design element
  i <- 1
  for (col in which(colnames(tidydesign) != wellnames_colname)) {
    #Assign row and column names
    rownames(output[[i]]) <- unique(rownames)
    colnames(output[[i]]) <- unique(colnames)
    #Fill data into appropriate row,column in output
    # (spot is found by matching row/col name to row/col from well column
    #  in tidydesign input)
    output[[i]][match(rownames, rownames(output[[i]])) + 
             (match(colnames, colnames(output[[i]]))-1)*nrow(output[[i]])] <-
      tidydesign[, col]
    i <- i+1
  }
  
  #Collapse (if requested)
  if (!is.null(collapse)) {
    output <- list(matrix(do.call("paste", c(output, sep = collapse)),
                     nrow = length(unique(rownames)),
                     ncol = length(unique(colnames))))
    rownames(output[[1]]) <- unique(rownames)
    colnames(output[[1]]) <- unique(colnames)
    names(output) <- paste(
      colnames(tidydesign)[which(colnames(tidydesign) != wellnames_colname)],
      collapse = collapse)
  }
  
  return(output)
}

write_blockdesign <- function(designs, file, ...) {
  #Basically just a wrapper for write.csv when handed a list of matrices/dataframes
  #Also puts the names of the designs in 1,1 cell (as is the case for block designs
  if (is.data.frame(designs)) {
    write.csv(x = designs, file = file, ...)
  } else if (is.list(designs)) {
    for (i in 1:length(designs)) {
      tmp <- cbind(data.frame(row.names(designs[[i]])),
                        designs[[i]])
      colnames(tmp)[1] <- names(designs)[i]
      write.csv(x = tmp, file = file, row.names = FALSE, ...)
    }   
  }
}

import_blockdesign <- function(files,
                               startrow = 2, startcol = 2,
                               metadata = list("design_element" = c(1, 1)),
                               field_sep = "_",
                               id_cols = c("block_name", "design_element"),
                               ...) {
  #Takes in a list of files, each of which includes a layout in it
  # then cleans up the layout information in those files
  # by splitting it
  #Outputs that layout information in a tidy format
  #
  #By default assumes that the design element name is in row 1, column 1
  #E.g. a design block file looks like this:
  #     Treatment   1   2   3   4   5   6   7   ...
  #     A           L   G   C   L   G   C   L   ...
  #     B           L   G   C   L   G   C   L   ...
  #     C           L   G   C   L   G   C   L   ...
  #     ...
  # Where A, B, C... are the row names; 1, 2, 3... are the column names
  #   and L, G, and C are the values for the treatment element
  # If this is not true, set metadata = NULL and adjust startrow/startcol
  
  ##General steps
  ##1. use read_blocks to get design into R
  ##2. use widen_blocks to get design into wide format
  ##3. use pivot wides longer to get design into tidy format
  ##2. use split block to split design elements
  
  blockdesigns <- read_blocks(files = files,
                              metadata = metadata,
                              startrow = startrow, startcol = startcol,
                              ...)
  widedesigns <- widen_blocks(blockdesigns, ...)
  
  
  
  
  #What to do if design_element is not supplied?
  tidydesigns <- pivot_wide_longer(widedesigns, id_cols = id_cols,
                                  values_to = widedesigns$design_element[1],
                                  ...)
  
                                        
                                        
                                        
        split_blockdesign()
  #For reference, old version (possibly)
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
  
  
  
}

split_blockdesign <- function() {
  #This function will be called after read_blocks, widen_blocks,
  # pivot_wide_longer to split up multiple fields that exist
  # in the design when it's put in as a block in csv
}


import_tidydesign <- function() {
  
}


merge_tidydesign_tidymeasures <- function() {
  #This should also include the capability to merge multiple design
  # objects (e.g. if dift design elements were written in block form
  # in different csv files)
  
}


#Merge layout & data
layout_data_merge <- function(layout, data) {
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

#Preprocess ----

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

##TODO: this should really be renamed, possibly find_peaks
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