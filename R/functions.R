# Startup ----
.onAttach <- function(...) {
  ver <- utils::packageDescription("gcplyr")$Version
  build_date <- utils::packageDescription("gcplyr")$Date
  year <- sub("-.*", "", utils::packageDescription("gcplyr")$Date)
  
  packageStartupMessage(
    paste(
      "## \n",
      "## gcplyr (Version ", ver, ", Build Date: ", build_date, ")\n",
      "## See http://github.com/mikeblazanin/gcplyr for additional documentation\n",
      "## Please cite software as:\n",
      "##   Blazanin, Michael. ", year, ". 'gcplyr: manipulation and analysis of\n",
      "##   growth curves data.' R package version ", ver, "\n",
      "## \n",
      sep = ""))
}

# Utility functions ----

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
#' @export
uninterleave <- function(interleaved_list, n) {
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

#' A function that converts numbers into base-26 Excel-style letters
#' 
#' @param x A vector of numbers in base-10
#' 
#' @return A vector of letters in Excel-style base-26 format
#' 
#' @export
to_excel <- function(x) {
  x_numeric <- suppressWarnings(as.numeric(x))
  if(any(is.na(x_numeric))) {
  stop(paste("Failed to convert to Excel-format:",
       paste(x[is.na(x_numeric)], collapse = ",")))
  } else {x <- x_numeric}
  
  divisor_modulo_excel <- function(x) {
    #This function is just a way to return %/% and %% modified as Excel uses them
    #see Python inspiration: https://stackoverflow.com/questions/48983939/convert-a-number-to-excel-s-base-26
    div <- x %/% 26
    rem <- x %% 26
    if (rem == 0) {return(c(div-1, rem + 26))
    } else {return(c(div, rem))}
  }
  
  #Carry out conversion to Excel letters
  out <- c()
  for (i in 1:length(x)) {
    val <- x[i]
    chars <- c()
    while(val > 0) {
      temp <- divisor_modulo_excel(val)
      val <- temp[1]
      rem <- temp[2]
      chars <- c(LETTERS[rem], chars)
    }
    out <- c(out, paste(chars, collapse = ""))
  }
  return(out)
}

#' A function that converts base-26 Excel-style letters to numbers
#' 
#' @param x A vector of numbers in Excel-style base-26 letter format
#' 
#' @return A vector of numbers in base-10
#' 
#' @export
from_excel <- function(x) {
  #Based on: https://stackoverflow.com/questions/48983939/convert-a-number-to-excel-s-base-26
  out <- rep(NA, length(x))
  x_splt <- strsplit(x, "")
  for (i in 1:length(x_splt)) {
    #Get indices of letters
    temp <- match(x_splt[[i]], LETTERS)
    #Multiply indices by powers of 26
    out[i] <- sum(temp*26**((length(temp)-1):0))
  }
  if(any(is.na(out))) {
    stop(paste("Failed to convert from Excel-format:",
               paste(x[is.na(out)], collapse = ",")))
  }
  return(as.numeric(out))
}

#' A function that parses dots arguments and only passes the correct ones
#' 
#' Use this in cases where a parent function calls multiple sub-functions
#' and passes the ... dots argument to each, but some arguments users
#' specify in the ... dots argument only work for some of the sub-functions.
#' In this case, dots_parser will check and run \code{FUN} with only
#' the arguments that \code{FUN} accepts
#' 
#' @param FUN The function to be called
#' @param ... Additional arguments, some of which may not be arguments
#'            for \code{FUN}
#' 
#' @return The output of \code{FUN} operating on arguments in \code{...}
#' 
dots_parser <- function(FUN, ...) {
  argnames <- names(formals(FUN))
  dots <- list(...)
  return(do.call(FUN, dots[names(dots) %in% argnames]))
}

# Read files ----

#' An internal function that handles name inference logic for other functions
#' 
#' This function takes in the fully-read dataframe alongside information
#' about whether rows and columns have been specified, and whether header
#' and sider have been specified.
#' 
#' @param df The dataframe
#' @param startrow,endrow,startcol,endcol The rows & columns specified by user
#' @param header,sider Whether the file contains a header or sider
#'                     (can be TRUE, FALSE, or NA)
#' 
#' @details None of the specified arguments should be a vector, they should
#'          all be single values or NA's
#' 
#' @return a list: list(startrow, endrow, startcol, endcol, rownames_col, colnames_row)
#' 
infer_names <- function(df,
                        startrow, endrow, startcol, endcol,
                        header, sider) {
  
  #Infer endrow/endcol if they're not provided to be the last row/col
  if (is.na(endrow)) {endrow <- nrow(df)}
  if (is.na(endcol)) {endcol <- ncol(df)}
  
  output <- list(startrow = startrow, startcol = startcol, 
                 endrow = endrow, endcol = endcol,
                 rownames_col = NA, colnames_row = NA)
  
  #Inferring startrow/startcol & rownames/colnames is complex:
  #In this case the colnames will be auto-generated
  if (isFALSE(header) | (is.na(header) & isFALSE(sider))) {
    output$colnames_row <- 0
    if (is.na(output$startrow)) {output$startrow <- 1}
  }
  #In this case the rownames will be auto-generated
  if (isFALSE(sider) | (is.na(sider) & isFALSE(header))) {
    output$rownames_col <- 0
    if (is.na(output$startcol)) {output$startcol <- 1}
  }
  if (isTRUE(header)) {
    if (!is.na(startrow)) {
      output$colnames_row <- startrow
      output$startrow <- startrow + 1
    } else {
      output$colnames_row <- 1
      output$startrow <- 2
    }
    
    if (is.na(sider)) {
      #This is just a way to check if the top-left cell is empty
      # and if so then we'll use the first row/col as rownames/colnames
      temp <- c(startrow, startcol)
      temp[is.na(temp)] <- 1
      if (df[temp[1], temp[2]] == "" | is.na(df[temp[1], temp[2]])) {
        output$rownames_col <- temp[2]
        output$startcol <- temp[2] + 1
      } else {
        output$rownames_col <- 0
        output$startcol <- temp[2]
      }
    }
  }
  if (isTRUE(sider)) {
    if (!is.na(startcol)) {
      output$rownames_col <- startcol
      output$startcol <- startcol + 1
    } else {
      output$rownames_col <- 1
      output$startcol <- 2
    }
    
    if (is.na(header)) {
      #This is just a way to check if the top-left cell is empty
      # and if so then we'll use the first row/col as rownames/colnames
      temp <- c(startrow, startcol)
      temp[is.na(temp)] <- 1
      if (df[temp[1], temp[2]] == "" | is.na(df[temp[1], temp[2]])) {
        output$colnames_row <- temp[1]
        output$startrow <- temp[1] + 1
      } else {
        output$colnames_row <- 0
        output$startrow <- temp[1]
      }
    }
  }
  if (is.na(header) & is.na(sider)) {
    temp <- c(startrow, startcol)
    temp[is.na(temp)] <- 1
    if (df[temp[1], temp[2]] == "" | is.na(df[temp[1], temp[2]])) {
      output$colnames_row <- temp[1]
      output$startrow <- temp[1] + 1
      output$rownames_col <- temp[2]
      output$startcol <- temp[2] + 1
    } else {
      output$colnames_row <- 0
      output$startrow <- temp[1]
      output$rownames_col <- 0
      output$startcol <- temp[2]
    }
  }
  
  if(any(is.na(output))) {stop("infer_names failed to infer rows/cols")}
  
  #0's should be replaced with NA's
  output[output == 0] <- NA
  
  return(output)
}

#' Read blockmeasures
#' 
#' A function that reads block measures into the R environment
#' 
#' @param files A vector of filepaths relative to the current working directory
#'              where each filepath is a single plate read
#' @param extension (optional) the extension of the files:
#'                  "csv", "xls", or "xlsx", or "tbl" for use of read.table
#'                  
#'                  If none provided, \code{read_blocks} will infer file extension from
#'                  provided filenames. When extension is not "csv", "xls", or
#'                  "xlsx" will use \code{utils::read.table}
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the measures data are in \code{files},
#'                 can be a vector or list the same length as \code{files}, or
#'                 a single value that applies to all \code{files}.
#'                 If not provided data is presumed to begin on the first
#'                 row and column of the files.
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
#' @param header   \code{TRUE}, \code{FALSE}, or \code{NA}, or a vector of
#'                 such values, indicating whether the file(s) contains the
#'                 column names as its first line. If \code{header = NA}
#'                 will attempt to infer the presence of column names. If
#'                 \code{header = FALSE} or no column names are inferred when 
#'                 \code{header = NA}, column names will be generated
#'                 automatically according to \code{wellnames_Excel}
#' @param sider    \code{TRUE}, \code{FALSE}, or \code{NA}, or a vector of
#'                 such values, indicating whether the file(s) contains the
#'                 row names as its first line. If \code{sider = NA}
#'                 will attempt to infer the presence of row names. If
#'                 \code{sider = FALSE} or no row names are inferred when 
#'                 \code{sider = NA}, row names will be generated
#'                 automatically according to \code{wellnames_Excel}
#' @param wellnames_Excel If row names and column names are not provided in the
#'                        input dataframe as specified by \code{header}
#'                        and \code{sider}, then names will be generated
#'                        automatically.
#'                        If \code{wellnames_Excel} is TRUE, generated names
#'                        will use Excel-style base-26 lettering for columns
#'                        and numbers for rows. 
#'                        If \code{wellnames_Excel} is FALSE, rows and columns
#'                        will be numbered with "R" and "C" prefixes, respectively.
#' @param ...   Other arguments passed to \code{utils::read_csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx},
#'              or \code{utils::read.table}
#'
#' @details 
#'  For metadata, \code{read_blocks} can handle an arbitrary number of additional
#'  pieces of information to extract from each blockcurve file as metadata.
#'  These pieces of information are specified as a list of (named) vectors
#'  where each vector is the c(row, column) where the information is to be
#'  pulled from in the input files.
#' 
#'  This metadata is returned as the second list element of each
#'  blockcurve, e.g.:
#'  
#'   [[1]] [1] "data" #1 [2] "metadata"  [2][1] name #1
#'   
#'   [2][2] date-time #1
#'   
#'   [2][3] temp #1
#'   
#'   [[2]] [1] "data" #2 [2] "metadata"  [2][1] name #2
#'   
#'   [2][2] date-time #2
#'   
#'   [2][3] temp #2
#'   
#'   ...
#' 
#'  Calling \code{uninterleave} on the output of read_blocks works on block data
#'  and the associated metadata because uninterleave operates on the highest 
#'  level entries of the list (the [[1]] [[2]] level items), 
#'  leaving the meta-data associated with the block data
#' 
#'  \code{trans_block_to_wide} integrates this metadata into the
#'  wide-shaped dataframe it produces
#' 
#' @return A list where each entry is a list containing the block measures data
#'         followed by the block_names (or filenames, if block_names is not 
#'         provided) and any specified metadata.
#'
#' @export     
read_blocks <- function(files, extension = NULL, 
                        startrow = NULL, endrow = NULL, 
                        startcol = NULL, endcol = NULL,
                        sheet = NULL, metadata = NULL,
                        block_names = NULL,
                        header = NA, sider = NA,
                        wellnames_Excel = TRUE, ...) {
  if (is.null(startrow)) {
    startrow <- rep(NA, length(files))
  } else {
    if (!all(is.numeric(startrow))) {
      startrow[!is.numeric(startrow)] <- from_excel(startrow[!is.numeric(startrow)])
      startrow <- as.numeric(startrow)
    }
    startrow <- checkdim_inputs(startrow, "startrow", length(files))
  }
  if (is.null(endrow)) {
    endrow <- rep(NA, length(files))
  } else {
    if (!all(is.numeric(endrow))) {
      endrow[!is.numeric(endrow)] <- from_excel(endrow[!is.numeric(endrow)])
      endrow <- as.numeric(endrow)
    }
    endrow <- checkdim_inputs(endrow, "endrow", length(files))
  }
  if (is.null(startcol)) {
    startcol <- rep(NA, length(files))
  } else {
    if (!all(is.numeric(startcol))) {
      startcol[!is.numeric(startcol)] <- from_excel(startcol[!is.numeric(startcol)])
      startcol <- as.numeric(startcol)
    }
    startcol <- checkdim_inputs(startcol, "startcol", length(files))
  }
  if (is.null(endcol)) {
    endcol <- rep(NA, length(files))
  } else {
    if (!all(is.numeric(endcol))) {
      endcol[!is.numeric(endcol)] <- from_excel(endcol[!is.numeric(endcol)])
      endcol <- as.numeric(endcol)
    }
    endcol <- checkdim_inputs(endcol, "endcol", length(files))
  }
  if (!is.null(sheet)) {
    sheet <- checkdim_inputs(sheet, "sheet", length(files))
  }
  header <- checkdim_inputs(header, "infer_colnames", length(files))
  sider <- checkdim_inputs(sider, "infer_rownames", length(files))
  
  if (!is.null(block_names)) {
    stopifnot(length(block_names) == length(files))
  }
  
  #Determine file extension(s)
  if (is.null(extension)) {
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
                        USE.NAMES = FALSE)
    if(any(!extension %in% c("csv", "xls", "xlsx"))) {
      warning("Extension inferred but not one of: csv, xls, xlsx. Will treat as tbl")
    }
  } else {
    extension <- checkdim_inputs(extension, "extension", length(files))
    if(any(!extension %in% c("csv", "xls", "xlsx", "tbl"))) {
      stop("Extension provided by user must be one of: csv, xls, xlsx, tbl")
    }
  }
  
  
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
    if (extension[i] == "tbl") {
      temp <- dots_parser(utils::read.table, file = files[i], ...)
    } else if (extension[i] == "csv") {
      temp <- dots_parser(utils::read.csv, file = files[i], 
                          colClasses = "character", header = FALSE, ...)
    } else if (extension[i] == "xls") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xls, file = files[i], 
                        col_names = FALSE, col_types = "text", 
                        sheet = sheet[i], ...)))
    } else if (extension[i] == "xlsx") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xlsx, file = files[i], 
                        col_names = FALSE, col_types = "text", 
                        sheet = sheet[i], ...)))
    }
    
    #Infer rows, cols, rownames, colnames
    inferred_rc <- 
      infer_names(temp, startrow = startrow[i], endrow = endrow[i],
                  startcol = startcol[i], endcol = endcol[i],
                  header = header[i], sider = sider[i])
    
    #Save information to outputs
    outputs[[i]]$data <- temp[inferred_rc$startrow:inferred_rc$endrow,
                              inferred_rc$startcol:inferred_rc$endcol]
    
    #If temp_colnames or temp_rownames haven't been inferred, number them
    if (is.na(inferred_rc$colnames_row)) {
      if (wellnames_Excel) {
        temp_colnames <- to_excel(1:ncol(outputs[[i]]$data))
      } else {temp_colnames <- paste("C", 1:ncol(outputs[[i]]$data), sep = "")}
    } else {
      temp_colnames <- temp[inferred_rc$colnames_row, 
                            inferred_rc$startcol:inferred_rc$endcol]
    }
    if (is.na(inferred_rc$rownames_col)) {
      if (wellnames_Excel) {
        temp_rownames <- as.character(1:nrow(outputs[[i]]$data))
      } else {
        temp_rownames <- paste("R", 1:nrow(outputs[[i]]$data), sep = "")}
    } else {
      temp_rownames <- temp[inferred_rc$startrow:inferred_rc$endrow, 
                            inferred_rc$rownames_col]
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
        sub("^\\.?/?(.*)\\.[[:alnum:]]+$", "\\1", files[i])
    }
    #Add user-specified metadata (if any)
    if (!is.null(metadata)) {
      for (j in 1:length(metadata)) {
        #Convert from Excel-style formatting if needed
        if(is.na(suppressWarnings(as.numeric(metadata[[j]][1])))) {
          metadata[[j]][1] <- from_excel(metadata[[j]][1])
        }
        if(is.na(suppressWarnings(as.numeric(metadata[[j]][2])))) {
          metadata[[j]][2] <- from_excel(metadata[[j]][2])
        }
        outputs[[i]]$metadata[j+1] <- 
          temp[as.numeric(metadata[[j]][1]), as.numeric(metadata[[j]][2])]
      }
    }
  }
  
  ##Error checking for output dataframe dimensions
  if (length(outputs) > 1 &
      stats::var(sapply(outputs, simplify = TRUE, 
                 FUN = function(x) {dim(x$data)[1]})) != 0) {
    warning("Not all blockmeasures have the same number of rows of data")
  }
  if (length(outputs) > 1 &
      stats::var(sapply(outputs, simplify = TRUE,
                 FUN = function(x) {dim(x$data)[2]})) != 0) {
    warning("Not all blockmeasures have the same number of columns of data")
  }
  
  return(outputs)
}

#' Read wides
#' 
#' A function that imports widemeasures in files into the R environment
#' 
#' @details
#' startrow, endrow, startcol, endcol, timecol, sheet and extension 
#' can either be a single value that applies for all files or
#' vectors or lists the same length as \code{files}, 
#' 
#' @param files A vector of filepaths (relative to current working directory)
#'              where each one is a widemeasures set of data
#' @param extension (optional) the extension of the files:
#'                  "csv", "xls", or "xlsx", or "tbl" for use of read.table
#'                  
#'                  If none provided, \code{read_wides} will infer file 
#'                  extension from provided filenames. When extension is not 
#'                  "csv", "xls", or "xlsx" will use \code{utils::read.table}
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where
#'                  the data is located. If none provided assumes the entire
#'                  file is data.
#'                  Can be specified as a numeric or using base-26 Excel letter
#'                  notation
#' @param header Boolean for whether there is a header to the data. If FALSE
#'               columns are simple numbered. If TRUE is the row above
#'               \code{startrow} (if startrow is specified) or the first row
#'               of the input files (if startrow is not specified)
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#'              sheet
#' @param run_names Names to give the widemeasures read in. By default uses the
#'                   file names if not specified
#' @param names_to_col Should the run names (provided in \code{run_names}
#'                     or inferred from \code{files}) be added as a column to the
#'                     widemeasures? If \code{names_to_col} is NULL, they will not be.
#'                     If \code{names_to_col} is a string, that string will be
#'                     the column header for the column where the names will be
#'                     stored
#' @param metadata (optional) non-spectrophotometric data that should be associated
#'                 with each widemeasures. A list where each item in the
#'                 list is a vector of length 2. Each vector should provide the
#'                 row and column where the metadata is located in the widemeasures
#'                 input file. If the list is named those names will be inherited
#'                 to the output metadata.
#' @param metadata_Excel_names Boolean. If metadata do not have names, names
#'                             will be generated. If \code{metadata_Excel_names}
#'                             is TRUE, names will be in Excel format
#'                             (e.g. D7 is the 4th column, 7th row).
#'                             If FALSE, names will be numbered with "R" and "C"
#'                             prefixes for row and column.
#'                             (e.g. R4C7 is the 4th column, 7th row)
#' @param ...   Other arguments passed to \code{utils::read_csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx}, or
#'              \code{utils::read.table}
#'              
#' @return A dataframe containing a single widemeasures, or
#'         A list of widemeasures named by filename
#' 
#' @export
read_wides <- function(files, extension = NULL, 
                       startrow = NULL, endrow = NULL, 
                       startcol = NULL, endcol = NULL,
                       header = TRUE,
                       sheet = NULL, 
                       run_names = NULL,
                       names_to_col = "file",
                       metadata = NULL, 
                       metadata_Excel_names = TRUE,
                       ...) {
  #Logic 2.0: if header TRUE
  #             if startrow provided, header is startrow
  #             if startrow not provided, header is 1
  #           if header FALSE
  #             columns numbered V1...Vn
  
  if (!is.null(startrow) & !is.numeric(startrow)) {
    startrow <- from_excel(startrow)}
  if (!is.null(endrow) & !is.numeric(endrow)) {
    endrow <- from_excel(endrow)}
  if (!is.null(startcol) & !is.numeric(startcol)) {
    startcol <- from_excel(startcol)}
  if (!is.null(endcol) & !is.numeric(endcol)) {
    endcol <- from_excel(endcol)}
  
  if (!is.null(startrow) & header == TRUE & any(startrow <= 1)) {
    warning("startrow <= 1 but header is TRUE, treating header as FALSE")
    header <- FALSE
  }
  
  if(is.null(startrow)) {startrow <- NA}
  startrow <- checkdim_inputs(startrow, "startrow", length(files))
  
  if (is.null(endrow)) {endrow <- NA}
  endrow <- checkdim_inputs(endrow, "endrow", length(files))

  if (is.null(startcol)) {startcol <- NA}
  startcol <- checkdim_inputs(startcol, "startcol", length(files))
  
  if (is.null(endcol)) {endcol <- NA}
  endcol <- checkdim_inputs(endcol, "endcol", length(files))
  
  if (!is.null(sheet)) {
    sheet <- checkdim_inputs(sheet, "sheet", length(files))
  }
  
  #Determine file extension(s)
  if (is.null(extension)) {
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings",
                        USE.NAMES = FALSE)
    if(any(!extension %in% c("csv", "xls", "xlsx"))) {
      warning("Extension inferred but not one of: csv, xls, xlsx. Will treat as tbl")
    }
  } else {
    extension <- checkdim_inputs(extension, "extension", length(files))
    stopifnot(all(extension %in% c("csv", "xls", "xlsx", "tbl")))
  }
  
  #Check for names error
  if (!is.null(run_names)) {stopifnot(length(run_names) == length(files))}
  
  #If run_names not provided, infer from filenames
  if (is.null(run_names)) {
    #infer the names from filenames, stripping off the extension from end
    # and the dot at the beginning (if any)
    run_names <- sub("^\\.?/?(.*)\\.[[:alnum:]]+$", "\\1", files)
  }
  
  #If metadata unnamed, assign names
  if (!is.null(metadata)) {
    if (is.null(names(metadata))) {
      names(metadata) <- rep("", length(metadata))}
    for (i in 1:length(metadata)) {
      #Convert to numeric if provided Excel-style
      if(is.na(suppressWarnings(as.numeric(metadata[[i]][1])))) {
        metadata[[i]][1] <- from_excel(metadata[[i]][1])
      }
      if(is.na(suppressWarnings(as.numeric(metadata[[i]][2])))) {
        metadata[[i]][2] <- from_excel(metadata[[i]][2])
      }
      #Then make names
      if (names(metadata)[i] == "") {
        if (metadata_Excel_names) {
          names(metadata)[i] <- paste(to_excel(metadata[[i]][1]), 
                                      metadata[[i]][2], sep = "")
        } else {
          names(metadata)[i] <- paste("R", metadata[[i]][1], 
                                      "C", metadata[[i]][2], sep = "")
        }
      }
    }
  }
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Import data
  for (i in 1:length(files)) {
    #Read file & save in temp
    if (extension[i] == "tbl") {
      temp <- dots_parser(utils::read.table, file = files[i], ...)
    } else if (extension[i] == "csv") {
      temp <- 
        dots_parser(utils::read.csv, file = files[i], 
                    colClasses = "character", header = FALSE, ...)
    } else if (extension[i] == "xls") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xls, file = files[i], col_names = FALSE, 
                             col_types = "text", sheet = sheet[i], ...)))
    } else if (extension[i] == "xlsx") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xlsx, files[i], col_names = FALSE, 
                              col_types = "text", sheet = sheet[i], ...)))
    }
    
    #Infer colnames/take subsets as needed
    if(is.na(endrow[i])) {endrow[i] <- nrow(temp)}
    if(is.na(endcol[i])) {endcol[i] <- ncol(temp)}
    if(is.na(startcol[i])) {startcol[i] <- 1}
    if (is.na(startrow[i])) {startrow[i] <- 1}
    if (header == TRUE) { #so colnames taken from file
      outputs[[i]] <- temp[(startrow[i]+1):endrow[i], startcol[i]:endcol[i]]
      colnames(outputs[[i]]) <- temp[(startrow[i]), startcol[i]:endcol[i]]
    } else { #so colnames should be numbered
      outputs[[i]] <- temp[startrow[i]:endrow[i], startcol[i]:endcol[i]]
      colnames(outputs[[i]]) <- paste("V", 1:ncol(temp), sep = "")
    }
    
    #Get metadata
    if (!is.null(metadata)) {
      metadata_vector <- rep(NA, times = length(metadata))
      names(metadata_vector) <- names(metadata)
      for (j in 1:length(metadata)) {
        metadata_vector[j] <- 
          temp[as.numeric(metadata[[j]][1]), as.numeric(metadata[[j]][2])]
      }
    } else {metadata_vector <- NULL}
    #Add run_names if requested as column
    if(!is.null(names_to_col)) {
      metadata_vector <- c(run_names[i], metadata_vector)
      names(metadata_vector)[1] <- names_to_col
    }
    #Add metadata (incl run names) on LHS of df in same order as specified
    if (!is.null(metadata_vector)) {
      outputs[[i]] <- cbind(
        as.data.frame(
          sapply(metadata_vector, function(x, nreps) {rep(x, times = nreps)}, 
                 nreps = nrow(outputs[[i]]))),
        outputs[[i]])
    }
  }
  
  #Put names onto list elements
  names(outputs) <- run_names
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}

#' Read tidy-shaped files
#' 
#' A function that imports tidy-shaped files into R. Largely acts as a
#' wrapper for \code{utils::read.csv}, \code{readxl::read_xls},
#' \code{readxl::read_xls}, or \code{readxl::read_xlsx}, but can handle
#' multiple files at once and has additional options for taking subsets 
#' of rows/columns rather than the entire file and for adding filename 
#' or run names as an added column in the output.
#' 
#' @param files A vector of filepaths (relative to current working directory)
#'              where each one is a tidy-shaped data file
#' @param extension (optional) the extension of the files:
#'                  "csv", "xls", or "xlsx", or "tbl" for use of read.table
#'                  
#'                  If none provided, \code{read_tidys} will infer file 
#'                  extension from provided filenames. When extension is not 
#'                  "csv", "xls", or "xlsx" will use \code{utils::read.table}
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where
#'                  the data is located. If none provided assumes the entire
#'                  file is data.
#'                  
#'                  Can be specified as a numeric or using base-26 Excel letter
#'                  notation
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#' @param run_names Names to give the tidy files read in. By default uses the
#'                  file names if not specified. These names may be added
#'                  to the resulting data frame depending on the value of
#'                  the \code{names_to_col} argument
#' @param names_to_col Should the run names (provided in \code{run_names}
#'                     or inferred from \code{files}) be added as a column to the
#'                     output? 
#'                     
#'                     If \code{names_to_col} is TRUE, they will be added with.
#'                     the column name "run_name"
#'                     
#'                     If \code{names_to_col} is FALSE, they will not be added.
#'                     
#'                     If \code{names_to_col} is a string, they will be added
#'                     and the column name will be the string specified
#'                     for \code{names_to_col}.
#'                     
#'                     If \code{names_to_col} is NULL, they only will be 
#'                     added if there are multiple tidy data.frames being read.
#'                     In which case, the column name will be "run_name"
#'                     
#' @param ...   Other arguments passed to \code{utils::read_csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx}, or
#'              \code{utils::read.table}
#'              sheet
#'               
#' @details
#' \code{startrow}, \code{endrow}, \code{startcol}, \code{endcol}, 
#' \code{sheet} and \code{extension} can either be a single value that 
#' applies for all files or vectors or lists the same length as \code{files}
#' 
#' Note that the startrow is always assumed to be a header
#' 
#' @return A dataframe containing a single tidy data.frame, or
#'         A list of tidy-shaped data.frames named by filename
#'         
#' @export
read_tidys <- function(files, extension = NULL, 
                       startrow = NULL, endrow = NULL, 
                       startcol = NULL, endcol = NULL,
                       sheet = NULL, 
                       run_names = NULL, names_to_col = NULL,
                       ...) {
  if (!is.null(startrow) & !is.numeric(startrow)) {
    startrow <- from_excel(startrow)}
  if (!is.null(endrow) & !is.numeric(endrow)) {
    endrow <- from_excel(endrow)}
  if (!is.null(startcol) & !is.numeric(startcol)) {
    startcol <- from_excel(startcol)}
  if (!is.null(endcol) & !is.numeric(endcol)) {
    endcol <- from_excel(endcol)}
  
  if(is.null(startrow)) {startrow <- NA}
  startrow <- checkdim_inputs(startrow, "startrow", length(files))
  
  if (is.null(endrow)) {endrow <- NA}
  endrow <- checkdim_inputs(endrow, "endrow", length(files))
  
  if (is.null(startcol)) {startcol <- NA}
  startcol <- checkdim_inputs(startcol, "startcol", length(files))
  
  if (is.null(endcol)) {endcol <- NA}
  endcol <- checkdim_inputs(endcol, "endcol", length(files))
  
  if (!is.null(sheet)) {
    sheet <- checkdim_inputs(sheet, "sheet", length(files))
  }
  
  #Determine file extension(s)
  if (is.null(extension)) {
    extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings",
                        USE.NAMES = FALSE)
    if(any(!extension %in% c("csv", "xls", "xlsx"))) {
      warning("Extension inferred but not one of: csv, xls, xlsx. Will treat as tbl")
    }
  } else {
    extension <- checkdim_inputs(extension, "extension", length(files))
    stopifnot(all(extension %in% c("csv", "xls", "xlsx", "tbl")))
  }
  
  #Check for names error
  if (!is.null(run_names)) {stopifnot(length(run_names) == length(files))}
  
  #If run_names not provided, infer from filenames
  if (is.null(run_names)) {
    #infer the names from filenames, stripping off the extension from end
    # and the dot at the beginning (if any)
    run_names <- sub("^\\.?/?(.*)\\.[[:alnum:]]+$", "\\1", files)
  }
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Import data
  for (i in 1:length(files)) {
    #Read file & save in temp
    if (extension[i] == "tbl") {
      temp <- dots_parser(utils::read.table, file = files[i], ...)
    } else if (extension[i] == "csv") {
      temp <- 
        dots_parser(utils::read.csv, file = files[i], 
                    colClasses = "character", header = FALSE, ...)
    } else if (extension[i] == "xls") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xls, files[i], col_names = FALSE, 
                             col_types = "text", sheet = sheet[i], ...)))
    } else if (extension[i] == "xlsx") {
      suppressMessages(
        temp <- 
          as.data.frame(
            dots_parser(readxl::read_xlsx, files[i], col_names = FALSE, 
                              col_types = "text", sheet = sheet[i], ...)))
    }
    
    #Infer colnames/take subsets as needed
    if(is.na(endrow[i])) {endrow[i] <- nrow(temp)}
    if(is.na(endcol[i])) {endcol[i] <- ncol(temp)}
    if(is.na(startcol[i])) {startcol[i] <- 1}
    if (is.na(startrow[i])) {startrow[i] <- 1}
    
    #Get header
    outputs[[i]] <- temp[(startrow[i]+1):endrow[i], startcol[i]:endcol[i]]
    colnames(outputs[[i]]) <- temp[(startrow[i]), startcol[i]:endcol[i]]
    
    #Add run name if needed
    if(is.null(names_to_col)) {
      if (length(outputs) > 1) {
        #names should be saved in a column titled run_name
        outputs[[i]] <- cbind(data.frame("run_name" = run_names[i]),
                              outputs[[i]])
      }
    } else {
      if(is.character(names_to_col)) {
        #names should be saved in a column titled the value of names_to_col
        temp <- data.frame("run_name" = run_names[i])
        names(temp) <- names_to_col
        outputs[[i]] <- cbind(temp, outputs[[i]])
      } else {
        if(names_to_col) {
          #names_to_col is TRUE
          outputs[[i]] <- cbind(data.frame("run_name" = run_names[i]),
                                outputs[[i]])
        } else if (!names_to_col) {
          #names_to_col is FALSE, so add nothing
        } else {stop("names_to_col is not one of the valid types")}
      }
    }
  }
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}


## Import block-shaped (read, uninterleave, transform to wide) ----

#' Import blockmeasures
#' 
#' Function to import blockmeasures from files and return widemeasures
#' This function acts as a wrapper to call read_blocks, uninterleave, 
#' then trans_block_to_wide in one go
#' 
#' @param files Vector of filenames (as strings), each of which is a blockmeasures
#'              formatted file. Inputs can be .csv, .xls, or .xlsx
#' @param num_plates Number of plates. If multiple plates uninterleave will be
#'                   used to separate blockmeasures into those plates accordingly
#' @param plate_names (optional) Names to put onto the plates when output
#' @param wellnames_sep String to use as separator for well names between 
#'                      rowname and column name
#' @param ... Other arguments to pass to \code{read_blocks}, \code{uninterleave},
#'            or \code{widen_blocks}
#' 
#' @export       
import_blockmeasures <- function(files, num_plates = 1, 
                                 plate_names = NULL,
                                 wellnames_sep = "_",
                                 ...) {
  blockmeasures <- uninterleave(read_blocks(files = files, ...),
                                n = num_plates)
  widemeasures <- rep(list(NA), num_plates)
  for (i in 1:length(blockmeasures)) {
    widemeasures[[i]] <- trans_block_to_wide(blockmeasures[[i]],
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


#' Import blockdesigns
#' 
#' Function to import block-shaped designs from files and return tidy designs.
#' This function acts as a wrapper to call read_blocks, paste_blocks, 
#' trans_block_to_wide, and trans_wide_to_tidy in one go
#'
#' @param files Vector of filenames (as strings), each of which is a 
#'              block-shaped designs file. Inputs can be .csv, .xls, or .xlsx
#' @param into  Vector of column names for design elements after
#'              separation by \code{separate_tidys}. Should be in the same
#'              order as elements listed within \code{files} and/or same order
#'              as corresponding \code{files} themselves. If \code{NULL},
#'              column names will simply be "Design_1", "Design_2", etc.
#' @param ...   Other arguments to pass to \code{read_blocks}, 
#'              \code{paste_blocks}, \code{trans_block_to_wide},
#'              \code{trans_wide_to_tidy}, or \code{separate_tidy}.
#'              
#'              See Details for more information
#'              
#' @details     Common arguments that you may want to provide include:
#' 
#'              startrow, endrow, startcol, endcol, sheet - specifying the
#'              location of design information inside \code{files} to 
#'              \code{read_blocks}
#'              
#'              sep - string separating separate design elements for
#'              \code{separate_tidys} in block design files that are already
#'              pasted
#' 
#' @export
import_blockdesigns <- function(files, into = NULL, ...) {
  blocks <- dots_parser(read_blocks, files = files, ...)
  
  if(length(files) > 1) {
    blocks_pasted <- dots_parser(paste_blocks, blocks = blocks, ...)
  } else {blocks_pasted <- blocks}
  
  wides <- dots_parser(trans_block_to_wide, blocks = blocks_pasted, ...)
  
  tidys <- dots_parser(trans_wide_to_tidy, wides = wides, 
                       id_cols = "block_name", 
                       values_to = "Design", values_to_numeric = FALSE,
                       ...)
  
  if(!is.null(into)) {nfields <- length(into)
  } else {nfields <- length(strsplit(tidys[1, "Design"], split = "_")[[1]])}
  if(nfields > 1) {
    if(is.null(into)) {into = paste("Design", 1:nfields, sep = "_")}
    tidy_sep <- dots_parser(separate_tidy, data = tidys, 
                            col = "Design", into = into, ...)
  } else {tidy_sep <- tidys}
    
  return(tidy_sep)
}


# Make designs ----

#' Make tidy design data.frames
#' 
#' This is a function to easily input experimental design elements
#' for later merging with read data
#' 
#' @details 
#' Note that either \code{nrows} or \code{block_row_names} must be provided
#' and that either \code{ncols} or \code{block_col_names} must be provided
#' 
#' Examples:
#' my_example <- make_tidydesign(nrows = 8, ncols = 12,
#'         design_element_name = list(c("Value1", "Value2", "Value3"),
#'                           rowstart:rowend, colstart:colend,
#'                           "111222333000", TRUE)
#' To make it easier to pass arguments, use make_designpattern:
#' my_example <- make_tidydesign(nrows = 8, ncols = 12,
#'       design_element_name = make_designpattern(values = c("L", "G", "C"),
#'                                                 rows = 2:7, cols = 2:11,
#'                                                 pattern = "11223300",
#'                                                 byrow = TRUE))
#' 
#' @param nrows,ncols Number of rows and columns in the plate data
#' @param block_row_names,block_col_names Names of the rows, columns
#'                                     of the plate blockmeasures data
#' @param wellnames_sep A string used when concatenating rownames and column
#'                      names to create well names
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names
#' @param wellnames_Excel If \code{block_row_names} or \code{block_col_names}
#'                        are not specified, should rows and columns be named
#'                        using Excel-style base-26 lettering for rows
#'                        and numbering for columns? If FALSE, rows and columns
#'                        will be numbered with "R" and "C" prefix.
#' @param pattern_split character to split pattern elements provided in
#'                      \code{...} by
#' @param lookup_tbl_start Value in the lookup table for the split pattern values
#'                         that corresponds to the first value in the vector.
#'                         
#'                         Lookup table by default is 
#'                         c(1,2,...,8,9,A,B,...Y,Z,a,b,...,y,z). If,
#'                         for example, lookup_tbl_start = "A", then the lookup
#'                         table will now be c(A,B,...Y,Z,a,b,...,y,z)
#' @param colnames_first  In the wellnames created by \code{paste}-ing the
#'                        rownames and column names, should the column names
#'                        come first
#' @param ... Each \code{...} argument must be a list with five elements:
#' 
#'              1. a vector of the values
#'              
#'              2. a vector of the rows the pattern should be applied to
#'              
#'              3. a vector of the columns the pattern should be applied to
#'              
#'              4. a string of the pattern itself, where numbers refer to
#'               the indices in the values vector
#'               
#'               0's refer to NA
#'               
#'               This pattern will be split using pattern_split, which
#'               defaults to every character
#'               
#'              5. a Boolean for whether this pattern should be filled byrow
#'              
#' 
#' @export         
make_tidydesign <- function(nrows = NULL, ncols = NULL,
                        block_row_names = NULL, block_col_names = NULL,
                        wellnames_sep = "", wellnames_colname = "Well",
                        wellnames_Excel = TRUE, lookup_tbl_start = 1,
                        pattern_split = "", colnames_first = FALSE,
                        ...) {

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
  if(is.null(nrows) & is.null(block_row_names)) {
    stop("nrows or block_row_names must be provided")
  }
  if(is.null(ncols) & is.null(block_col_names)) {
    stop("ncols or block_col_names must be provided")
  }
  if (is.null(block_row_names)) {
    if (wellnames_Excel) {block_row_names <- to_excel(1:nrows)
    } else {block_row_names <- paste("R", 1:nrows, sep = "")}
  }
  if (is.null(block_col_names)) {
    if (wellnames_Excel) {block_col_names <- 1:ncols
    } else {block_col_names <- paste("C", 1:ncols, sep = "")}
  }
  if (is.null(nrows)) {nrows <- length(block_row_names)}
  if (is.null(ncols)) {ncols <- length(block_col_names)}
  
  dot_args <- list(...)
  
  #Make base output dataframe
  output <- as.data.frame(matrix(NA, nrow = nrows*ncols, 
                                 ncol = 1+length(unique(names(dot_args)))))
  if(colnames_first) {
    output[,1] <- paste(block_col_names,
                        rep(block_row_names, each = ncols),
                        sep = wellnames_sep)
  } else {
    output[,1] <- paste(rep(block_row_names, each = ncols),
                        block_col_names, sep = wellnames_sep)
  }
  
  colnames(output)[1] <- wellnames_colname
  
  #Note dot_args structure
  #dot_args[[i]] = list(values = c("A", "B", "C"),
  #                     rows = rowstart:rowend, cols = colstart:colend
  #                     pattern = "111222333000", byrow = TRUE)
  
  #Loop through input arguments & fill into output dataframe
  for (i in 1:length(dot_args)) {
    pattern_list <- strsplit(dot_args[[i]][[4]],
                             split = pattern_split)[[1]]
    if (any(nchar(pattern_list) > 1)) {
      if (any(is.na(suppressWarnings(as.numeric(pattern_list))))) {
        stop("Pattern values are multi-character after splitting, but not all pattern values are numeric")
      } else { #they're all numeric
        pattern_list <- as.numeric(pattern_list)
        pattern_list[pattern_list==0] <- NA
      }
    } else { #they're all single-character pattern values
      lookup_table <- c(1:9, LETTERS, letters) #Note since 0 not included, 0's become NA
      lookup_table <- lookup_table[match(lookup_tbl_start, lookup_table):
                                   length(lookup_table)]
      if (any(!pattern_list[pattern_list != "0"] %in% lookup_table)) {
        stop("Some values in pattern are not in lookup table. Check that you 
             have lookup_tbl_start correct and that you're only using 
             alphanumeric values")
      }
      pattern_list <- match(pattern_list, lookup_table)
    }
    
    if (((length(dot_args[[i]][[2]])*length(dot_args[[i]][[3]])) %% 
         length(pattern_list)) != 0) {
      warning(paste("Total number of wells is not a multiple of pattern length for",
                    names(dot_args)[i]))
    }
    
    #Byrow is optional, if not provided default is byrow = TRUE
    if (length(dot_args[[i]]) < 5) {
      dot_args[[i]][[5]] <- TRUE
    }
    
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

#' Make design pattern
#' 
#' A helper function for use with make_tidydesign
#' 
#' @details 
#' Example:
#' my_example <- make_tidydesign(nrows = 8, ncols = 12,
#'       design_element_name = make_designpattern(values = c("L", "G", "C"),
#'                                                 rows = 2:7, cols = 2:11,
#'                                                 pattern = "11223300",
#'                                                 byrow = TRUE))
#' 
#' @param values Vector of values to use
#' @param rows Vector of rows where pattern applies
#' @param cols Vector of cols where pattern applies
#' @param pattern Numeric pattern itself, where numbers refer to entries
#'                in \code{values}
#' @param byrow Boolean for whether pattern should be created by row
#' 
#' @export
make_designpattern <- function(values, rows, cols, pattern, byrow = TRUE) {
  stopifnot(is.vector(values), is.vector(rows), is.vector(cols),
            is.character(pattern), is.logical(byrow))
  return(list(values, rows, cols, pattern, byrow))
}

# Reshape (including merge) ----

#' Transform blocks to wides
#' 
#' Takes blocks and returns them in a wide format
#' 
#' @param blocks Blocks, either a single data.frame or a list of
#'                      data.frames
#' @param wellnames_sep String to use as separator for well names between 
#'                      rowname and column name
#' @param nested_metadata A Boolean indicating the existence of nested metadata
#'                        in the \code{blockmeasures} list, e.g. as is typically
#'                        output by \code{read_blocks}. If NULL, will attempt to
#'                        infer existence of nested metadata
#' @param colnames_first  In the wellnames created by \code{paste}-ing the
#'                        rownames and column names, should the column names
#'                        come first
#'
#' @return A single widemeasures data.frame
#' 
#' @export
trans_block_to_wide <- function(blocks, wellnames_sep = "_", 
                         nested_metadata = NULL, colnames_first = FALSE) {
  
  if(class(blocks) != "list") {
    blocks <- list(blocks)
  }
  
  #Infer nestedness if nested_metadata is set to NULL
  if (is.null(nested_metadata)) {
    if (all(sapply(blocks, simplify = TRUE, FUN = class) == "data.frame")) {
      nested_metadata <- FALSE
      warning("Inferring nested_metadata to be FALSE")
    } else if (all(sapply(blocks, simplify = TRUE, FUN = class) == "list")) {
      nested_metadata <- TRUE
      warning("Inferring nested_metadata to be TRUE")
    } else {
      stop("Unable to infer nested_metadata, this may be because blocks vary in nestedness or are not data.frame's")
    }
  }
  
  #Check that all blocks have same dimensions
  if (length(blocks) > 1) {
    if (nested_metadata) { #there is nested metadata
      if (stats::var(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x[[1]])[1]})) != 0) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (stats::var(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x[[1]])[2]})) != 0) {
        stop("Not all blocks have the same number of columns of data")
      }
    } else { #there is not nested metadata
      if (stats::var(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x)[1]})) != 0) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (stats::var(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x)[2]})) != 0) {
        stop("Not all blocks have the same number of columns of data")
      }
    }
  }
  
  #convert list of dataframes to single dataframe
  #where each column is a well and each row is a plate read
  #(each column is a row-column combination from the blockcurve)
  #(each row is a single dataframe from blocks)
  #Adding the metadata as the first n columns

  if (nested_metadata) { #There is nested metadata
    if (colnames_first) {
      #Reshape
      output <- 
        as.data.frame(
          do.call(rbind,
                  lapply(
                    blocks, 
                    FUN = function(x) {c(x[[2]],
                                         apply(x[[1]], MARGIN = 2, unlist))})),
          stringsAsFactors = FALSE)
      #Assign column names
      colnames(output) <- c(names(blocks[[1]][[2]]),
                            paste(rep(colnames(blocks[[1]][[1]]),
                                      each = nrow(blocks[[1]][[1]])),
                                  rownames(blocks[[1]][[1]]),
                                  sep = wellnames_sep))
    } else { #rownames first
      #Reshape
      output <- 
        as.data.frame(
          do.call(rbind,
                  lapply(
                    blocks, 
                    FUN = function(x) {c(x[[2]],
                                         apply(x[[1]], MARGIN = 1, unlist))})),
          stringsAsFactors = FALSE)
      #Assign column names
      colnames(output) <- c(names(blocks[[1]][[2]]),
                            paste(rep(rownames(blocks[[1]][[1]]),
                                      each = ncol(blocks[[1]][[1]])),
                                  colnames(blocks[[1]][[1]]),
                                  sep = wellnames_sep))
    }
  } else { #There is not nested metadata
    if (colnames_first) {
      #Reshape
      output <- 
        as.data.frame(
          do.call(rbind,
                  lapply(blocks, 
                         FUN = function(x) {c(apply(x, MARGIN = 2, unlist))})),
          stringsAsFactors = FALSE)
      #Assign column names
      colnames(output) <- paste(rep(colnames(blocks[[1]]),
                                    each = nrow(blocks[[1]])),
                                rownames(blocks[[1]]),
                                sep = wellnames_sep)
    } else { #rownames first
      #Reshape
      output <- 
        as.data.frame(
          do.call(rbind,
                  lapply(blocks, 
                         FUN = function(x) {c(apply(x, MARGIN = 1, unlist))})),
          stringsAsFactors = FALSE)
      #Assign column names
      colnames(output) <- paste(rep(rownames(blocks[[1]]),
                                    each = ncol(blocks[[1]])),
                                colnames(blocks[[1]]),
                                sep = wellnames_sep)
    }
  }
  
  return(output)
}

#' Transform wides into block format (in progress)
#' 
#' Takes wides and returns them in a block format
#'
#' @param wides Wide dataframe(s) (either widemeasures or widedesign)
#' @param collapse NULL or a string to use for concatenating contents
#'                 together. If NULL each row in \code{wides} will be put into 
#'                 its own block. If a string, that string will be used to 
#'                 \code{paste} together all elements and all elements will
#'                 be returned in a single block
#' @param wellnames_sep A string used to identify the rownames and column
#'                      names
#' @return A list of block data.frames (if \code{collapse} is not 
#'         \code{NULL} the list is of length 1)
#' 
trans_wide_to_block <- function(wides, collapse = NULL,
                             wellnames_sep = "_") {
  
  
  
  ##Old code below:
  
  
  # #Make empty output
  # output <- rep(list(matrix(NA, nrow = length(unique(rownames)),
  #                           ncol = length(unique(colnames)))), 
  #               length(which(colnames(tidydesign) != wellnames_colname)))
  # 
  # 
  # 
  # 
  # #Get rownames & colnames from well column
  # rownames <- sapply(strsplit(tidydesign[, wellnames_colname],
  #                             split = wellnames_sep),
  #                    simplify = TRUE,
  #                    FUN = function(x) {x[1]})
  # colnames <- sapply(strsplit(tidydesign[, wellnames_colname],
  #                             split = wellnames_sep),
  #                    simplify = TRUE,
  #                    FUN = function(x) {x[2]})
  # 
  # 
  # #Iterate through each design element
  # i <- 1
  # for (col in which(colnames(tidydesign) != wellnames_colname)) {
  #   #Assign row and column names
  #   rownames(output[[i]]) <- unique(rownames)
  #   colnames(output[[i]]) <- unique(colnames)
  #   #Fill data into appropriate row,column in output
  #   # (spot is found by matching row/col name to row/col from well column
  #   #  in tidydesign input)
  #   output[[i]][match(rownames, rownames(output[[i]])) + 
  #                 (match(colnames, colnames(output[[i]]))-1)*nrow(output[[i]])] <-
  #     tidydesign[, col]
  #   i <- i+1
  # }
  # 
  # #Collapse (if requested)
  # if (!is.null(collapse)) {
  #   output <- list(matrix(do.call("paste", c(output, sep = collapse)),
  #                         nrow = length(unique(rownames)),
  #                         ncol = length(unique(colnames))))
  #   rownames(output[[1]]) <- unique(rownames)
  #   colnames(output[[1]]) <- unique(colnames)
  #   names(output) <- paste(
  #     colnames(tidydesign)[which(colnames(tidydesign) != wellnames_colname)],
  #     collapse = collapse)
  # }
  # 
  # return(output)
}


#' Pivot widemeasures longer
#' 
#' Essentially a wrapper for tidyr::pivot_longer that works on both a single
#' widemeasures as well as a list of widemeasures
#' 
#' @param wides A single widemeasures data.frame, or a list of widemeasures
#'                   data.frame's
#' @param data_cols,id_cols Specifies which columns have data vs are ID's
#'                          (in \code{tidyr::pivot_longer} parlance). Each can be
#'                          a single vector (which will be applied for all
#'                          widemeasures) or a list of vectors, with each
#'                          vector corresponding to the same-index widemeasure
#'                          in \code{widemeasures}
#'                          
#'                          Entries that are NA in the list will not be used
#'                          
#'                          If neither data_cols nor id_cols are specified,
#'                          user must provide arguments to tidyr::pivot_longer
#'                          via \code{...} for at least the \code{cols} argument
#'                          and these arguments provided via \code{...} will
#'                          be used for all \code{widemeasures} data.frame's
#' @param names_to,values_to Specifies the output column names created by
#'                           tidyr::pivot_longer. Each can be provided as vectors
#'                           the same length as \code{widemeasures}
#'                           Note that if neither data_cols nor id_cols
#' @param values_to_numeric Boolean indicating whether values will be coerced
#'                          to numeric. See below for when this may be
#'                          overridden by arguments passed in \code{...}
#' @param ... Other functions to be passed to tidyr::pivot_longer
#'            Note that including values_transform here will override the
#'            behavior of values_to_numeric
#' @return Pivoted longer data.frame (if \code{widemeasures} is a single data.frame)
#'         or list of pivoted longer data.frame's (if \code{widemeasures} is
#'         a list of data.frame's)
#' 
#' @export  
trans_wide_to_tidy <- function(wides, 
                              data_cols = NA,
                              id_cols = NA,
                              names_to = "Well",
                              values_to = "Measurements",
                              values_to_numeric = TRUE,
                              ...) {
  #Reformat to list
  if (is.data.frame(wides)) {
    wides <- list(wides)
  }
  
  #Reformat cols inputs as needed
  if (!is.list(id_cols)) {
    id_cols <- list(id_cols)
  }
  id_cols <- checkdim_inputs(id_cols, "id_cols", length(wides))
  
  if (!is.list(data_cols)) {
    data_cols <- list(data_cols)
  }
  data_cols <- checkdim_inputs(data_cols, "data_cols", length(wides))
  
  #Check cols inputs
  if (any(!is.na(data_cols) & !is.na(id_cols))) {
    warning("Cannot provide both data_cols and id_cols for a given wides, using data_cols only")
  }
  
  names_to <- checkdim_inputs(names_to, "names_to", length(wides))
  values_to <- checkdim_inputs(values_to, "values_to", length(wides))
  
  #Create values_transform list as appropriate
  if("values_transform" %in% names(list(...))) {
    if(values_to_numeric) {
      warning("values_to_numeric is TRUE but values_transform is supplied in ..., 
              values_transform will override values_to_numeric")}
  } else {
    if(values_to_numeric) {
      values_transform = rep(list(list(temp = as.numeric)), length(values_to))
      for (i in 1:length(values_to)) {
        names(values_transform[[i]]) <- values_to[i]
      }
    } else {values_transform = rep(list(list()), length(values_to))}
  }
  
  #Empty list for outputs
  outputs <- rep(list(NA), length(wides))
  for (i in 1:length(wides)) {
    if (!is.na(data_cols[i])) { #user specified which columns are data columns
      outputs[[i]] <- as.data.frame(
        tidyr::pivot_longer(wides[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = data_cols[[i]],
                            values_transform = values_transform[[i]],
                            ...))
    } else if (!is.na(id_cols[i])) { #user specified which columns are id columns
      outputs[[i]] <- as.data.frame(
        tidyr::pivot_longer(wides[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = which(!colnames(wides[[i]]) %in% id_cols[[i]]),
                            values_transform = values_transform[[i]],
                            ...))
    } else { #User must be providing their own arguments to pivot_longer
      outputs[[i]] <- as.data.frame(tidyr::pivot_longer(wides[[i]],
                                                        names_to = names_to[i],
                                                        values_to = values_to[i],
                                                        values_transform = values_transform[[i]],
                                                        ...))
    }
  }
  
  names(outputs) <- names(wides)
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}

#' Transform tidy dfs into wide (in progress)
#' 
trans_tidy_to_wide <- function() {
  
}

#' Collapse a list of dataframes, or merge two dataframes together
#' 
#' This function is essentially a wrapper for dplyr::full_join
#' The most typical use of this function is to merge designs 
#' with measures data, or to use the collapse functionality of this 
#' function to merge a list of dataframes into a single dataframe 
#'  
#' @param x First data.frame to be joined
#' @param y Second data.frame to be joined
#' @param by A character vector of variables to join by, passed directly
#'           to \code{dplyr::full_join}
#' @param drop Should only \code{complete_cases} of the resulting
#'             data.frame be returned?
#' @param collapse A Boolean indicating whether x or y is a list containing
#'                 data frames that should be merged together before
#'                 being merged with the other
#' @param names_to Column name for where \code{names(x)} or \code{names(y)} 
#'                 will be entered in if \code{collapse = TRUE}
#'                 If a value of \code{NA} then \code{names(x)} or 
#'                 \code{names(y)} will not be put into a column in the
#'                 returned data.frame
#' @param ... Other arguments to pass to \code{dplyr::full_join}
#' 
#' @return Data.frame containing merged output of \code{x} and
#'         \code{y}
#' 
#' @export
merge_dfs <- function(x, y = NULL, by = NULL, drop = FALSE,
                             collapse = FALSE, names_to = NA,
                             ...) {
  if(collapse) {
    #First define the worker func that collapses the df's
    collapse_list <- function(listdfs, names_to) {
      temp <- NULL
      for (i in 1:length(listdfs)) {
        if(!is.null(names_to) & !is.na(names_to)) {
          #Put name of ea list element (ea df) into column
          listdfs[[i]] <- cbind(listdfs[[i]], names(listdfs)[i])
          colnames(listdfs[[i]])[ncol(listdfs[[i]])] <- names_to
        }
        #Collapse dfs together
        if (is.null(temp)) {temp <- listdfs[[i]]
        } else {temp <- dplyr::full_join(temp, listdfs[[i]])}
      }
      return(temp)
    }
    
    #Then collapse x and collapse y into dfs as needed
    if (is.data.frame(x) & is.data.frame(y)) {
      warning("collapse = TRUE, but both x and y are data.frames already")
    } else {
      if (!is.data.frame(x)) {
        if (!is.list(x)) {stop("x is neither a list nor a data.frame")}
        x <- collapse_list(x, names_to = names_to)
      }
      if (!is.null(y) & !is.data.frame(y)) {
        if (!is.list(y)) {
          stop("y is neither a list nor a data.frame")}
        y <- collapse_list(y, names_to = names_to)
      }
    }
  }
  
  if (!is.null(y)) {
    #Join x and y
    output <- dplyr::full_join(x, y,
                             by = by, ...)
    if (drop) {output <- output[stats::complete.cases(output), ]}
  } else {output <- x}
  
  return(output)
}

#' Paste a list of blocks into a single block
#' 
#' This function uses \code{paste} to concatenate the same-location entries
#' of a list of data.frames together (i.e. all the first row-first column
#' values are pasted together, all the second row-first column values are
#' pasted together, etc.)
#' 
#' @param blocks Blocks, either a single data.frame or a list of
#'                      data.frames
#' @param sep String to use as separator for output pasted values
#' @param nested_metadata A Boolean indicating the existence of nested metadata
#'                        in the \code{blockmeasures} list, e.g. as is typically
#'                        output by \code{read_blocks}. If NULL, will attempt to
#'                        infer existence of nested metadata
#' 
#' @return If nested_metadata = TRUE (or is inferred to be TRUE), a list
#'         containing a list containing: 1. a \code{data.frame} with the
#'         pasted data values from \code{blocks}, and 2. a vector with the 
#'         pasted metadata values from \code{blocks}
#'         
#'         If nested_metadata = FALSE (or is inferred to be FALSE), a list
#'         containing \code{data.frame}'s with the pasted values from
#'         \code{blocks}
#' 
#' @export
paste_blocks <- function(blocks, sep = "_", nested_metadata = NULL) {
  #Infer nestedness if nested_metadata is set to NULL
  if (is.null(nested_metadata)) {
    if (all(sapply(blocks, simplify = TRUE, FUN = class) == "data.frame")) {
      nested_metadata <- FALSE
      warning("Inferring nested_metadata to be FALSE")
    } else if (all(sapply(blocks, simplify = TRUE, FUN = class) == "list")) {
      nested_metadata <- TRUE
      warning("Inferring nested_metadata to be TRUE")
    } else {
      stop("Unable to infer nested_metadata, this may be because blocks vary in nestedness or are not data.frame's")
    }
  }
  
  #Check that all blocks have same dimensions
  if (length(blocks) > 1) {
    if (nested_metadata) { #there is nested metadata
      if (stats::var(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x[[1]])[1]})) != 0) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (stats::var(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x[[1]])[2]})) != 0) {
        stop("Not all blocks have the same number of columns of data")
      }
    } else { #there is not nested metadata
      if (stats::var(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x)[1]})) != 0) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (stats::var(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x)[2]})) != 0) {
        stop("Not all blocks have the same number of columns of data")
      }
    }
  }
  
  if(nested_metadata) {
    #Make empty output list
    blocks_pasted <- 
      list(list(data = data.frame(matrix(nrow = nrow(blocks[[1]]$data),
                                         ncol = ncol(blocks[[1]]$data))),
                metadata = rep(NA, length(blocks[[1]]$metadata))))
    colnames(blocks_pasted[[1]]$data) <- colnames(blocks[[1]]$data)
    row.names(blocks_pasted[[1]]$data) <- row.names(blocks[[1]]$data)
    names(blocks_pasted[[1]]$metadata) <- names(blocks[[1]]$metadata)
    
    #Fill in values
    for (i in 1:nrow(blocks_pasted[[1]]$data)) {
      for (j in 1:ncol(blocks_pasted[[1]]$data)) {
        blocks_pasted[[1]]$data[i, j] <-
          paste(sapply(blocks, FUN = function(x) {x[[1]][i, j]}),
                collapse = sep)
      }
    }
    for (i in 1:length(blocks_pasted[[1]]$metadata)) {
      blocks_pasted[[1]]$metadata[i] <-
        paste(sapply(blocks, FUN = function(x) {x[[2]][i]}), collapse = sep)
    }
  } else { #nested_metadata = FALSE
    #make empty output list
    blocks_pasted <- 
      data.frame(matrix(nrow = nrow(blocks[[1]]), ncol = ncol(blocks[[1]])))
    colnames(blocks_pasted) <- colnames(blocks[[1]])
    row.names(blocks_pasted) <- row.names(blocks[[1]])
    
    #fill in values
    for (i in 1:nrow(blocks_pasted)) {
      for (j in 1:ncol(blocks_pasted)) {
        blocks_pasted[i, j] <-
          paste(sapply(blocks, FUN = function(x) {x[i, j]}),
                collapse = sep)
      }
    }
  }
  
  return(blocks_pasted)
}

#' Separate a column into multiple columns
#' 
#' This function is primarily a wrapper for \code{tidyr::separate}, which
#' turns a single character column into multiple columns
#' 
#' @param data A data frame
#' @param col  Column name or position
#' @param into A character vector of the new column names. Use \code{NA} to
#'             omit the variable in the output.
#'             
#'             If NULL, \code{separate_gc} will attempt to infer the new
#'             column names from the column name of \code{col}
#' @param sep Separator between columns passed to \code{tidyr::separate}:
#' 
#'            If character, \code{sep} is interpreted as a regular expression.
#'            
#'            If numeric, \code{sep} is interpreted as character positions
#'            to split at. Positive values start at 1 at the far-left of the
#'            string; negative values start at -1 at the far-right of the
#'            string. The length of \code{sep} should be one less than 
#'            \code{into}
#' @param ... Other arguments passed to \code{tidyr::separate}
#' 
#' @return A data frame containing new columns in the place of \code{col}
#' 
#' @export
separate_tidy <- function(data, col, into = NULL, sep = "_", ...) {
  if(is.null(into)) {
    if(col %in% colnames(data)) {
      into <- strsplit(col, sep = sep)[[1]]
    } else if (is.numeric(col)) {
      into <- strsplit(colnames(data)[col], sep = sep)[[1]]
    } else {
      stop("into is NULL, but col is neither numeric nor a column name in data")
    }
  }
  
  return(
    tidyr::separate(data = data, col = col, into = into, sep = sep, ...))
}


# Smoothing ----

#' Smooth data
#' 
#' This function calls other functions to smooth growth curve data
#' 
#' @param x An (optional) vector of predictor values to smooth along (e.g. time)
#' @param y A vector of response values to be smoothed (e.g. density)
#' @param method Argument specifying which smoothing method should
#'                  be used to smooth data. Options include 
#'                  "moving-average", "moving-median', "loess", and "gam"
#' @param subset_by A vector as long as the number of rows of data. 
#'                  Each unique value of this vector will be smoothed
#'                  independently of the others.
#' @param return_fitobject Boolean indicating whether entire object returned
#'                         by fitting function should be returned. If FALSE,
#'                         just fitted values are returned.
#' @param ... Other arguments passed to \code{stats::loess}, \code{mgcv::gam},
#'            \code{moving_average}, or \code{moving_median}.
#'            
#'            For \code{moving_average} and \code{moving_median}, window_width_n
#'            is required. For \code{loess} and \code{gam}, see details.
#'
#' @details For smoothing using \code{loess} or \code{gam} that depends on 
#'          more than one predictor, \code{formula} and \code{data} can be
#'          passed to \code{smooth_data} via the \code{...} argument and will
#'          override the \code{x} and \code{y} arguments.
#'          
#'          The formula should specify the response (e.g. density) 
#'          and predictors. For \code{gam} smoothing, the formula should
#'          typically be of the format: y ~ s(x), which uses 
#'          \code{mgcv::s} to smooth the data
#'          
#'          The data argument should be a \code{data.frame} containing the
#'          variables in the formula
#' 
#' @return If return_fitobject == FALSE:
#' 
#'         A vector, the same length as \code{y}, with the now-smoothed y values
#'         
#'         If return_fitobject == TRUE:
#'         
#'         A list the same length as unique(subset_by) where each element is
#'         an object of the same class as returned by the smoothing method
#'         (typically a named list-like object)
#'         
#'         Varies by method, but always with a first element named 'fitted'
#'         containing the smoothed values of the response variable, and a 
#'         second element named 'residuals' containing the residuals of the
#'         fitted values and the input values
#' 
#' @export
smooth_data <- function(x = NULL, y, method, subset_by = NULL,
                        return_fitobject = FALSE, ...) {
  if(!method %in% c("moving-average", "moving-median", "gam", "loess")) {
    stop("method must be one of: moving-average, moving-median, gam, or loess")
  }
  
  #Parse x and y, or ... args, into formula and data
  if (any(c("formula", "data") %in% names(list(...)))) {
    if(!all(c("formula", "data") %in% names(list(...)))) {
      warning("both or neither formula and data must be specified, reverting to smoothing y on x")
    } else {
      formula <- list(...)$formula
      data <- list(...)$data
    }
  } else {
    if(is.null(x)) {x <- 1:length(y)}
    if(length(x) != length(y)) {stop("x and y must be the same length")}
    data <- data.frame(x, y)
    if(method == "gam") {formula <- y ~ s(x)
    } else {formula <- y ~ x}
  }
  
  if (method == "gam" & substr(as.character(formula[3]), 1, 2) != "s(") {
    warning("gam method is called without 's()' to smooth")}
  
  if(is.null(subset_by)) {subset_by <- rep("A", nrow(data))
  } else if (length(subset_by) != nrow(data)) {
    stop("subset_by must be the same length as data")
  }
  
  #Prepare output containers
  if (return_fitobject) {
    fits_list <- list(rep(NA, length(unique(subset_by))))
  } else {
    out <- rep(NA, nrow(data))
  }
  
  #Run smoothing methods
  for (i in 1:length(unique(subset_by))) {
    #Calculate fitted values
    if (method == "moving-average") {
      temp <- 
        list(
        moving_average(formula = formula, 
                       data = data[subset_by == unique(subset_by)[i], ],
                       ...))
      names(temp) <- "fitted"
    } else if (method == "moving-median") {
      temp <-
        list(
          moving_median(formula = formula,
                       data = data[subset_by == unique(subset_by)[i], ],
                       ...))
      names(temp) <- "fitted"
    } else {
      if (method == "loess") {
        temp <- 
          stats::loess(formula = formula, 
                       data = data[subset_by == unique(subset_by)[i], ], 
                       ...)
      } else if (method == "gam") {
        temp <- 
          mgcv::gam(formula = formula, 
                    data = data[subset_by == unique(subset_by)[i], ], 
                    ...)
        #Rename fitted.values to fitted
        names(temp)[match("fitted.values", names(temp))] <- "fitted"
      }
      #Reorder elements to have 'fitted' be first, then 'residuals'
      temp <- 
        temp[c("fitted", "residuals", 
                    names(temp)[which(!names(temp) %in% 
                                             c("fitted", "residuals"))])]
    }
    
    #Store results as requested
    if (return_fitobject) {
      fits_list[[i]] <- temp
    } else {
      #Fill in output if needed
      out[subset_by == unique(subset_by)[i]] <- temp[["fitted"]]
    }
  }
  
  #Return as requested
  if (return_fitobject == TRUE) {return(fits_list)} else {return(out)}
}


  
#' Moving average smoothing
#' 
#' This function uses a moving average to smooth data
#' 
#' @param formula Formula specifying the numeric response (density) 
#'                and numeric predictor (time).
#' @param data Dataframe containing variables in \code{formula}
#' @param window_width_n Number of data points wide the moving average window is
#'                     (therefore, must be an odd number of points)
#' @return Vector of smoothed data, with NA's appended at both ends
#' 
#' @export   
moving_average <- function(formula, data, window_width_n) {
  #Check window width
  if(window_width_n %% 2 == 0) {stop("window_width_n must be an odd number")}
  
  #Check formula formatting
  if (length(formula) < 3) {stop("No response variable specified")}
  if (length(formula[[3]]) > 1) {stop("Multiple predictors in formula")}
  
  #Parse formula
  response_var <- as.character(formula[[2]])
  predictor_var <- as.character(formula[[3]])
  
  #Check for vars in data
  stopifnot(response_var %in% colnames(data),
            predictor_var %in% colnames(data))
  
  if (!is.numeric(data[, predictor_var])) {
    warning(paste("data is being sorted by order(", predictor_var,
            "), but ", predictor_var, " is not numeric", sep = ""))
  }
  data <- data[order(data[, predictor_var]), ]
  
  if(!is.numeric(data[, response_var]) ) {
    warning(paste("Coercing", response_var, "to numeric"))
    data[, response_var] <- as.numeric(data[, response_var])
  }
  
  #Calculate moving average
  window_radius <- (window_width_n - 1)/2
  results <- c(rep(NA, window_radius),
               rep(0, (nrow(data)-2*window_radius)),
               rep(NA, window_radius))
  
  for (i in 1:window_width_n) {
    offset <- i - 1 - window_radius
    
    results[(1 + window_radius):(length(results) - window_radius)] <-
      results[(1 + window_radius):(length(results) - window_radius)] +
      data[(1 + window_radius + offset):
             (length(results) - window_radius + offset), 
           response_var]
  }
  results <- results/window_width_n

  return(results)
}

#' Moving median smoothing
#' 
#' This function uses a moving median to smooth data
#' 
#' @param formula Formula specifying the numeric response (density) 
#'                and numeric predictor (time).
#' @param data Dataframe containing variables in \code{formula}
#' @param window_width_n Number of data points wide the moving average window is
#'                     (therefore, must be an odd number of points)
#' @return Vector of smoothed data, with NA's appended at both ends
#' 
#' @export   
moving_median <- function(formula, data, window_width_n) {
  #Check window width
  if(window_width_n %% 2 == 0) {stop("window_width_n must be an odd number")}
  
  #Check formula formatting
  if (length(formula) < 3) {stop("No response variable specified")}
  if (length(formula[[3]]) > 1) {stop("Multiple predictors in formula")}
  
  #Parse formula
  response_var <- as.character(formula[[2]])
  predictor_var <- as.character(formula[[3]])
  
  #Check for vars in data
  stopifnot(response_var %in% colnames(data),
            predictor_var %in% colnames(data))
  
  if (!is.numeric(data[, predictor_var])) {
    warning(paste("data is being sorted by order(", predictor_var,
                  "), but ", predictor_var, " is not numeric", sep = ""))
  }
  data <- data[order(data[, predictor_var]), ]
  
  if(!is.numeric(data[, response_var]) ) {
    warning(paste("Coercing", response_var, "to numeric"))
    data[, response_var] <- as.numeric(data[, response_var])
  }
  
  #Calculate moving average
  window_radius <- (window_width_n - 1)/2
  results <- c(rep(NA, window_radius),
               rep(0, (nrow(data)-2*window_radius)),
               rep(NA, window_radius))
  
  for (i in (window_radius+1):(length(results) - window_radius)) {
    results[i] <- stats::median(data[(i - window_radius):(i + window_radius), 
                              response_var])
  }
  
  return(results)
}

# Derivatives ----

#' Calculate derivatives of vector of data
#' 
#' Provided a vector of y values, this function returns the difference
#' between sequential values, derivative between sequential values,
#' or per-capita derivative between sequential values
#' 
#' @param y       Data to calculate difference or derivative of
#' @param x Vector of x values provided as a simple numeric
#'          (e.g. if time, in number of seconds, not POSIX).
#' @param x_scale Factor to scale x by in denominator of derivative calculation
#'                
#'                When \code{x_scale = NA}, \code{calc_deriv} returns 
#'                differences in \code{y}
#'                
#'                When \code{x_scale = 1}, \code{calc_deriv} returns the 
#'                standard derivative in the same units as y/x
#'                
#'                For other units, set x_scale to the ratio of the the units of 
#'                x to the desired units. E.g. if x is in seconds, but the 
#'                desired derivative is in units of /minute, set 
#'                \code{x_scale = 60} (since there are 60 seconds in 1 minute).
#' @param percapita When percapita = TRUE, the differences of y are 
#'                  divided by y
#' @param subset_by if subset_by is provided, it should be a vector (same 
#'                  length as y), the unique values of which will 
#'                  separate calculations
#' @return A vector of values for the difference (if \code{x_scale = NA}, 
#'         derivative (if \code{x_scale} is a number), or per-capita derivative
#'         (if \code{percapita = TRUE}) between \code{y} values. Vector
#'         will be the same length as \code{y},  with \code{NA} appended 
#'         to the end
#' 
#' @export   
calc_deriv <- function(y, x = NULL, x_scale = 1,
                       percapita = FALSE, subset_by = NULL) {
  
  #Check inputs
  if (is.null(y)) {stop("y must be provided")}
  if (length(x_scale) > 1) {stop("x_scale must be NA or a single value")}
  if (!is.na(x_scale)) {
    if (!is.numeric(x_scale)) {stop("x_scale is not numeric")}
    if (is.null(x)) {stop("x_scale is specified, but x is not provided")}
  }
  if (!is.null(x) & !is.numeric(x)) {
    stop("x is not numeric")
  }
  if(!is.numeric(y)) {y <- as.numeric(y)}
  
  #Calc derivative
  ans <- rep(NA, length(y))
  if(is.null(subset_by)) {subset_by <- rep("A", length(y))}
  for (i in 1:length(unique(subset_by))) {
    indices <- which(subset_by == unique(subset_by)[i])
    sub_y <- y[indices]
    if(!is.null(x)) {
      sub_x <- x[indices]
      start_order <- order(sub_x) #so we can put things back at the end
      #Reorder
      sub_y <- sub_y[start_order]
      sub_x <- sub_x[start_order]
    } else {start_order <- 1:length(sub_y)}
    #Calculate differences
    sub_ans <- sub_y[2:length(sub_y)]-sub_y[1:(length(sub_y)-1)]
    #Percapita (if specified)
    if(percapita) {sub_ans <- sub_ans/sub_y[1:(length(sub_y)-1)]}
    #Derivative & rescale (if specified)
    if(!is.na(x_scale)) {
      sub_ans <- sub_ans/
        ((sub_x[2:length(sub_x)]-sub_x[1:(length(sub_x)-1)])/x_scale)
    }
    ans[indices] <- c(sub_ans[start_order])
  }
  return(ans)
}

# Analyze ----

#' Find limits of window for next extrema search (for internal use only)
#' 
#' @param cnt_pos Current position index to start search from
#' @param y Numeric vector of y values in which to identify local extrema
#'          (technically optional if only using width_limit_n, but since
#'          find_next_extrema will always provide it's required for simplicity)
#' @param x Optional numeric vector of corresponding x values
#' @param width_limit Width of the window (in units of \code{x}) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param width_limit_n Width of the window (in number of \code{y} values) used to
#'                    search for local extrema. A narrower width will be more
#'                    sensitive to narrow local maxima/minima, while a wider
#'                    width will be less sensitive to local maxima/minima.
#' @param height_limit The maximum change in \code{y} a single extrema-search
#'                     step is allowed to take.
#'                     For example, a maxima-finding function will not pass a
#'                     valley deeper than height_limit. This also limits
#'                     approaches to true extrema, so if it is set too small
#'                     the function may return non-extrema
#' 
#' @return Vector, of length two, with start index and end index of the
#'         next window to use
#'           
get_window_limits <- function(cnt_pos, y, x = NULL,
                              width_limit = NULL,
                              width_limit_n = NULL,
                              height_limit = NULL) {
  #Check inputs
  if (is.null(width_limit_n) & is.null(height_limit) & is.null(width_limit)) {
    stop("Either width_limit, width_limit_n, or height_limit must be provided")
  }
  if (!is.null(width_limit) & is.null(x)) {
    stop("width_limit is specified, but x is not provided")
  }
  
  #Define window limits
  #Define window start candidates
  window_start <- c(NA, NA, NA)
  if (!is.null(width_limit)) {
    window_start[1] <- max(c(1, min(which((x[cnt_pos] - width_limit/2) <= x))))
  }
  if (!is.null(width_limit_n)) { #using width limit_n
    window_start[2] <- max(c(1, cnt_pos-floor(width_limit_n/2)))
  }
  if (!is.null(height_limit)) { #using height limit
    #For startpoint height, we want the earliest point that is
    #before our current point and
    #within +/- height limit of all points between it and current point
    i <- cnt_pos-1
    while(i >= 1 &
          all(y[i] >= y[(i+1):cnt_pos] - height_limit) &
          all(y[i] <= y[(i+1):cnt_pos] + height_limit)) {
      i <- i-1
    }
    window_start[3] <- i+1
    
    #Make sure we're going at least 1 point backwards
    if(window_start[3] >= cnt_pos) {window_start[3] <- cnt_pos-1}
  }
  
  #Define window end candidates
  window_end <- c(NA, NA, NA)
  if (!is.null(width_limit)) {
    window_end[1] <- min(c(length(y), 
                           max(which((x[cnt_pos] + width_limit/2) >= x))))
  }
  if (!is.null(width_limit_n)) { #using width limit_n
    window_end[2] <- min(c(length(y), cnt_pos+floor(width_limit_n/2)))
  }
  if (!is.null(height_limit)) { #using height limit
    #For endpoint height, we want the latest point that is
    #after our current point and
    #within +/- height limit of all points between it and current point
    i <- cnt_pos+1
    while(i <= length(y) &
          all(y[i] >= y[(i-1):cnt_pos] - height_limit) &
          all(y[i] <= y[(i-1):cnt_pos] + height_limit)) {
      i <- i+1
    }
    window_end[3] <- i-1
    
    #Make sure we're going at least one point forwards
    if (window_end[3] <= cnt_pos) {window_end[3] <- cnt_pos+1}
  }
  #Return conservative of start candidates, end candidates
  return(c(max(window_start, na.rm = T), min(window_end, na.rm = T)))
}

#' Find the next extrema from the current position (for internal use only)
#' 
#' @param cnt_pos Current position index to start search from
#' @param width_limit Width of the window (in units of \code{x}) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param width_limit_n Width of the window (in number of \code{y} values) used to
#'                    search for local extrema. A narrower width will me more
#'                    sensitive to narrow local maxima/minima, while a wider
#'                    width will be less sensitive to local maxima/minima.
#' @param height_limit The maximum change in \code{y} a single extrema-search
#'                     step is allowed to take.
#'                     For example, a maxima-finding function will not pass a
#'                     valley deeper than height_limit. This also limits
#'                     approaches to true extrema, so if it is set too small
#'                     the function may return non-extrema
#' @param y Numeric vector of y values in which to identify local extrema
#' @param x Optional numeric vector of corresponding x values
#' @param looking_for  Is the next window looking for a "maxima" or a "minima"?
#' 
#' @return The index of the next extrema, of the same type as specified by
#'         looking_for
#'
find_next_extrema <- function(cnt_pos, y, x = NULL,
                              width_limit = NULL,
                              width_limit_n = NULL,
                              height_limit = NULL,
                              looking_for = c("minima", "maxima")) {
  if (cnt_pos == length(y)) {best_pos <- cnt_pos-1
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
                                       width_limit_n = width_limit_n,
                                       height_limit = height_limit,
                                       y = y, x = x)
      #Make sure we're not going backwards
      window_lims <- c(max(start_pos, window_lims[1]),
                       max(start_pos, window_lims[2]))
      #Then move current pointer to highest point within window
      # (making sure not to check non-integer indices, or indices below 1 or
      #  higher than the length of the vector)
      cnt_pos <- window_lims[1]-1+which.max(y[window_lims[1]:window_lims[2]])
    }
  ##Looking for next minima
  } else if (looking_for == "minima") {
    while (cnt_pos != best_pos) {
      #Move the previous best pointer to current pointer location
      best_pos <- cnt_pos
      #Get next window limits
      window_lims <- get_window_limits(cnt_pos = cnt_pos,
                                       width_limit = width_limit,
                                       width_limit_n = width_limit_n,
                                       height_limit = height_limit,
                                       y = y, x = x)
      #Make sure we're not going backwards
      window_lims <- c(max(start_pos, window_lims[1]),
                       max(start_pos, window_lims[2]))
      #Then move current pointer to lowest point within window
      # (making sure not to check non-integer indices, or indices below 1 or
      #  higher than the length of the vector)
      cnt_pos <- window_lims[1]-1+which.min(y[window_lims[1]:window_lims[2]])
    }
  }
  return(best_pos)
}


#' Find local extrema of numeric vector
#' 
#' This function takes a vector of \code{y} values and returns a vector
#' of the indices of all local value extrema (by default, this includes both
#' local minima and local maxima)
#' Either width_limit_n or height_limit must be provided
#' 
#' @details 
#' If multiple of width_limit, width_limit_n, and height_limit are provided, 
#' steps are limited conservatively (a single step must meet all criteria)
#' 
#' This function is designed to be compatible for use within
#'  dplyr::group_by and dplyr::summarize
#'   
#' @param y Numeric vector of y values in which to identify local extrema
#' @param x Optional numeric vector of corresponding x values
#' @param return One of c("index", "x", "y"), determining whether the function
#'               will return the index, x value, or y value associated with the
#'               identified extremas
#' @param return_maxima,return_minima Boolean for which classes of local extrema
#'                                    to return
#' @param return_endpoints Should the first and last values in \code{y}
#'                         be included if they are in the returned 
#'                         vector of extrema?
#' @param width_limit Width of the window (in units of \code{x}) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param width_limit_n The maximum number of data points a single 
#'                      extrema-search step is allowed to take. For example,
#'                      when maxima-finding, the function will not pass
#'                      a valley consisting of more than \code{width_limit_n}
#'                      data points.
#'                      
#'                      A smaller \code{width_limit_n} will be more sensitive 
#'                      to narrow local maxima/minima, while a larger 
#'                      \code{width_limit_n} will be less sensitive to 
#'                      narrow local maxima/minima.
#' @param height_limit The maximum change in \code{y} a single extrema-search
#'                     step is allowed to take.  For example, when 
#'                     maxima-finding, the function will not pass a
#'                     valley deeper than \code{height_limit}.
#'                     
#'                     A smaller \code{height_limit} will be more sensitive 
#'                     to shallow local maxima/minima, while a larger 
#'                     \code{height_limit} will be less sensitive to 
#'                     shallow maxima/minima.
#' @param na.rm Boolean whether NA's should be removed before analyzing
#' @return If \code{return = "index"}, a vector of indices corresponding 
#'           to local extrema in the data
#'           
#'         If \code{return = "x"}, a vector of x values corresponding
#'           to local extrema in the data
#'          
#'         If \code{return = "y"}, a vector of y values corresponding
#'           to local extrema in the data
#' 
#' @export                             
find_local_extrema <- function(y, x = NULL, return = "index",
                               return_maxima = TRUE,
                               return_minima = TRUE,
                               return_endpoints = TRUE,
                               width_limit = NULL,
                               width_limit_n = NULL,
                               height_limit = NULL,
                               na.rm = TRUE) {
  #Check inputs
  if (!return_maxima & !return_minima) {
    stop("Both return_maxima and return_minima are FALSE, at least one must be TRUE")
  }
  #Check inputs
  if (is.null(width_limit_n) & is.null(height_limit) & is.null(width_limit)) {
    stop("Either width_limit, width_limit_n, or height_limit must be provided")
  }
  if (!is.null(width_limit) & is.null(x)) {
    stop("width_limit is specified, but x is not provided")
  }
  if (!is.null(width_limit_n)) {
    if (width_limit_n%%2 == 0) {
      warning("width_limit_n must be odd, will use ", width_limit_n-1, " as width_limit_n")
      width_limit_n <- width_limit_n - 1
    }
  }
  if (!return %in% c("x", "y", "index")) {
    stop('return must be one of "x", "y", or "index"')
  }
  if(!is.null(x) & length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(is.null(x) & return == "x") {stop('return = "x" but x is not provided')}
  
  #Deal with NA's in y
  if(any(is.na(y))) {
    if(na.rm == TRUE) {
      nas_removed_indices <- which(is.na(y))
      y_orig <- y
      y <- y[!is.na(y)]
    } else {
      stop("Some of y are NA but na.rm = FALSE")
    }
  } else {nas_removed_indices <- NULL}
  
  #Start finding extrema
  cnt_pos <- 1
  ##Find first maxima
  maxima_list <- c(find_next_extrema(cnt_pos, y, x = x,
                                     width_limit = width_limit,
                                     width_limit_n = width_limit_n,
                                     height_limit = height_limit,
                                     looking_for = "maxima"))
  ##Find first minima
  minima_list <- c(find_next_extrema(cnt_pos, y, x = x,
                                     width_limit = width_limit,
                                     width_limit_n = width_limit_n,
                                     height_limit = height_limit,
                                     looking_for = "minima"))
  
  ##Check for next extrema until...
  while (TRUE) {
    #we're finding repeats
    if (any(duplicated(c(minima_list, maxima_list)))) {break}
    #or we hit the end of the y
    if (length(y) %in% c(maxima_list, minima_list)) {
      break
    }
    #Since maxima & minima must alternate, always start with furthest one 
    # we've found so far
    cnt_pos <- max(c(minima_list, maxima_list))
    #we're looking for a maxima next
    if (cnt_pos %in% minima_list) {
      maxima_list <- c(maxima_list,
                       find_next_extrema(cnt_pos, y, x = x,
                                         width_limit = width_limit,
                                         width_limit_n = width_limit_n,
                                         height_limit = height_limit,
                                         looking_for = "maxima"))
      #we're looking for a minima next
    } else if (cnt_pos %in% maxima_list) {
      minima_list <- c(minima_list,
                       find_next_extrema(cnt_pos, y, x = x,
                                         width_limit = width_limit,
                                         width_limit_n = width_limit_n,
                                         height_limit = height_limit,
                                         looking_for = "minima"))
    }
  }
  
  #Combine maxima & minima y & remove duplicates
  output <- c()
  if (return_maxima) {output <- c(output, maxima_list)}
  if (return_minima) {output <- c(output, minima_list)}
  #If remove endpoints is true, remove first or last y from return
  if (!return_endpoints) {
    if (1 %in% output) {output <- output[-which(output == 1)]}
    if (length(y) %in% output) {
      output <- output[-which(output == length(y))]}
  }
  #Remove duplicates
  output <- unique(output)
  #Order
  output <- output[order(output)]
  
  #Adjust indices for NAs that were removed
  if(na.rm & !is.null(nas_removed_indices)) {
    for (index in nas_removed_indices) {
      output[output >= index] <- (output[output >= index] + 1)
    }
  }
  
  #Return as specified
  if (return == "index") {
    return(output)
  } else if (return == "x") {
    return(x[output])
  } else if (return == "y") {
    return(y_orig[output])
  }
}

#' Find the first local peak of a numeric vector
#' 
#' This function takes a vector of \code{y} values and returns the index
#' (by default) of the first local maxima. It serves as a wrapper function
#' for \code{find_local_extrema}
#' 
#' @param y Numeric vector of y values in which to identify local extrema
#' @param x Optional numeric vector of corresponding x values
#' @param return One of c("index", "x", "y"), determining whether the function
#'               will return the index, x value, or y value associated with the
#'               first peak in y values
#' @param width_limit_n Width of the window (in number of \code{y}) used to
#'                    search for local extrema. A narrower width will me more
#'                    sensitive to local maxima/minima, while a wider
#'                    width will be less sensitive to local maxima/minima.
#'                    
#'                    If not provided, defaults to ~0.2*length(y)
#' @param ... Other parameters to pass to \code{find_local_extrema}
#' 
#' @details 
#' This function is designed to be compatible for use within
#'  dplyr::group_by and dplyr::summarize
#'                    
#' @export      
first_peak <- function(y, x = NULL, return = "index", width_limit_n = NULL, ...) {
  if(is.null(width_limit_n)) {
    width_limit_n <- round(0.2*length(y)) - (1 - floor(0.2*length(y))%%2)
  }
  
  if (!return %in% c("x", "y", "index")) {
    stop('return must be one of "x", "y", or "index"')
  }
  if(!is.null(x) & length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(is.null(x) & return == "x") {stop('return = "x" but x is not provided')}
  
  dot_args <- list(...)
  if (any(c("return_maxima", "return_minima") %in% names(dot_args))) {
    stop("return_maxima and return_minima cannot be changed in first_peak,
    please use find_local_extrema for more flexibility")
  }
  
  out <- find_local_extrema(y = y,
                            return_maxima = TRUE,
                            return_minima = FALSE,
                            width_limit_n = width_limit_n,
                            ...)
  if (return == "index") {
    return(out[1])
  } else if (return == "x") {
    return(x[out[1]])
  } else if (return == "y") {
    return(y[out[1]])
  }
}

#' Find the first point when a numeric vector falls below some threshold
#' 
#' This function takes a vector of \code{x} and \code{y} values and 
#' returns the index (by default) of the first point that falls
#' below some threshold y value.
#' 
#' @param y Numeric vector of y values in which to identify first point
#'          below some threshold
#' @param x Optional numeric vector of corresponding x values
#' @param threshold Threshold y value of interest
#' @param return One of c("index", "x"), determining whether the function
#'               will return the index or x value associated with the
#'               first y value below some threshold
#' @param subset A vector of Boolean values indicating which x and y values
#'               should be included (TRUE) or excluded (FALSE).
#'               
#'               If \code{return = "index"}, index will be for the whole 
#'               vector and not the subset of the vector
#'               
#' @details 
#' This function is designed to be compatible for use within
#'  dplyr::group_by and dplyr::summarize
#'                    
#' @export    
first_below <- function(y, x = NULL, threshold, 
                        return = "index", subset = NULL) {
  if (!return %in% c("x", "index")) {
    stop('return must be "x" or "index"')
  }
  if(!is.null(x) & length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(return == "x" & is.null(x)) {stop("return = x but x is not provided")}
  
  if(any(is.na(suppressWarnings(as.numeric(y))))) {stop("y must be numeric")}
  if(!is.null(x)) {
    if(any(is.na(suppressWarnings(as.numeric(x))))) {stop("x must be numeric")}
    y <- y[order(x)]
    x <- x[order(x)]
  }
  
  #Apply subset
  if(is.null(subset)) {indices = 1:length(y)
  } else {indices <- which(subset)}
  if(!is.null(x)) {x <- x[indices]}
  y <- y[indices]
  
  #Find first below index
  if(any(y < threshold)) {
    out_idx <- min(which(y <= threshold))
  } else {out <- NA}
  
  #Return as appropriate
  if(return == "index") {
    return(indices[out_idx])
  } else if (return == "x") {
    #Use linear interpolation to find exact extinction time
    x1 <- x[(out_idx-1)]
    x3 <- x[out_idx]
    y1 <- y[(out_idx-1)]
    y3 <- y[out_idx]
    y2 <- threshold
    
    x2 <- (y2-y1)*(x3-x1)/(y3-y1) + x1
    return(x2)
  }
}


#' calculate area under the curve
#' 
#' This function takes a vector of \code{x} and \code{y} values
#' and returns a scalar for the area under the curve, calculated using 
#' the trapezoid rule
#'  
#' @param x Numeric vector of x values
#' @param y Numeric vector of y values
#' @param xlim Vector, of length 2, delimiting the x range over which the
#'             area under the curve should be calculated (where NA can be
#'             provided for the area to be calculated from the start or to
#'             the end of the data)
#' @param na.rm a logical indicating whether missing values should be removed
#' 
#' @details 
#' This function is designed to be compatible for use within
#'  dplyr::group_by and dplyr::summarize
#'
#' @return A scalar for the total area under the curve
#'             
#' @export
auc <- function(x, y, xlim = NULL, na.rm = TRUE) {
  if(!na.rm & any(c(is.na(x), is.na(y)))) {
    stop("na.rm = FALSE but x or y contain NA's")
  }
  if(!is.vector(x)) {
    stop(paste("x is not a vector, it is class:", class(x)))
  }
  if(!is.vector(y)) {
    stop(paste("y is not a vector, it is class:", class(y)))
  }

  to_keep <- which(!(is.na(x) | is.na(y)))
  x <- x[to_keep]
  y <- y[to_keep]
  stopifnot(order(x) == 1:length(x),
            length(x) > 1, length(y) > 1)
  
  #Check if xlim has been specified
  if(!is.null(xlim)) {
    stopifnot(is.vector(xlim), length(xlim) == 2, any(!is.na(xlim)))
    if(is.na(xlim[1])) {xlim[1] <- x[1]}
    if(is.na(xlim[2])) {xlim[2] <- x[length(x)]}
    if(xlim[1] < x[1]) {
      warning("xlim specifies lower limit below the range of x")
      xlim[1] <- x[1]
    } else { #add lower xlim to the x vector and the interpolated y to y vector
      if (!(xlim[1] %in% x)) {
        xndx <- max(which(xlim[1] > x))
        slp <- (y[xndx] - y[xndx+1])/(x[xndx] - x[xndx+1])
        x <- c(x, xlim[1])
        #interpolated_y = m * interpolation_x + b
        #   m = (y_1 - y_2)/(x_1 - x_2)
        #   b = y_1 - x_1 * m
        y <- c(y, xlim[1]*slp + y[xndx] - x[xndx]*slp)
        y <- y[order(x)]
        x <- x[order(x)]
      }
    }
       
    if(xlim[2] > x[length(x)]) {
      warning("xlim specifies upper limit above the range of x")
      xlim[2] <- x[length(x)]
    } else { #add upper xlim to the x vector and the interpolated y to y vector
      if (!(xlim[2] %in% x)) {
        xndx <- max(which(xlim[2] > x))
        slp <- (y[xndx] - y[xndx+1])/(x[xndx] - x[xndx+1])
        x <- c(x, xlim[2])
        #interpolated_y = m * interpolation_x + b
        #   m = (y_1 - y_2)/(x_1 - x_2)
        #   b = y_1 - x_1 * m
        y <- c(y, xlim[2]*slp + y[xndx] - x[xndx]*slp)
        y <- y[order(x)]
        x <- x[order(x)]
      }
    }
    y <- y[(x >= xlim[1]) & (x <= xlim[2])]
    x <- x[(x >= xlim[1]) & (x <= xlim[2])]
  }
  
  #Calculate auc
  # area = 0.5 * (y1 + y2) * (x2 - x1)
  return(sum(0.5 * 
               (y[1:(length(y)-1)] + y[2:length(y)]) *
               (x[2:length(x)] - x[1:(length(x)-1)])))
}


##Legacy Code ----

#' Turn tidydesign into block format
#'
#' This function allows users to convert designs created with tidydesign
#'  into a block format for easy output to csv for inclusion in lab notebooks,
#'  etc in a human-readable format
#' @param tidydesign A tidydesign data.frame (e.g. as created by make_tidydesign)
#' @param collapse NULL or a string to use for concatenating design elements
#'                 together. If NULL each design column will be put into its
#'                 own block. If a string, that string will be used to \code{paste}
#'                 together all design elements and all design elements will
#'                 be returned in a single block
#' @param wellnames_sep A string used when concatenating rownames and column
#'                      names to create well names
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names
#' @return A list of blockdesign data.frames (if \code{collapse} is not
#'         \code{NULL} the list is of length 1
#'
block_tidydesign <- function(tidydesign, collapse = NULL,
                             wellnames_sep = "_", wellnames_colname = "Well") {
  
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


#' Write block designs to csv
#' 
#' This function is basically just a wrapper for write.csv that also works
#' when handed a list of matrices/dataframes
#' If \code{designs} is a list, \code{names(designs)} will be placed in the
#' 1,1 cell of each block
#' 
#' @param designs data.frame or matrix (if one design), or list of data.frames
#'                or matrices, to be written to file
#' @param file The filename (if a single design), or vector of file names
#'             (if multiple designs)
#' @param ... Other arguments passed to \code{write.csv}
#' @return Nothing, but R objects are written to files
#' 
write_blockdesign <- function(designs, file, ...) {
  #Basically just a wrapper for write.csv when handed a list of matrices/dataframes
  #Also puts the names of the designs in 1,1 cell (as is the case for block designs
  if (is.data.frame(designs)) {
    utils::write.csv(x = designs, file = file, ...)
  } else if (is.list(designs)) {
    for (i in 1:length(designs)) {
      tmp <- cbind(data.frame(row.names(designs[[i]])),
                   designs[[i]])
      colnames(tmp)[1] <- names(designs)[i]
      utils::write.csv(x = tmp, file = file[i], row.names = FALSE, ...)
    }   
  }
}