
# Read functions ----

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
#' @noRd
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
      srsc_vec <- c(startrow, startcol)
      srsc_vec[is.na(srsc_vec)] <- 1
      if (df[srsc_vec[1], srsc_vec[2]] == "" | is.na(df[srsc_vec[1], srsc_vec[2]])) {
        output$rownames_col <- srsc_vec[2]
        output$startcol <- srsc_vec[2] + 1
      } else {
        output$rownames_col <- 0
        output$startcol <- srsc_vec[2]
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
      srsc_vec <- c(startrow, startcol)
      srsc_vec[is.na(srsc_vec)] <- 1
      if (df[srsc_vec[1], srsc_vec[2]] == "" | is.na(df[srsc_vec[1], srsc_vec[2]])) {
        output$colnames_row <- srsc_vec[1]
        output$startrow <- srsc_vec[1] + 1
      } else {
        output$colnames_row <- 0
        output$startrow <- srsc_vec[1]
      }
    }
  }
  if (is.na(header) & is.na(sider)) {
    srsc_vec <- c(startrow, startcol)
    srsc_vec[is.na(srsc_vec)] <- 1
    if (df[srsc_vec[1], srsc_vec[2]] == "" | is.na(df[srsc_vec[1], srsc_vec[2]])) {
      output$colnames_row <- srsc_vec[1]
      output$startrow <- srsc_vec[1] + 1
      output$rownames_col <- srsc_vec[2]
      output$startcol <- srsc_vec[2] + 1
    } else {
      output$colnames_row <- 0
      output$startrow <- srsc_vec[1]
      output$rownames_col <- 0
      output$startcol <- srsc_vec[2]
    }
  }
  
  if(any(is.na(output))) {stop("infer_names failed to infer rows/cols")}
  
  #0's should be replaced with NA's
  output[output == 0] <- NA
  
  return(output)
}


#' An internal function that handles checking and inferring (from extension) filetypes
#' 
#' @param filetype The vector of filetypes (often extensions) or NULL
#' @param files The vector of filenames/paths
#' @param needed_len Parameter to pass to \code{check_input_dimensions} for
#'                   desired length of output vector
#' @param needed_name Parameter to pass to \code{check_input_dimensions} for
#'                    name of desired length for error message
#' @param ... Other arguments to pass to \code{check_input_dimensions}, most
#'            frequently \code{needed_name} for what the desired length 
#'            corresponds to (e.g. number of files)
#' 
#' @return vector of filetypes
#' 
#' @noRd
infer_check_filetype <- function(filetype, files, needed_len, needed_name, ...) {
  valid_exts <- c("tbl", "table", "csv", "csv2", 
                  "delim", "delim2", "xls", "xlsx")
  
  #Determine filetype(s)
  if (is.null(filetype)) {
    filetype <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
                        USE.NAMES = FALSE)
    if(any(!filetype %in% valid_exts)) {
      warning("filetype inferred but not one of the valid values. Will treat as tbl\n")
      filetype[!filetype %in% valid_exts] <- "tbl"
    }
  } else {
    filetype <- check_input_dimensions(filetype, "filetype", needed_len, needed_name)
    if(any(!filetype %in% valid_exts)) {
      stop("filetype provided by user must be one of the valid values")
    }
  }
  return(filetype)
}


#' An internal function that handles parsing of filestrings for saving
#' 
#' @param filestrings The vector of file/path strings
#' @param keep_dot Should the leading dot and slash be kept and returned
#' @param keep_path Should the path to the file be kept and returned
#' @param keep_ext Should the file extension be kept and returned
#' 
#' @return Vector of filestrings modified accordingly
#' 
#' @noRd
parse_filestrings <- function(filestrings, keep_dot, keep_path, keep_ext) {
  #infer the names from filenames
  for (i in 1:length(filestrings)) {
    if(!keep_dot) { #strip off the dot and leading slash from the beginning
      filestrings[i] <- sub("^\\./", "", filestrings[i])
    }
    if(!keep_path) { #strip off the path from the beginning
      filestrings[i] <- sub("(^\\.?/?).*/([^/]*$)", "\\1\\2", filestrings[i])
    }
    if(!keep_ext) { #strip off the extension from the end
      filestrings[i] <- sub("(^.*)\\.[[:alnum:]]+$", "\\1", filestrings[i])
    }
  }
  return(filestrings)
}

#' An internal function that handles reading a file in table format
#' 
#' @param file The filename or path
#' @param filetype The type of file, one of:
#'                 'tbl', 'table', 'csv', 'xls', 'xlsx', 'csv2', 'delim',
#'                 'delim2'
#' @param na.strings Strings to be interpreted as \code{NA}
#' @param sheet What sheet to read (only needed for 'xls' and 'xlsx')
#' 
#' @details None of the specified arguments should be a vector, they should
#'          all be single values
#' 
#' @return The \code{data.frame} resulting from reading the file
#' 
#' @noRd
read_gcfile <- function(file, filetype, na.strings, sheet = NULL, ...) {
  if(any(filetype %in% c("xls", "xlsx")) && 
     !requireNamespace("readxl", quietly = TRUE)) {
    stop("Package \"readxl\" must be installed to read xls or xlsx files",
         call. = FALSE)
  }
  
  if(filetype %in% c("tbl", "table", "csv", "csv2", "delim", "delim2")) {
    if("colClasses" %in% names(list(...))) {
      warning("specified colClasses is being ignored, read_gcfile always uses colClasses = 'character'")}
  } else if (filetype %in% c("xls", "xlsx")) {
    if("col_types" %in% names(list(...))) {
      warning("specified col_types is being ignored, read_gcfile always uses col_types = 'text'")}
  } else {warning("filetype not checked by read_gcfile, report this bug to gcplyr maintainer")}
  
  if (filetype == "tbl" | filetype == "table") {
    readgcfile_temp <- parse_dots(utils::read.table, file = file,
                        na.strings = na.strings, colClasses = "character",
                        ...)
  } else if (filetype == "csv") {
    #define defaults (this re-creates the behavior of read.csv, but allows
    # behavior to be overridden by user if desired)
    sep <- check_dots("sep", ",", ...)
    quote <- check_dots("quote", "\"", ...)
    dec <- check_dots("dec", ".", ...)
    fill <- check_dots("fill", TRUE, ...)
    comment.char <- check_dots("comment.char", "", ...)
    
    readgcfile_temp <- parse_dots(utils::read.table, file = file, 
                        colClasses = "character", header = FALSE,
                        na.strings = na.strings, sep = sep,
                        quote = quote, dec = dec, fill = fill,
                        comment.char = comment.char, ...)
  } else if (filetype == "csv2") {
    #define defaults (this re-creates the behavior of read.csv2, but allows
    # behavior to be overridden by user if desired)
    sep <- check_dots("sep", ";", ...)
    quote <- check_dots("quote", "\"", ...)
    dec <- check_dots("dec", ",", ...)
    fill <- check_dots("fill", TRUE, ...)
    comment.char <- check_dots("comment.char", "", ...)
    
    readgcfile_temp <- parse_dots(utils::read.table, file = file, 
                        colClasses = "character", header = FALSE,
                        na.strings = na.strings, sep = sep,
                        quote = quote, dec = dec, fill = fill,
                        comment.char = comment.char, ...)
  } else if (filetype == "delim") {
    #define defaults (this re-creates the behavior of read.delim, but allows
    # behavior to be overridden by user if desired)
    sep <- check_dots("sep", "\t", ...)
    quote <- check_dots("quote", "\"", ...)
    dec <- check_dots("dec", ".", ...)
    fill <- check_dots("fill", TRUE, ...)
    comment.char <- check_dots("comment.char", "", ...)
    
    readgcfile_temp <- parse_dots(utils::read.table, file = file, 
                        colClasses = "character", header = FALSE,
                        na.strings = na.strings, sep = sep,
                        quote = quote, dec = dec, fill = fill,
                        comment.char = comment.char, ...)
  } else if (filetype == "delim2") {
    #define defaults (this re-creates the behavior of read.delim2, but allows
    # behavior to be overridden by user if desired)
    sep <- check_dots("sep", "\t", ...)
    quote <- check_dots("quote", "\"", ...)
    dec <- check_dots("dec", ",", ...)
    fill <- check_dots("fill", TRUE, ...)
    comment.char <- check_dots("comment.char", "", ...)
    
    readgcfile_temp <- parse_dots(utils::read.table, file = file, 
                        colClasses = "character", header = FALSE,
                        na.strings = na.strings, sep = sep,
                        quote = quote, dec = dec, fill = fill,
                        comment.char = comment.char, ...)
  } else if (filetype == "xls") {
    suppressMessages(
      readgcfile_temp <- 
        as.data.frame(
          parse_dots(readxl::read_xls, path = file, 
                      col_names = FALSE, col_types = "text", 
                      sheet = sheet, na = na.strings, ...)))
  } else if (filetype == "xlsx") {
    suppressMessages(
      readgcfile_temp <- 
        as.data.frame(
          parse_dots(readxl::read_xlsx, path = file, 
                      col_names = FALSE, col_types = "text", 
                      sheet = sheet, na = na.strings, ...)))
  } else {stop("read_gcfile was passed an invalid filetype")}
  return(readgcfile_temp)
}


#' A function that gets metadata from a row,col. Gives a warning if out of range
#' returns NA, otherwise returns the value
#' 
#' @param df The dataframe containing metadata at row,col
#' @param row The row where metadata should be
#' @param col The column where metadata should be
#' 
#' @return The metadata, or \code{NA} with a warning if out of range
#' 
#' @noRd
get_metadata <- function(df, row, col) {
  #Convert from Excel-formatting if needed
  if(!canbe.numeric(row)) {row <- from_excel(row)}
  if(!canbe.numeric(col)) {col <- from_excel(col)}
  row <- as.numeric(row)
  col <- as.numeric(col)
  
  if(row > nrow(df) || col > ncol(df)) {
    warning(paste("metadata in row", row, "column", col, "is out of range"))
    return(NA)
  } else {return(df[row, col])}
}


#' Read blockmeasures
#' 
#' A function that reads block measures into the R environment
#' 
#' @param files A vector of filepaths relative to the current working directory
#'              where each filepath is a single plate read
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#'                  "csv", "xls", or "xlsx".
#'                  
#'                  "tbl" or "table" to use \code{read.table} to read the file,
#'                  "csv2" to use \code{read.csv2}, "delim" to 
#'                  use \code{read.delim}, or "delim2" to use \code{read.delim2}.
#'                  
#'                  If none provided, \code{read_blocks} will infer filetype(s) 
#'                  from the extension(s) in \code{files}. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the measures data are located in \code{files}.
#'                 
#'                 Can be a vector or list the same length as \code{files}, or
#'                 a single value that applies to all \code{files}. Values
#'                 can be numeric or a string that will be automatically
#'                 converted to numeric by \code{from_excel}.
#'                 
#'                 If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param sheet (optional) If data is in .xls or .xlsx files, which sheet it 
#'                 is located on. Defaults to the first sheet if not specified
#' @param metadata (optional) non-spectrophotometric data that should be 
#'                 associated with each read blockmeasures. A named list where 
#'                 each item in the list is either: a vector of length 2, or
#'                 a list containing two vectors. 
#'                 
#'                 In the former case, each vector should provide the row and 
#'                 column where the metadata is located in all of the
#'                 blockmeasures input files.
#'                 
#'                 In the latter case, the first vector should provide the rows
#'                 where the metadata is located in each of the corresponding
#'                 input files, and the second vector should provide the 
#'                 columns where the metadata is located in each of the
#'                 corresponding input files. (This case is typically used 
#'                 when reading multiple blocks from a single file.)
#' @param block_names (optional) vector of names corresponding to each plate
#'                 in \code{files}. If not provided, block_names are inferred
#'                 from the filenames
#' @param block_names_header The name of the metadata field containing the
#'                          \code{block_names}
#' @param block_names_dot If block_names are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param block_names_path If block_names are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param block_names_ext If block_names are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param header   \code{TRUE}, \code{FALSE}, or \code{NA}, or a vector of
#'                 such values, indicating whether the file(s) contains the
#'                 column names as its first line. If \code{header = NA}
#'                 will attempt to infer the presence of column names. If
#'                 \code{header = FALSE} or no column names are inferred when 
#'                 \code{header = NA}, column names will be generated
#'                 automatically according to \code{wellnames_numeric}
#' @param sider    \code{TRUE}, \code{FALSE}, or \code{NA}, or a vector of
#'                 such values, indicating whether the file(s) contains the
#'                 row names as its first column. If \code{sider = NA}
#'                 will attempt to infer the presence of row names. If
#'                 \code{sider = FALSE} or no row names are inferred when 
#'                 \code{sider = NA}, row names will be generated
#'                 automatically according to \code{wellnames_numeric}
#' @param wellnames_numeric If row names and column names are not provided in the
#'                        input dataframe as specified by \code{header}
#'                        and \code{sider}, then names will be generated
#'                        automatically according to \code{wellnames_numeric}.
#'                        
#'                        If \code{wellnames_numeric} is TRUE, rows and columns
#'                        will be numbered with "R" and "C" prefixes, respectively.
#'                        
#'                        If \code{wellnames_numeric} is FALSE, rows will be
#'                        lettered A through Z, while columns will be numbered
#' @param na.strings A character vector of strings which are to be interpreted
#'                   as \code{NA} values by \code{utils::read.csv},
#'                   \code{readxl::read_xls}, \code{readxl::read_xlsx},
#'                   or \code{utils::read.table}
#' @param extension Allowed for backward compatibility; \code{filetype} is
#'                  now the preferred argument name.
#' @param block_name_header Allowed for backward compatibility; 
#'               \code{block_names_header} is now the preferred argument name.
#' @param ...   Other arguments passed to \code{utils::read.csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx},
#'              or \code{utils::read.table}
#'
#' @details 
#'  For metadata, \code{read_blocks} can handle an arbitrary number of additional
#'  pieces of information to extract from each blockcurve file as metadata.
#'  These pieces of information are specified as a named list of vectors
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
#' @return A list where each entry is a list containing the block data frame
#'         followed by the block_names (or filenames, if block_names is not 
#'         provided) and any specified metadata.
#'
#' @export     
read_blocks <- function(files, filetype = NULL, 
                        startrow = NULL, endrow = NULL, 
                        startcol = NULL, endcol = NULL,
                        sheet = NULL, metadata = NULL,
                        block_names = NULL,
                        block_names_header = "block_name",
                        block_names_dot = FALSE,
                        block_names_path = TRUE, block_names_ext = FALSE,
                        header = NA, sider = NA,
                        wellnames_numeric = FALSE,
                        na.strings = c("NA", ""),
                        extension, block_name_header,
                        ...) {
  
  if(!base::missing(extension)) {
    if(!base::missing(filetype)) {
      warning("Ignoring extension, using filetype")
    } else {filetype <- extension}
  }
  if(!base::missing(block_name_header)) {
    if(!base::missing(block_names_header)) {
      warning("Ignoring block_name_header, using block_names_header")
    } else {block_names_header <- block_name_header}
  }
  
  nblocks <- max(length(files), length(startrow), length(endrow),
                 length(startcol), length(endcol), length(sheet),
                 na.rm = TRUE)
  
  files <- check_input_dimensions(files, "files", nblocks, "the number of blocks")
  
  if(!is.null(startrow) & !all(is.numeric(startrow))) {
    startrow[!is.numeric(startrow)] <- from_excel(startrow[!is.numeric(startrow)])
    startrow <- as.numeric(startrow)
  }
  if(is.null(startrow)) {startrow <- rep(NA, nblocks)} 
  startrow <- check_input_dimensions(startrow, "startrow", nblocks, 
                              "the number of blocks")
  
  if(!is.null(endrow) & !all(is.numeric(endrow))) {
    endrow[!is.numeric(endrow)] <- from_excel(endrow[!is.numeric(endrow)])
    endrow <- as.numeric(endrow)
  }
  if(is.null(endrow)) {endrow <- rep(NA, nblocks)}
  endrow <- check_input_dimensions(endrow, "endrow", nblocks, "the number of blocks")
  
  if(!is.null(startcol) & !all(is.numeric(startcol))) {
    startcol[!is.numeric(startcol)] <- from_excel(startcol[!is.numeric(startcol)])
    startcol <- as.numeric(startcol)
  }
  if(is.null(startcol)) {startcol <- rep(NA, nblocks)}
  startcol <- check_input_dimensions(startcol, "startcol", nblocks, 
                              "the number of blocks")
  
  if(!is.null(endcol) & !all(is.numeric(endcol))) {
    endcol[!is.numeric(endcol)] <- from_excel(endcol[!is.numeric(endcol)])
    endcol <- as.numeric(endcol)
  }
  if(is.null(endcol)) {endcol <- rep(NA, nblocks)}
  endcol <- check_input_dimensions(endcol, "endcol", nblocks, "the number of blocks")
  
  if (!is.null(sheet)) {
    sheet <- check_input_dimensions(sheet, "sheet", nblocks, "the number of blocks")
  }
  header <- check_input_dimensions(header, "header", nblocks, "the number of blocks")
  sider <- check_input_dimensions(sider, "sider", nblocks, "the number of blocks")
  
  if (!is.null(block_names) & length(block_names) != nblocks) {
    stop("block_names must be the same length as the number of blocks")
  }
  
  if(!is.null(metadata) & any(names(metadata) == "")) {
    stop("not all metadata have names")
  }
  
  #Determine filetype(s)
  filetype <- infer_check_filetype(
    filetype = filetype, files = files,
    needed_len = nblocks, needed_name = "the number of blocks")
  
  #Check metadata for any list entries, if there are and they're not
  # the right length, pass error. Otherwise, replicate as needed
  if (!is.null(metadata)) {
    for (i in 1:length(metadata)) {
      if(length(metadata[[i]]) != 2) {
        stop(paste("metadata", names(metadata)[i], "is not a vector or a list of length 2"))
      }
      if(is.list(metadata[[i]])) {
        metadata[[i]][[1]] <- 
          check_input_dimensions(metadata[[i]][[1]], names(metadata)[i],
                          nblocks, "the number of blocks")
        metadata[[i]][[2]] <- 
          check_input_dimensions(metadata[[i]][[2]], names(metadata)[i],
                          nblocks, "the number of blocks")
      }
    }
  }
  
  #Create empty list for read-in block measures
  if (is.null(metadata)) { #there is no user-specified metadata
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = stats::setNames("NA", block_names_header))), 
                   nblocks)
  } else { #there is user-specified metadata
    metadata_vector <- rep(NA, times = length(metadata)+1)
    names(metadata_vector) <- c(block_names_header, names(metadata))
    #Okay so the goal here is to have each block measures returned as an item in a big list
    #each item will itself be a named list with 2 things: "data" and "metadata"
    #data is just the dataframe (with colnames & rownames inferred or not)
    #within metadata there will at least be "name" for the block measures filename
    #but users can specify other metadata they would like extracted 
    # (with a named list of c(row, column) combinations)
    outputs <- rep(list(list("data" = NA, 
                             "metadata" = metadata_vector)), 
                   nblocks)
  }
  
  #Import data
  for (i in 1:nblocks) {
    rawfile <- read_gcfile(file = files[i], filetype = filetype[i],
                        na.strings = na.strings, sheet = sheet[i], ...)
    
    #Infer rows, cols, rownames, colnames
    inferred_rc <- 
      infer_names(rawfile, startrow = startrow[i], endrow = endrow[i],
                  startcol = startcol[i], endcol = endcol[i],
                  header = header[i], sider = sider[i])
    
    if(inferred_rc$startrow < 1 || inferred_rc$endrow > nrow(rawfile) ||
       inferred_rc$startcol < 1 || inferred_rc$endcol > ncol(rawfile)) {
      stop("Startrow, startcol, endrow, or endcol are out of range for the file")}
    
    #Save information to outputs
    outputs[[i]]$data <- rawfile[inferred_rc$startrow:inferred_rc$endrow,
                              inferred_rc$startcol:inferred_rc$endcol]
    
    #If temp_colnames or temp_rownames haven't been inferred, number them
    if (is.na(inferred_rc$colnames_row)) {
      if(wellnames_numeric) {
        temp_colnames <- paste0("C", 1:ncol(outputs[[i]]$data))
      } else {temp_colnames <- 1:ncol(outputs[[i]]$data)}
    } else {
      temp_colnames <- rawfile[inferred_rc$colnames_row, 
                            inferred_rc$startcol:inferred_rc$endcol]
    }
    if (is.na(inferred_rc$rownames_col)) {
      if(wellnames_numeric) {
        temp_rownames <- paste0("R", 1:nrow(outputs[[i]]$data))
      } else {temp_rownames <- to_excel(1:nrow(outputs[[i]]$data))}
    } else {
      temp_rownames <- rawfile[inferred_rc$startrow:inferred_rc$endrow, 
                            inferred_rc$rownames_col]
    }
    
    #Assign rownames and colnames from temp_variables
    colnames(outputs[[i]]$data) <- temp_colnames
    rownames(outputs[[i]]$data) <- temp_rownames
    
    ##Add metadata
    #Add filenames to metadata
    if (!is.null(block_names)) { #block_names were provided
      outputs[[i]]$metadata[block_names_header] <- block_names[i]
    } else { #block_names were not provided, infer from filename
      outputs[[i]]$metadata[block_names_header] <- 
        parse_filestrings(
          files[i], keep_dot = block_names_dot, 
          keep_path = block_names_path, keep_ext = block_names_ext)
    }
    #Add user-specified metadata (if any)
    if (!is.null(metadata)) {
      for (j in 1:length(metadata)) {
        if(!is.list(metadata[[j]])) { #metadata item is a vector
          outputs[[i]]$metadata[j+1] <- 
            get_metadata(df = rawfile, row = metadata[[j]][1],
                         col = metadata[[j]][2])
        } else { #metadata item is a list (presumably of two vectors)
          #the first vector is the rows for each ith block
          #the second vector is the columns for each ith block
          outputs[[i]]$metadata[j+1] <- 
            get_metadata(df = rawfile, row = metadata[[j]][[1]][i],
                         col = metadata[[j]][[2]][i])
        }
      }
    }
  }
  
  ##Error checking for output dataframe dimensions
  if (length(outputs) > 1 &&
      !all_same(sapply(outputs, simplify = TRUE, 
                       FUN = function(x) {dim(x$data)[1]}))) {
    warning("Not all blockmeasures have the same number of rows of data\n")
  }
  if (length(outputs) > 1 &&
      !all_same(sapply(outputs, simplify = TRUE,
                       FUN = function(x) {dim(x$data)[2]}))) {
    warning("Not all blockmeasures have the same number of columns of data\n")
  }
  
  return(outputs)
}

#' Read wides
#' 
#' A function that imports widemeasures in files into the R environment
#' 
#' @details
#' startrow, endrow, startcol, endcol, timecol, sheet and filetype 
#' can either be a single value that applies for all files or
#' vectors or lists the same length as \code{files}, 
#' 
#' @param files A vector of filepaths (relative to current working directory)
#'              where each one is a widemeasures set of data
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#'                  "csv", "xls", or "xlsx".
#'                  
#'                  "tbl" or "table" to use \code{read.table} to read the file,
#'                  "csv2" to use \code{read.csv2}, "delim" to 
#'                  use \code{read.delim}, or "delim2" to use \code{read.delim2}.
#'                  
#'                  If none provided, \code{read_wides} will infer filetype(s) 
#'                  from the extension(s) in \code{files}. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the data are located in \code{files}.
#'                 
#'                 Can be a vector or list the same length as \code{files}, or
#'                 a single value that applies to all \code{files}. Values
#'                 can be numeric or a string that will be automatically
#'                 converted to numeric by \code{from_excel}.
#'                 
#'                 If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param header logical for whether there is a header in the data. If FALSE
#'               columns are simply numbered. If TRUE, the first row of the
#'               data (\code{startrow} if specified) is used 
#'               as the column names
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#'              sheet
#' @param run_names Names to give the widemeasures read in. By default uses the
#'                   file names if not specified
#' @param run_names_header Should the run names (provided in \code{run_names}
#'                     or inferred from \code{files}) be added as a column to 
#'                     the widemeasures? If \code{run_names_header} is NULL, 
#'                     they will not be. If \code{run_names_header} is a string, 
#'                     that string will be the column header for the column 
#'                     where the names will be stored
#' @param run_names_dot If run_names are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param run_names_path If run_names are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param run_names_ext If run_names are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param metadata (optional) non-spectrophotometric data that should be 
#'                 associated with each read widemeasures. A named list where 
#'                 each item in the list is either: a vector of length 2, or
#'                 a list containing two vectors. 
#'                 
#'                 In the former case, each vector should provide the row and 
#'                 column where the metadata is located in all of the
#'                 blockmeasures input files.
#'                 
#'                 In the latter case, the first vector should provide the rows
#'                 where the metadata is located in each of the corresponding
#'                 input files, and the second vector should provide the 
#'                 columns where the metadata is located in each of the
#'                 corresponding input files. (This case is typically used 
#'                 when reading multiple blocks from a single file.)
#' @param na.strings A character vector of strings which are to be interpreted
#'                   as \code{NA} values by \code{utils::read.csv},
#'                   \code{readxl::read_xls}, \code{readxl::read_xlsx},
#'                   or \code{utils::read.table}
#' @param extension Allowed for backward compatibility; \code{filetype} is
#'                  now the preferred argument name.
#' @param names_to_col Allowed for backward compatibility; 
#'               \code{run_names_header} is now the preferred argument name.
#' @param ...   Other arguments passed to \code{utils::read.csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx}, or
#'              \code{utils::read.table}
#'              
#' @return A dataframe containing a single widemeasures, or
#'         A list of widemeasures named by filename
#' 
#' @export
read_wides <- function(files, filetype = NULL, 
                       startrow = NULL, endrow = NULL, 
                       startcol = NULL, endcol = NULL,
                       header = TRUE,
                       sheet = NULL, 
                       run_names = NULL,
                       run_names_header = "file",
                       run_names_dot = FALSE, run_names_path = TRUE,
                       run_names_ext = FALSE,
                       metadata = NULL, 
                       na.strings = c("NA", ""),
                       extension, names_to_col,
                       ...) {
  #Logic 2.0: if header TRUE
  #             if startrow provided, header is startrow
  #             if startrow not provided, header is 1
  #           if header FALSE
  #             columns numbered V1...Vn
  
  if(!base::missing(extension)) {
    if(!base::missing(filetype)) {
      warning("Ignoring extension, using filetype")
    } else {filetype <- extension}
  }
  if(!base::missing(names_to_col)) {
    if(!base::missing(run_names_header)) {
      warning("Ignoring names_to_col, using run_names_header")
    } else {run_names_header <- names_to_col}
  }

  nwides <- max(length(files), length(startrow), length(endrow),
                length(startcol), length(endcol), length(sheet),
                na.rm = TRUE)
  
  files <- check_input_dimensions(files, "files", nwides, "the number of wides")
  
  if(!is.null(startrow) & !all(is.numeric(startrow))) {
    startrow[!is.numeric(startrow)] <- from_excel(startrow[!is.numeric(startrow)])
    startrow <- as.numeric(startrow)
  }
  if(is.null(startrow)) {startrow <- NA}
  startrow <- check_input_dimensions(startrow, "startrow", nwides,
                              "the number of wides")
  
  if(!is.null(endrow) & !all(is.numeric(endrow))) {
    endrow[!is.numeric(endrow)] <- from_excel(endrow[!is.numeric(endrow)])
    endrow <- as.numeric(endrow)
  }
  if(is.null(endrow)) {endrow <- NA}
  endrow <- check_input_dimensions(endrow, "endrow", nwides,
                            "the number of wides")

  if(!is.null(startcol) & !all(is.numeric(startcol))) {
    startcol[!is.numeric(startcol)] <- from_excel(startcol[!is.numeric(startcol)])
    startcol <- as.numeric(startcol)
  }
  if(is.null(startcol)) {startcol <- NA}
  startcol <- check_input_dimensions(startcol, "startcol", nwides,
                              "the number of wides")
  
  if(!is.null(endcol) & !all(is.numeric(endcol))) {
    endcol[!is.numeric(endcol)] <- from_excel(endcol[!is.numeric(endcol)])
    endcol <- as.numeric(endcol)
  }
  if(is.null(endcol)) {endcol <- NA}
  endcol <- check_input_dimensions(endcol, "endcol", nwides,
                            "the number of wides")
  
  if(!is.null(sheet)) {
    sheet <- check_input_dimensions(sheet, "sheet", nwides,
                             "the number of wides")
  }
  
  if(!is.null(metadata) & any(names(metadata) == "")) {
    stop("not all metadata have names")
  }
  
  #Determine filetype(s)
  filetype <- infer_check_filetype(
    filetype = filetype, files = files,
    needed_len = nwides, needed_name = "the number of wides")
  
  #Check for names error
  if (!is.null(run_names)) {stopifnot(length(run_names) == nwides)}
  
  #If run_names not provided, infer from filenames
  if (is.null(run_names)) {
    run_names <- parse_filestrings(
      files, keep_dot = run_names_dot, 
      keep_path = run_names_path, keep_ext = run_names_ext)
  }
  
  if (!is.null(metadata)) {
    for (i in 1:length(metadata)) {
      #Check metadata for any list entries, if there are and they're not
      # the right length, pass error. Otherwise, replicate as needed
      if(length(metadata[[i]]) != 2) {
        stop(paste("metadata", names(metadata)[i], "is not a vector or a list of length 2"))
      }
      if(is.list(metadata[[i]])) {
        metadata[[i]][[1]] <- 
          check_input_dimensions(metadata[[i]][[1]], names(metadata)[i],
                          nwides, "the number of blocks")
        metadata[[i]][[2]] <- 
          check_input_dimensions(metadata[[i]][[2]], names(metadata)[i],
                          nwides, "the number of blocks")
      }
    }
  }
  
  #Create empty recipient list
  outputs <- rep(list(NA), nwides)
  
  #Import data
  for (i in 1:nwides) {
    rawfile <- read_gcfile(file = files[i], filetype = filetype[i],
                        na.strings = na.strings, sheet = sheet[i], ...)
    
    #Infer colnames/take subsets as needed
    if(is.na(endrow[i])) {endrow[i] <- nrow(rawfile)}
    if(is.na(endcol[i])) {endcol[i] <- ncol(rawfile)}
    if(is.na(startcol[i])) {startcol[i] <- 1}
    if (is.na(startrow[i])) {startrow[i] <- 1}
    if(startrow[i] < 1 || endrow[i] > nrow(rawfile) ||
       startcol[i] < 1 || endcol[i] > ncol(rawfile)) {
      stop("Startrow, startcol, endrow, or endcol are out of range for the file")}
    if (header == TRUE) { #so colnames taken from file
      outputs[[i]] <- rawfile[(startrow[i]+1):endrow[i], startcol[i]:endcol[i]]
      colnames(outputs[[i]]) <- rawfile[(startrow[i]), startcol[i]:endcol[i]]
    } else { #so colnames should be numbered
      outputs[[i]] <- rawfile[startrow[i]:endrow[i], startcol[i]:endcol[i]]
      colnames(outputs[[i]]) <- paste0("V", 1:ncol(outputs[[i]]))
    }
    
    #Get metadata
    if (!is.null(metadata)) {
      metadata_vector <- rep(NA, times = length(metadata))
      names(metadata_vector) <- names(metadata)
      for (j in 1:length(metadata)) {
        if(!is.list(metadata[[j]])) { #metadata item is a vector
          metadata_vector[j] <- 
            get_metadata(df = rawfile, row = metadata[[j]][1],
                         col = metadata[[j]][2])
        } else {  #metadata item is a list (presumably of two vectors)
          #the first vector is the rows for each ith block
          #the second vector is the columns for each ith block
          metadata_vector[j] <- 
            get_metadata(df = rawfile, row = metadata[[j]][[1]][i],
                         col = metadata[[j]][[2]][i])
        }
      }
    } else {metadata_vector <- NULL}
    
    #Add run_names if requested as column
    if(!is.null(run_names_header)) {
      metadata_vector <- c(run_names[i], metadata_vector)
      names(metadata_vector)[1] <- run_names_header
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
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#'                  "csv", "xls", or "xlsx".
#'                  
#'                  "tbl" or "table" to use \code{read.table} to read the file,
#'                  "csv2" to use \code{read.csv2}, "delim" to 
#'                  use \code{read.delim}, or "delim2" to use \code{read.delim2}.
#'                  
#'                  If none provided, \code{read_tidys} will infer filetype(s) 
#'                  from the extension(s) in \code{files}. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the data are located in \code{files}.
#'                 
#'                 Can be a vector or list the same length as \code{files}, or
#'                 a single value that applies to all \code{files}. Values
#'                 can be numeric or a string that will be automatically
#'                 converted to numeric by \code{from_excel}.
#'                 
#'                 If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#' @param run_names Names to give the tidy files read in. By default uses the
#'                  file names if not specified. These names may be added
#'                  to the resulting data frame depending on the value of
#'                  the \code{names_to_col} argument
#' @param run_names_header Should the run names (provided in \code{run_names}
#'                     or inferred from \code{files}) be added as a column to the
#'                     output? 
#'                     
#'                     If \code{run_names_header} is TRUE, they will be added with
#'                     the column name "run_name"
#'                     
#'                     If \code{run_names_header} is FALSE, they will not be added.
#'                     
#'                     If \code{run_names_header} is a string, they will be added
#'                     and the column name will be the string specified
#'                     for \code{run_names_header}.
#'                     
#'                     If \code{run_names_header} is NULL, they only will be 
#'                     added if there are multiple tidy data.frames being read.
#'                     In which case, the column name will be "run_name"
#' @param run_names_dot If run_names are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param run_names_path If run_names are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param run_names_ext If run_names are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param na.strings A character vector of strings which are to be interpreted
#'                   as \code{NA} values by \code{utils::read.csv},
#'                   \code{readxl::read_xls}, \code{readxl::read_xlsx},
#'                   or \code{utils::read.table}
#' @param extension Allowed for backward compatibility; \code{filetype} is
#'                  now the preferred argument name.
#' @param names_to_col Allowed for backward compatibility; 
#'               \code{run_names_header} is now the preferred argument name.
#' @param ...   Other arguments passed to \code{utils::read.csv},
#'              \code{readxl::read_xls}, \code{readxl::read_xlsx}, or
#'              \code{utils::read.table}
#'              sheet
#'               
#' @details
#' \code{startrow}, \code{endrow}, \code{startcol}, \code{endcol}, 
#' \code{sheet} and \code{filetype} can either be a single value that 
#' applies for all files or vectors or lists the same length as \code{files}
#' 
#' Note that the startrow is always assumed to be a header
#' 
#' @return A dataframe containing a single tidy data.frame, or
#'         A list of tidy-shaped data.frames named by filename
#'         
#' @export
read_tidys <- function(files, filetype = NULL, 
                       startrow = NULL, endrow = NULL, 
                       startcol = NULL, endcol = NULL,
                       sheet = NULL, 
                       run_names = NULL, run_names_header = NULL,
                       run_names_dot = FALSE, run_names_path = TRUE,
                       run_names_ext = FALSE,
                       na.strings = c("NA", ""),
                       extension, names_to_col,
                       ...) {
  
  if(!base::missing(extension)) {
    if(!base::missing(filetype)) {
      warning("Ignoring extension, using filetype")
    } else {filetype <- extension}
  }
  if(!base::missing(names_to_col)) {
    if(!base::missing(run_names_header)) {
      warning("Ignoring names_to_col, using run_names_header")
    } else {run_names_header <- names_to_col}
  }
  
  if (!is.null(startrow) & !is.numeric(startrow)) {
    startrow <- from_excel(startrow)}
  if (!is.null(endrow) & !is.numeric(endrow)) {
    endrow <- from_excel(endrow)}
  if (!is.null(startcol) & !is.numeric(startcol)) {
    startcol <- from_excel(startcol)}
  if (!is.null(endcol) & !is.numeric(endcol)) {
    endcol <- from_excel(endcol)}
  
  if(is.null(startrow)) {startrow <- NA}
  startrow <- check_input_dimensions(startrow, "startrow", length(files))
  
  if (is.null(endrow)) {endrow <- NA}
  endrow <- check_input_dimensions(endrow, "endrow", length(files))
  
  if (is.null(startcol)) {startcol <- NA}
  startcol <- check_input_dimensions(startcol, "startcol", length(files))
  
  if (is.null(endcol)) {endcol <- NA}
  endcol <- check_input_dimensions(endcol, "endcol", length(files))
  
  if (!is.null(sheet)) {
    sheet <- check_input_dimensions(sheet, "sheet", length(files))
  }
  
  #Determine filetype(s)
  filetype <- infer_check_filetype(
    filetype = filetype, files = files, needed_len = length(files),
    needed_name = "the number of tidys")
  
  #Check for names error
  if (!is.null(run_names)) {stopifnot(length(run_names) == length(files))}
  
  #If run_names not provided, infer from filenames
  if (is.null(run_names)) {
    run_names <- parse_filestrings(
      files, keep_dot = run_names_dot, 
      keep_path = run_names_path, keep_ext = run_names_ext)
  }
  
  #Create empty recipient list
  outputs <- rep(list(NA), length(files))
  
  #Import data
  for (i in 1:length(files)) {
    rawfile <- read_gcfile(file = files[i], filetype = filetype[i],
                        na.strings = na.strings, sheet = sheet[i], ...)
    
    #Infer colnames/take subsets as needed
    if(is.na(endrow[i])) {endrow[i] <- nrow(rawfile)}
    if(is.na(endcol[i])) {endcol[i] <- ncol(rawfile)}
    if(is.na(startcol[i])) {startcol[i] <- 1}
    if (is.na(startrow[i])) {startrow[i] <- 1}
    if(startrow[i] < 1 || endrow[i] > nrow(rawfile) ||
       startcol[i] < 1 || endcol[i] > ncol(rawfile)) {
      stop("Startrow, startcol, endrow, or endcol are out of range for the file")}
    
    #Get header
    outputs[[i]] <- rawfile[(startrow[i]+1):endrow[i], startcol[i]:endcol[i]]
    colnames(outputs[[i]]) <- rawfile[(startrow[i]), startcol[i]:endcol[i]]
    
    #Add run name if needed
    if(is.null(run_names_header)) {
      if (length(outputs) > 1) {
        #names should be saved in a column titled run_name
        outputs[[i]] <- cbind(data.frame("run_name" = run_names[i]),
                              outputs[[i]])
      }
    } else {
      if(is.character(run_names_header)) {
        #names should be saved in a column titled the value of run_names_header
        temp_runname_df <- data.frame("run_name" = run_names[i])
        names(temp_runname_df) <- run_names_header
        outputs[[i]] <- cbind(temp_runname_df, outputs[[i]])
      } else {
        if(run_names_header) {
          #run_names_header is TRUE
          outputs[[i]] <- cbind(data.frame("run_name" = run_names[i]),
                                outputs[[i]])
        } else if (!run_names_header) {
          #run_names_header is FALSE, so add nothing
        } else {stop("run_names_header is not one of the valid types")}
      }
    }
  }
  
  if (length(outputs) == 1) {
    return(outputs[[1]])
  } else {
    return(outputs)
  }
}


# Import functions (for block-shaped files) ----

#' Import blockmeasures
#' 
#' Function to import blockmeasures from files and return widemeasures
#' This function acts as a wrapper to call \code{read_blocks}, 
#' \code{uninterleave}, then \code{trans_block_to_wide} in one go
#' 
#' @param files Vector of filenames (as strings), each of which is a 
#'              block-shaped file containing measures data. File formats
#'              can be .csv, .xls, or .xlsx
#' @param num_plates Number of plates. If multiple plates uninterleave will be
#'                   used to separate blockmeasures into those plates accordingly
#' @param plate_names (optional) Names to put onto the plates when output
#' @param wellnames_sep String to use as separator for well names between 
#'                      rowname and column name
#' @param ... Other arguments to pass to \code{read_blocks}, \code{uninterleave},
#'            or \code{widen_blocks}
#' @details     Common arguments that you may want to provide via \code{...}
#'              include:
#' 
#'              \code{startrow}, \code{endrow}, \code{startcol}, \code{endcol}, 
#'              \code{sheet} - specifying the location of design information 
#'              inside \code{files} to \code{read_blocks}
#'              
#'              \code{metadata} - specifying metadata to \code{read_blocks}
#'              
#'              See help for \code{read_blocks} for more details
#'              
#'              If you find yourself needing more control, you can run the 
#'              steps manually, first reading with \code{read_blocks}, 
#'              separating plates as needed with \code{uninterleave}, 
#'              then transforming to wide with \code{trans_block_to_wide}.
#'              
#' @return If \code{num_plates = 1}, a wide-shaped \code{data.frame}
#'         containing the measures data.
#'         
#'         if \code{num_plates} is greater than one, a list of 
#'         \code{data.frame}'s, where each \code{data.frame} is wide-shaped.
#'              
#' @export       
import_blockmeasures <- function(files, num_plates = 1, 
                                 plate_names = NULL,
                                 wellnames_sep = "",
                                 ...) {
  blockmeasures <- uninterleave(read_blocks(files = files, ...),
                                n = num_plates)
  widemeasures <- rep(list(NA), num_plates)
  for (i in 1:length(blockmeasures)) {
    widemeasures[[i]] <- 
      trans_block_to_wide(blockmeasures[[i]],
                          wellnames_sep = wellnames_sep,nested_metadata = TRUE)
  }
  if (is.null(plate_names)) { #no plate_names provided
    names(widemeasures) <- paste0("plate_", 1:length(widemeasures))
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
#' This function acts as a wrapper to call \code{read_blocks}, 
#' \code{paste_blocks}, \code{trans_block_to_wide}, \code{trans_wide_to_tidy}, 
#' and \code{separate_tidys} in one go
#'
#' @param files Vector of filenames (as strings), each of which is a 
#'              block-shaped designs file. Inputs can be .csv, .xls, or .xlsx
#' @param block_names
#'              Vector of names for design elements. These will be the
#'              resulting column names in the output data frame. Should be 
#'              in the same order as \code{files} and/or same order
#'              as corresponding \code{files} themselves. If \code{NULL},
#'              file names will be used as column names.
#' @param block_name_header The column name for the column containing the
#'                          \code{block_names}
#' @param sep   If block design files are already pasted,
#'              sep specifies the string separating design elements
#'              
#'              If NULL, \code{import_blockdesigns} will assume no elements
#'              are already pasted together and attempt to find a character
#'              not used in the imported files to paste and later separate
#'              design elements.
#' @param ...   Other arguments to pass to \code{read_blocks}, 
#'              \code{paste_blocks}, \code{trans_block_to_wide},
#'              \code{trans_wide_to_tidy}, or \code{separate_tidy}.
#'              
#'              See Details for more information
#'              
#' @details     Common arguments that you may want to provide via \code{...}
#'              include:
#' 
#'              \code{startrow}, \code{endrow}, \code{startcol}, \code{endcol}, 
#'              \code{sheet} - specifying the location of design information 
#'              inside \code{files} to \code{read_blocks}
#'              
#'              \code{wellnames_sep} - specifying what character (or "" for none)
#'              should be used when pasting together the rownames and
#'              column names. Note that this should be chosen to match
#'              the well names in your measures.
#'              
#'              Note that \code{import_blockdesigns} cannot currently handle
#'              metadata specified via the \code{metadata} argument of
#'              \code{read_blocks}
#'              
#'              If you find yourself needing more control, you can run the 
#'              steps manually, first reading with \code{read_blocks},
#'              pasting as needed with \code{paste_blocks}, 
#'              transforming to tidy with \code{trans_block_to_wide} and
#'              \code{trans_wide_to_tidy}, and separating as needed with
#'              \code{separate_tidys}.
#'              
#' @return A tidy-shaped \code{data.frame} containing the design information
#'         from \code{files}
#' 
#' @export
import_blockdesigns <- function(files, block_names = NULL, 
                                block_name_header = "block_name", 
                                sep = NULL, ...) {
  blocks <- parse_dots(read_blocks, 
                        block_names = block_names, files = files, 
                        block_name_header = block_name_header, ...)
  
  if(length(files) > 1) {
    if(is.null(sep)) {
      #No sep provided, so look for a character that is not present
      # in the data/metadata and so can be used as a separator
      sep <- c("_", " ", "-", ",", ";")
      
      not_in_blocks <-
        sapply(
          X = sep,
          FUN = function(y) {
            !any(grepl(pattern = y, fixed = TRUE,
                       x = unlist(
                         lapply(X = blocks,
                                FUN = function(x) {
                                  c(unlist(x[1]), unlist(x[2]))
                                }))))
          })
      
      if(!any(not_in_blocks)) {
        stop("all of '_', ' ', '-', ',', and ';' are found in the files,
              specify a sep not found in the files")
      } else {sep <- sep[which(not_in_blocks)[1]]}
    }

    blocks_pasted <- parse_dots(paste_blocks, blocks = blocks,
                                 sep = sep, nested_metadata = TRUE, ...)
  } else {blocks_pasted <- blocks}
  
  wides <- parse_dots(trans_block_to_wide, blocks = blocks_pasted,
                       nested_metadata = TRUE, ...)
  
  #Transform to tidy, dropping the block_name column and using it
  # as the column name for the values column
  vals_colname <- wides[1, block_name_header]
    
  tidys <- parse_dots(
    trans_wide_to_tidy, 
    wides = wides[, -which(block_name_header == colnames(wides))], 
    data_cols = colnames(wides)[colnames(wides) != block_name_header], 
    values_to = vals_colname, values_to_numeric = FALSE,
    ...)
  
  if(length(files) > 1) {
    tidy_sep <- parse_dots(separate_tidy, 
                            data = tidys, sep = sep, col = vals_colname)
  } else {tidy_sep <- tidys}
    
  return(tidy_sep)
}


# Design functions ----

#' Make design data.frame(s)
#' 
#' This is a function to easily input experimental design elements
#' for later merging with read data
#' 
#' @details 
#' Note that either \code{nrows} or \code{block_row_names} must be provided
#' and that either \code{ncols} or \code{block_col_names} must be provided
#' 
#' @param nrows,ncols Number of rows and columns in the plate data
#' @param block_row_names,block_col_names Names of the rows, columns
#'                                     of the plate blockmeasures data
#' @param block_name_header The name of the field containing the
#'                          \code{block_names}
#' @param output_format One of c("blocks", "blocks_pasted", "wide", "tidy")
#'                      denoting the format of the resulting data.frame
#'                      
#'                      For easy merging with tidymeasures, leave as default
#'                      of 'tidy'. 
#'                      
#'                      For human-readability to confirm design
#'                      is correct, choose 'blocks' or 'blocks_pasted'. 
#'                      
#'                      For writing to block-shaped file(s), choose 'blocks' or
#'                      'blocks_pasted'.
#' @param wellnames_numeric If \code{block_row_names} or \code{block_col_names}
#'                        are not specified, then names will be generated
#'                        automatically according to \code{wellnames_numeric}.
#'                        
#'                        If \code{wellnames_numeric} is TRUE, rows and columns
#'                        will be numbered with "R" and "C" prefixes, respectively.
#'                        
#'                        If \code{wellnames_numeric} is FALSE, rows will be
#'                        lettered A through Z, while columns will be numbered
#' @param wellnames_sep A string used when concatenating rownames and column
#'                      names to create well names, when 
#'                      \code{output_format = "wide"} or 
#'                      \code{output_format = "tidy"}
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names, when \code{output_format = "tidy"}
#' @param colnames_first  When wellnames are created for 
#'                        \code{output_format = "wide"} or 
#'                        \code{output_format = "tidy"} by \code{paste}-ing the
#'                        rownames and column names, should the column names
#'                        come first. 
#' @param lookup_tbl_start Value in the lookup table for the split pattern values
#'                         that corresponds to the first value in the vector.
#'                         
#'                         Lookup table by default is 
#'                         c(1,2,...,8,9,A,B,...Y,Z,a,b,...,y,z). If,
#'                         for example, lookup_tbl_start = "A", then the lookup
#'                         table will now be c(A,B,...Y,Z,a,b,...,y,z)
#' @param pattern_split character to split pattern elements provided in
#'                      \code{...} by, if they're not already a vector
#' @param ... Each \code{...} argument must be named, and must be a list with 
#'            five elements:
#' 
#'              1. a vector of the values
#'              
#'              2. a vector of the rows the pattern should be applied to
#'              
#'              3. a vector of the columns the pattern should be applied to
#'              
#'              4. a string or vector denoting the pattern in which the
#'                 values should be filled into the rows and columns specified.
#'                 
#'                 If it's a string, will be split by \code{pattern_split}.
#'                 Pattern will be used as the indices of the values vector.
#'               
#'                 0's refer to NA. The pattern will be recycled as necessary
#'                 to fill all the wells of the rows and columns specified.
#'               
#'              5. a logical for whether this pattern should be filled byrow
#'
#' @return Depends on \code{output_format}:
#' 
#'         If \code{output_format = "blocks"}, a list of \code{data.frame}'s
#'         where each \code{data.frame} is block-shaped containing the
#'         information for a single design element
#'         
#'         If \code{output_format = "blocks_pasted"}, a single 
#'         \code{data.frame} containing the \code{paste}-ed information
#'         for all design elements
#'         
#'         If \code{output_format = "wide"}, a wide-shaped \code{data.frame}
#'         containing all the design elements
#'         
#'         If \code{output_format = "tidy"}, a tidy-shaped \code{data.frame}
#'         containing all the design elements
#'         
#' @examples
#' make_design(nrows = 8, ncols = 12,
#'             design_element_name = list(c("A", "B", "C"),
#'                                        2:7,
#'                                        2:11,
#'                                        "112301", 
#'                                        TRUE))
#'                           
#' ## To be reminded what arguments are needed, use make_designpattern:
#' make_design(nrows = 8, ncols = 12,
#'             design_element_name = make_designpattern(
#'                  values = c("A", "B", "C"),
#'                  rows = 2:7, 
#'                  cols = 2:11,
#'                  pattern = "112301",
#'                  byrow = TRUE))              
#' 
#' @export         
make_design <- function(nrows = NULL, ncols = NULL,
                        block_row_names = NULL, block_col_names = NULL,
                        block_name_header = "block_name",
                        output_format = "tidy",
                        wellnames_numeric = FALSE, wellnames_sep = "", 
                        wellnames_colname = "Well", colnames_first = FALSE,
                        lookup_tbl_start = 1, pattern_split = "", ...) {
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
    if (wellnames_numeric) {block_row_names <- paste0("R", 1:nrows)
    } else {block_row_names <- to_excel(1:nrows)}
  }
  if (is.null(block_col_names)) {
    if (wellnames_numeric) {block_col_names <- paste0("C", 1:ncols)
    } else {block_col_names <- 1:ncols}
  }
  if (is.null(nrows)) {nrows <- length(block_row_names)}
  if (is.null(ncols)) {ncols <- length(block_col_names)}
  
  if(length(output_format) > 1) {stop("output_format must be a single string")}
  if(!output_format %in% c("blocks", "blocks_pasted", "wide", "tidy")) {
    stop("output_format must be one of c('blocks', 'blocks_pasted', 'wide', 'tidy')")
  }
  
  dot_args <- list(...)
  if(any(is.null(names(dot_args)))) {stop("Each ... arguments must have a name")}
  
  #Make empty output list
  output <- rep(list(list(
    "data" = matrix(NA, nrow = nrows, ncol = ncols,
                    dimnames = list(block_row_names, block_col_names)),
                    "metadata" = stats::setNames("NA", block_name_header))),
    length(unique(names(dot_args))))
  
  #Note dot_args structure
  #dot_args[[i]] = list(values = c("A", "B", "C"),
  #                     rows = rowstart:rowend, cols = colstart:colend
  #                     pattern = "111222333000", byrow = TRUE)
  #                     
  #                     (or pattern can be an already-split vector)
  
  #Loop through input arguments & fill into output dataframe
  for (i in 1:length(dot_args)) {
    if(any(dot_args[[i]][[2]] > nrows) || any(dot_args[[i]][[3]] > ncols)) {
      stop(paste(names(dot_args)[i], "has rows or columns out of range"))}
    
    if(!is.vector(dot_args[[i]][[4]]) & !is.character(dot_args[[i]][[4]])) {
      stop("pattern is not a string nor a vector")
    }
    if (length(dot_args[[i]][[4]]) > 1 || is.numeric(dot_args[[i]][[4]])) {
      pattern_list <- dot_args[[i]][[4]]
    } else if(length(dot_args[[i]][[4]]) == 1) {
      pattern_list <- strsplit(dot_args[[i]][[4]], split = pattern_split)[[1]]
    }
    if (any(nchar(pattern_list) > 1)) {
      if (!canbe.numeric(pattern_list)) {
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
      if(min(pattern_list, na.rm = TRUE) > 9) {
        warning("Your pattern doesn't use any of your first 9 values,
do you need to set `lookup_tbl_start` differently?")
      }
    }
    
    if (((length(dot_args[[i]][[2]])*length(dot_args[[i]][[3]])) %% 
         length(pattern_list)) != 0) {
      warning(paste("Total number of wells is not a multiple of pattern length for",
                    names(dot_args)[i], "\n"))
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
    
    #Fill values into blocks
    output_idx <- match(names(dot_args)[i], unique(names(dot_args)))
    output[[output_idx]]$data[dot_args[[i]][[2]], dot_args[[i]][[3]]] <-
      matrix(vals_list,
             nrow = length(dot_args[[i]][[2]]),
             ncol = length(dot_args[[i]][[3]]),
             byrow = dot_args[[i]][[5]])
    output[[output_idx]]$metadata[block_name_header] <- names(dot_args)[i]
  }
  
  if(output_format %in% c("blocks_pasted", "wide", "tidy")) {
    if(length(dot_args) > 1) {
      sep <- sep_finder(output, nested_metadata = TRUE)[1]
      output <- paste_blocks(output, nested_metadata = TRUE, sep = sep)
    }
    if(output_format %in% c("wide", "tidy")) {
      output <- trans_block_to_wide(output, wellnames_sep = wellnames_sep,
                                    nested_metadata = TRUE,
                                    colnames_first = colnames_first)
      if (output_format == "tidy") {
        vals_colname <- output$block_name[1]
        
        output <- 
          trans_wide_to_tidy(
            output[, -which(block_name_header == colnames(output))], 
            data_cols = colnames(output)[colnames(output) != block_name_header],
            values_to = vals_colname,
            values_to_numeric = FALSE)
        if(length(dot_args) > 1) {
          output <- separate_tidy(data = output, sep = sep, col = vals_colname)
        }
      }
    }
  }
  
  return(output)
}


#' Make design pattern
#' 
#' A helper function for use with \code{make_design}
#' 
#' @param values Vector of values to use
#' @param rows Vector of rows where pattern applies
#' @param cols Vector of cols where pattern applies
#' @param pattern Numeric pattern itself, where numbers refer to entries
#'                in \code{values}
#' @param byrow logical for whether pattern should be created by row
#' 
#' @return \code{list(values, rows, cols, pattern, byrow)}
#' 
#' @examples
#' make_design(nrows = 8, ncols = 12,
#'             design_element_name = make_designpattern(
#'                  values = c("A", "B", "C"),
#'                  rows = 2:7, 
#'                  cols = 2:11,
#'                  pattern = "112301",
#'                  byrow = TRUE))
#' 
#' @seealso [gcplyr::make_design()]
#' 
#' @export
make_designpattern <- function(values, rows, cols, 
                               pattern = 1:length(values), byrow = TRUE) {
  stopifnot(is.vector(values), is.vector(rows), is.vector(cols),
            (is.character(pattern) | is.vector(pattern)), is.logical(byrow))
  return(list(values, rows, cols, pattern, byrow))
}

#' @rdname make_designpattern
#' @export
mdp <- make_designpattern

#' Fill output data.frame with \code{data} and \code{metadata}
#'
#' This is an internal function used by \code{write_blocks} that does the 
#' fill data and metadata step
#' 
#' @param output Data frame for data and metadata to be filled into
#' @param input One data and metadata to fill into \code{output}
#' @param rs index of the row in \code{output} where data and metadata should
#'           start being filled in
#' @param metadata_include Indices of which metadata items should be included
#'                         
#'                         Use NULL for no metadata inclusion
#'                         
#' @return A list, where [[1]] is \code{output} containing the data and
#'         metadata from \code{input} but properly formatted for writing.
#'         
#'         And [[2]] is the updated \code{rs} to be used for subsequent
#'         calls to \code{fill_data_metadata}
#' 
#' @noRd
fill_data_metadata <- function(output, input, rs, 
                               metadata_include = 1:length(input$metadata)) {
  data <- input$data
  if(!is.null(metadata_include)) {
    metadata <- input$metadata[metadata_include]
    
    #Save metadata names
    output[rs:(rs+length(metadata)-1), 1] <- names(metadata)
    #Save metadata values
    output[rs:(rs+length(metadata)-1), 2] <- metadata
    
    #Update row counter
    rs <- rs+length(metadata)
  }
  #Save colnames
  output[rs, 2:ncol(output)] <- colnames(data)
  rs <- rs+1
  
  #Save rownames
  output[rs:(rs+nrow(data)-1), 1] <- row.names(data)
  
  #Save data
  output[rs:(rs+nrow(data)-1), 2:ncol(output)] <- data
  
  rs <- rs+nrow(data)+1 #add spacer empty row
  
  return(list(output, rs))
}

#' Write block designs to csv
#' 
#' This function writes block-shaped lists (as created by
#' \code{read_blocks} or \code{make_design}) to csv files, including
#' both \code{data} and \code{metadata} in a variety of output formats
#' 
#' 
#' @param blocks list of block-shaped data to be written to file
#' @param file \code{NULL}, a character string naming a file to write to, or a 
#'             vector of character strings naming files to write to. 
#' 
#'             A file name is required when \code{output_format = "single"}
#'             
#'             A file name can be specified when \code{output_format = "pasted"},
#'             or \code{file} can be set to \code{NULL} as long as 
#'             \code{block_name_location = "filename"} (where pasted 
#'             \code{block_name} metadata will be used for the file name)
#'             
#'             File names can be specified when \code{output_format = "multiple"},
#'             or \code{file} can be set to \code{NULL} as long as 
#'             \code{block_name_location = "filename"} (where the 
#'             \code{block_name} metadata will be used for the file names)
#'             
#' @param output_format One of "single", "pasted", "multiple".
#' 
#'                      "single" will write all blocks into a single
#'                      csv file, with an empty row between successive
#'                      blocks.
#'                      
#'                      "pasted" will paste all blocks together using a
#'                      \code{paste_sep}, and then write that now-pasted
#'                      block to a single csv file.
#'                      
#'                      "multiple" will write each block to its own csv file.
#' @param block_name_location Either \code{NULL}, 'filename' or 'file'.
#' 
#'                           If \code{NULL}, \code{block_name_location} will 
#'                           be automatically selected based on 
#'                           \code{output_format}. For 
#'                           \code{output_format = 'single'} and 
#'                           \code{output_format = 'pasted'}, 
#'                           \code{block_name_location} defaults to 'file'.
#'                           For \code{output_format = 'multiple'}, 
#'                           \code{block_name_location} defaults to 'filename'
#' 
#'                           If 'filename', the \code{block_name} metadata 
#'                           will be used as the output file name(s) when
#'                           no file name(s) are provided, or appended to
#'                           file name(s) when they have been provided.
#'                           
#'                           If 'file', the \code{block_name} metadata will be
#'                           included as a row in the output file.
#' @param block_name_header The name of the field containing the
#'                          \code{block_names}
#' @param paste_sep When \code{output_format = 'pasted'}, what character
#'                  will be used to paste together blocks.   
#' @param filename_sep What character will be used to paste together 
#'                     filenames when block_name_location = 'filename'.   
#' @param na The string to use for missing values in the data.
#' @param dir The directory that file(s) will be written into. When 
#'            \code{dir = NULL}, writes to the current working directory.
#'            (Can only be used when \code{file = NULL})
#' @param ... Other arguments passed to \code{write.table}
#' @return Nothing, but R objects are written to files
#' 
#' @export
write_blocks <- function(blocks, file, 
                         output_format = "multiple",
                         block_name_location = NULL,
                         block_name_header = "block_name",
                         paste_sep = "_", filename_sep = "_", 
                         na = "", dir = NULL, ...) {
  #Checks on dir and file
  if(is.null(dir)) {
    dir <- ""
  } else { #dir is not NULL
    if(!is.null(file)) {
      stop("dir must be NULL when file is specified")
    } else {
      #Make sure dir ends in /
      if(substr(dir, nchar(dir), nchar(dir)) != "/") {dir <- paste0(dir, "/")}
    }
  }
  
  if(!all(sapply(X = blocks, FUN = length) == 2) |
     !all(unlist(lapply(X = blocks, FUN = names)) %in% c("data", "metadata"))) {
    stop("blocks is incorrectly formatted")
  }
  
  if(!output_format %in% c("single", "pasted", "multiple")) {
    stop("output_format must be one of c('single', 'pasted', 'multiple')")
  }
  
  if(is.null(block_name_location)) {
    if(output_format %in% c("single", "pasted")) {
      block_name_location <- "file"
    } else if (output_format == "multiple") {
      block_name_location <- "filename"
    }
  } else if(!block_name_location %in% c('filename', 'file')) {
    stop("block_name_location must be one of c('filename', 'file')")
  }
  
  if(output_format == "single") {
    #basically going to put all the blocks in one file one on top
    # of another with filename = file argument
    
    if(is.null(file)) {stop("file is required when output_format = 'single'")}
    if(substr(file, nchar(file)-3, nchar(file)) != ".csv") {
      warning("appending '.csv' to filename\n")
      file <- paste0(file, ".csv")
    }
    if(block_name_location == "filename") {
      warning("block_name_location can only be 'file' when output_format = 'single'
              saving with block_name_location = 'file'\n")
    }
    
    #Calc needed # cols: 2 for metadata, 1 for rownames, ncol for data
    numcols <- max(2, 1+sapply(X = blocks, FUN = function(x) {ncol(x[[1]])}))
    #Calc needed # rows: 1 ea metadata, 1 ea row of data,
    #                          1 col header, 1 space after ea block
    numrows <- 2+sum(sapply(blocks, 
                            FUN = function(x) {nrow(x[[1]]) + length(x[[2]])}))
    
    output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
    
    rs <- 1 #row start index
    for (i in 1:length(blocks)) {
      #Use sub-function to fill all blocks into output df
      fill_data_metadata_tempoutput <- 
        fill_data_metadata(output = output, input = blocks[[i]], rs = rs)
      output <- fill_data_metadata_tempoutput[[1]]
      rs <- fill_data_metadata_tempoutput[[2]]
    }
    #Write file
    utils::write.table(output, file = paste0(dir, file), sep = ",", na = na,
                       col.names = FALSE, row.names = FALSE, ...)
  } else if (output_format == "pasted") {
    #going to paste all the blocks together then save a single file
    blocks <- paste_blocks(blocks, sep = paste_sep)
    
    if(block_name_location == "filename") {
      #Put pasted block names in filename
      if(is.null(file)) {
        file <- paste0(blocks[[1]]$metadata[block_name_header], ".csv")
      } else {
        if(substr(file, nchar(file)-3, nchar(file)) != ".csv") {
          file <- 
            paste0(file, filename_sep, blocks[[1]]$metadata[block_name_header], 
                  ".csv")
        } else {
          file <- 
            sub("\\.csv", x = file,
                paste0(filename_sep, blocks[[1]]$metadata[block_name_header], 
                      ".csv"))
        }
      }
      
      #Calc needed # rows: 1 ea metadata except block_name (-1), 
      #                          1 ea row of data, 1 col header (+1)
      numrows <- sum(nrow(blocks[[1]]$data) + length(blocks[[1]]$metadata))
      
      if(length(blocks[[1]]$metadata) > 1) {
        #there are other metadata, put them in rows above with WARNING
        warning("block_name_location = 'filename' but there are multiple
                 metadata entries. Putting block_names in filename
                 and writing remaining metadata into file\n")
        
        #Calc needed # cols: 2 for metadata, 1 for rownames, ncol for data
        numcols <- max(2, 1+ncol(blocks[[1]]$data))
        
        #Set up for saving
        output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))

        #Use sub-function to fill block & non-blocknames metadata into output df
        output <- 
          fill_data_metadata(
            output = output, input = blocks[[1]], rs = 1,
            metadata_include = 
              which(names(blocks[[1]]$metadata) != block_name_header))[[1]]
        
      } else {#metadata contains only block_names
        
        #Calc needed # cols: 1 for rownames, ncol for data
        numcols <- 1+ncol(blocks[[1]]$data)
        
        #Set up for saving
        output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
        
        #Use sub-function to fill block & non-blocknames metadata into output df
        output <- 
          fill_data_metadata(output = output, input = blocks[[1]], 
                             rs = 1, metadata_include = NULL)[[1]]
      }
    } else { #block_name_location == 'file'
      #put pasted metadata in rows above block
      if(is.null(file)) {
        stop("file is required when output_format = 'pasted' and block_name_location = 'file'")
      }
      if(substr(file, nchar(file)-3, nchar(file)) != ".csv") {
        warning("appending '.csv' to filename\n")
        file <- paste0(file, ".csv")
      }
      
      #Calc needed # cols: 2 for metadata, 1 for rownames, ncol for data
      numcols <- max(2, 1+ncol(blocks[[1]]$data))
      #Calc needed # rows: 1 ea metadata, 1 ea row of data, 1 col header
      numrows <- 1+sum(nrow(blocks[[1]]$data) + length(blocks[[1]]$metadata))
      
      output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
      
      #Use sub-function to fill block & non-blocknames metadata into output df
      output <- 
        fill_data_metadata(output = output, input = blocks[[1]], rs = 1)[[1]]
    }
    #Write file
    utils::write.table(output, file = paste0(dir, file), sep = ",", na = na,
                       col.names = FALSE, row.names = FALSE, ...)
  } else if (output_format == "multiple") {
    #going to save each block as its own file
    if(block_name_location == "filename") {
      if(!is.null(file)) {
        #Add .csv suffix as needed
        for (i in 1:length(file)) {
          if(substr(file[i], nchar(file[i])-3, nchar(file[i])) != ".csv") {
            warning("appending '.csv' to filename\n")
            file[i] <- paste0(file[i], ".csv")
          }
        }
        #Replicate file vector as needed
        if (length(file) < length(blocks)) {
          file <- rep_len(file, length.out = length(blocks))
        }
      }
      
      for (i in 1:length(blocks)) {
        #Put block names in filename
        if(is.null(file)) {
          filenm <- paste0(blocks[[i]]$metadata[block_name_header], ".csv")
        } else {
          filenm <- sub("\\.csv", x = file[i],
                        paste0(filename_sep, blocks[[i]]$metadata[block_name_header], 
                              ".csv"))
        }
        
        #Calc needed # rows: 1 ea metadata except block_name (-1), 
        #                    1 ea row of data, 1 col header (+1)
        numrows <- sum(nrow(blocks[[i]]$data) + length(blocks[[i]]$metadata))
        
        if(length(blocks[[i]]$metadata) > 1) {
          #there are other metadata, put them in rows above with WARNING
          message("block_name_location = 'filename' but there are multiple metadata entries.
Putting block_names in filename and writing remaining metadata into file\n")
          
          #Calc needed # cols: 2 for metadata, 1 for rownames, ncol for data
          numcols <- max(2, 1+ncol(blocks[[i]]$data))
          
          #Set up for saving
          output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
          
          #Use sub-function to fill block & non-blocknames metadata into output df
          output <- 
            fill_data_metadata(
              output = output, input = blocks[[i]], rs = 1,
              metadata_include = 
                which(names(blocks[[i]]$metadata) != block_name_header))[[1]]
        } else { #metadata contains only block_names
          
          #Calc needed # cols: 1 for rownames, ncol for data
          numcols <- 1+ncol(blocks[[i]]$data)
          
          #Set up for saving
          output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
          
          #Use sub-function to fill block & non-blocknames metadata into output df
          output <- 
            fill_data_metadata(output = output, input = blocks[[i]], 
                               rs = 1, metadata_include = NULL)[[1]]
        }
        #Write file
        utils::write.table(output, file = paste0(dir, filenm), sep = ",", na = na,
                           col.names = FALSE, row.names = FALSE, ...)
      }
    } else { #block_name_location == 'file'
      if(is.null(file)) {
        stop("file is required when output_format = 'multiple' and block_name_location = 'file'")
      } else {
        #Add .csv suffix as needed
        for (i in 1:length(file)) {
          if(substr(file[i], nchar(file[i])-3, nchar(file[i])) != ".csv") {
            warning("appending '.csv' to filename\n")
            file[i] <- paste0(file[i], ".csv")
          }
        }
        #Replicate file vector as needed
        if (length(file) < length(blocks)) {
          file <- rep_len(file, length.out = length(blocks))
          warning("appending numeric suffix to files in order of 'blocks'\n")
          for (i in 1:length(file)) {
            file[i] <- gsub("\\.csv", paste0(filename_sep, i, ".csv"), 
                            file[i])
          }
        }
      }
      for (i in 1:length(blocks)) {
        #put metadata in rows at top of each file
        
        #Calc needed # cols: 2 for metadata, 1 for rownames, ncol for data
        numcols <- max(2, 1+ncol(blocks[[1]]$data))
        #Calc needed # rows: 1 ea metadata, 1 ea row of data, 1 col header
        numrows <- 1+sum(nrow(blocks[[1]]$data) + length(blocks[[1]]$metadata))
        
        output <- as.data.frame(matrix(nrow = numrows, ncol = numcols))
        
        #Use sub-function to fill block & non-blocknames metadata into output df
        output <- 
          fill_data_metadata(output = output, input = blocks[[i]], rs = 1)[[1]]
        
        #Write file
        utils::write.table(output, file = paste0(dir, file[i]), sep = ",", na = "",
                           col.names = FALSE, row.names = FALSE, ...)
      }
    }
  }
}


# Manipulation functions ----

#' Transform blocks to wides
#' 
#' Takes blocks and returns them in a wide format
#' 
#' @param blocks Blocks, either a single data.frame or a list of
#'                      data.frames
#' @param wellnames_sep String to use as separator for well names between 
#'                      rowname and column name (ordered according to
#'                      \code{colnames_first}
#' @param nested_metadata A logical indicating the existence of nested metadata
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
trans_block_to_wide <- function(blocks, wellnames_sep = "", 
                         nested_metadata = NULL, colnames_first = FALSE) {
  if(!inherits(blocks, "list")) {
    blocks <- list(blocks)
  }
  
  #Infer nestedness if nested_metadata is set to NULL
  if (is.null(nested_metadata)) {
    nested_metadata <- infer_block_metadata(blocks)
  }
  
  #Check that all blocks have same dimensions
  if (length(blocks) > 1) {
    if (nested_metadata) { #there is nested metadata
      if (!all_same(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x[[1]])[1]}))) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (!all_same(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x[[1]])[2]}))) {
        stop("Not all blocks have the same number of columns of data")
      }
    } else { #there is not nested metadata
      if (!all_same(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x)[1]}))) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (!all_same(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x)[2]}))) {
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
#' @noRd
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
#' @param values_to_numeric logical indicating whether values will be coerced
#'                          to numeric. See below for when this may be
#'                          overridden by arguments passed in \code{...}
#' @param ... Other functions to be passed to \code{tidyr::pivot_longer}
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
  id_cols <- check_input_dimensions(id_cols, "id_cols", length(wides))
  
  if (!is.list(data_cols)) {
    data_cols <- list(data_cols)
  }
  data_cols <- check_input_dimensions(data_cols, "data_cols", length(wides))
  
  #Check cols inputs
  if (any(!is.na(data_cols) & !is.na(id_cols))) {
    warning("Cannot provide both data_cols and id_cols for a given wides, using data_cols only\n")
  }
  
  names_to <- check_input_dimensions(names_to, "names_to", length(wides))
  values_to <- check_input_dimensions(values_to, "values_to", length(wides))
  
  #Create values_transform list as appropriate
  if("values_transform" %in% names(list(...))) {
    if(values_to_numeric) {
      warning("values_to_numeric is TRUE but values_transform is supplied in ..., 
              values_transform will override values_to_numeric\n")}
  } else {
    if(values_to_numeric) {
      values_transform = rep(list(list(templistname = as.numeric)), length(values_to))
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
        tidyr::pivot_longer(data = wides[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = data_cols[[i]],
                            values_transform = values_transform[[i]],
                            ...))
    } else if (!is.na(id_cols[i])) { #user specified which columns are id columns
      outputs[[i]] <- as.data.frame(
        tidyr::pivot_longer(data = wides[[i]],
                            names_to = names_to[i],
                            values_to = values_to[i],
                            cols = which(!colnames(wides[[i]]) %in% id_cols[[i]]),
                            values_transform = values_transform[[i]],
                            ...))
    } else { #User must be providing their own arguments to pivot_longer
      outputs[[i]] <- as.data.frame(tidyr::pivot_longer(data = wides[[i]],
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
#' @noRd
trans_tidy_to_wide <- function() {
  
}

#' Collapse a list of dataframes, or merge two dataframes together
#' 
#' This function is essentially a wrapper for a \code{dplyr} mutate join
#' (by default, a \link[dplyr]{full_join}). The most typical use of this 
#' function is to merge designs with measures data, or to use the collapse 
#' functionality to merge a list of dataframes into a single dataframe.
#' Merging is done by column names that match between \code{x} and \code{y}.
#'  
#' @param x First data.frame, or list of data frames, to be joined
#' @param y Second data.frame, or list of data frames, to be joined
#' @param by A character vector of variables to join by, passed directly
#'           to the join function
#' @param drop Should only \code{complete_cases} of the resulting
#'             data.frame be returned?
#' @param collapse A logical indicating whether x or y is a list containing
#'                 data frames that should be merged together before
#'                 being merged with the other
#' @param names_to Column name for where \code{names(x)} or \code{names(y)} 
#'                 will be entered in if \code{collapse = TRUE}.
#'                 
#'                 If a value of \code{NA} then \code{names(x)} or 
#'                 \code{names(y)} will not be put into a column in the
#'                 returned data.frame
#' @param join     Type of join used to merge \code{x} and \code{y}. Options
#'                 are 'full' (default), 'inner', 'left', and 'right'.
#'                 
#'                 \itemize{
#'                  \item A \code{full} join keeps all observations in \code{x} and 
#'                   \code{y}
#'                  \item A \code{left} join keeps all observations in \code{x}
#'                  \item A \code{right} join keeps all observations in \code{y}
#'                  \item An \code{inner} join only keeps observations found in
#'                   both \code{x} and \code{y} (inner joins are not appropriate
#'                   in most cases because observations are frequently dropped).
#'                 }
#'                 
#'                 See \link[dplyr]{full_join}, \link[dplyr]{left_join}, 
#'                 \link[dplyr]{right_join}, or \link[dplyr]{inner_join} for 
#'                 more details
#' @param ... Other arguments to pass to the underlying join function. See 
#'            \link[dplyr]{full_join}, \link[dplyr]{left_join}, 
#'            \link[dplyr]{right_join}, or \link[dplyr]{inner_join} for options.
#' 
#' @return Data.frame containing merged output of \code{x} and
#'         \code{y}
#' 
#' @export
merge_dfs <- function(x, y = NULL, by = NULL, drop = FALSE,
                      collapse = FALSE, names_to = NA,
                      join = "full", ...) {
  if(!collapse & (inherits(x, "list") | inherits(y, "list"))) {
    stop("if x or y are a list, collapse must be TRUE")}
  if(!join %in% c("full", "right", "left", "inner")) {
    stop("join must be one of: c('full', 'right', 'left', 'inner')")}
  
  if(collapse) {
    #First define the worker func that collapses the df's
    collapse_list <- function(listdfs, names_to) {
      collapse_list_output <- NULL
      for (i in 1:length(listdfs)) {
        if(!is.null(names_to) & !is.na(names_to)) {
          #Put name of ea list element (ea df) into column
          listdfs[[i]] <- cbind(listdfs[[i]], names(listdfs)[i])
          colnames(listdfs[[i]])[ncol(listdfs[[i]])] <- names_to
        }
        #Collapse dfs together
        if (is.null(collapse_list_output)) {collapse_list_output <- listdfs[[i]]
        } else {collapse_list_output <- dplyr::full_join(collapse_list_output, listdfs[[i]])}
      }
      return(collapse_list_output)
    }
    
    #Then collapse x and collapse y into dfs as needed
    if (is.data.frame(x) & is.data.frame(y)) {
      warning("collapse = TRUE, but both x and y are data.frames already\n")
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
  
  if (is.null(y)) {output <- x
  } else {
    #Join x and y
    if(join == "full") {
      output <- dplyr::full_join(x = x, y = y, by = by, ...)
      if(nrow(output) > nrow(x) & nrow(output) > nrow(y)) {
        warning("\nmerged_df has more rows than x and than y, this may indicate
               mis-matched values in the shared column(s) used to merge 
              (e.g. 'Well')\n")
      }
    } else if (join == "left") {
      output <- dplyr::left_join(x = x, y = y, by = by, ...)
    } else if (join == "right") {
      output <- dplyr::right_join(x = x, y = y, by = by, ...)
    } else if (join == "inner") {
      output <- dplyr::inner_join(x = x, y = y, by = by, ...)
    }
  }
  
  if (drop) {
    message(sum(!stats::complete.cases(output)), " rows were dropped as incomplete")
    output <- output[stats::complete.cases(output), ]}
  
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
#' @param nested_metadata A logical indicating the existence of nested metadata
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
      message("Inferring nested_metadata to be FALSE\n")
    } else if (all(sapply(blocks, simplify = TRUE, FUN = class) == "list")) {
      nested_metadata <- TRUE
      message("Inferring nested_metadata to be TRUE\n")
    } else {
      stop("Unable to infer nested_metadata, this may be because blocks vary in nestedness or are not data.frame's")
    }
  }
  
  #Check that all blocks have same dimensions
  if (length(blocks) > 1) {
    if (nested_metadata) { #there is nested metadata
      if (!all_same(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x[[1]])[1]}))) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (!all_same(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x[[1]])[2]}))) {
        stop("Not all blocks have the same number of columns of data")
      }
    } else { #there is not nested metadata
      if (!all_same(sapply(blocks, simplify = TRUE, 
                            FUN = function(x) {dim(x)[1]}))) {
        stop("Not all blocks have the same number of rows of data")
      }
      if (!all_same(sapply(blocks, simplify = TRUE,
                            FUN = function(x) {dim(x)[2]}))) {
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
#' @param coerce_NA  logical dictating if strings matching any of 
#'                   \code{na.strings} will be coerced into  \code{NA} values 
#'                   after separating.
#' @param na.strings A character vector of strings which are to be interpreted
#'                   as \code{NA} values if \code{coerce_NA == TRUE}
#' @param ... Other arguments passed to \code{tidyr::separate}
#' 
#' @return A data frame containing new columns in the place of \code{col}
#' 
#' @export
separate_tidy <- function(data, col, into = NULL, sep = "_",
                          coerce_NA = TRUE, na.strings = "NA", ...) {
  if(is.null(into)) {
    if(col %in% colnames(data)) {
      into <- strsplit(col, split = sep)[[1]]
    } else if (is.numeric(col)) {
      into <- strsplit(colnames(data)[col], split = sep)[[1]]
    } else {
      stop("into is NULL, but col is neither numeric nor a column name in data")
    }
  }
  
  output <- tidyr::separate(data = data, col = col, into = into, sep = sep, ...)
  if(coerce_NA == TRUE) {
    for (idx in which(colnames(output) %in% into)) {
      output[!is.na(output[, idx]) & output[, idx] %in% na.strings, idx] <- NA
    }
  }
  return(output)
}


# Processing: Smoothing ----

#' Smooth data
#' 
#' This function calls other functions to smooth growth curve data
#' 
#' @param ... Arguments passed to \code{stats::loess}, \code{mgcv::gam},
#'            \code{moving_average}, \code{moving_median}, or 
#'            \code{stats::smooth.spline}. Typically includes tuning 
#'            parameter(s), which in some cases are required.
#'            See Details for more information.
#' @param x An (often optional) vector of predictor values to smooth along 
#'          (e.g. time)
#' @param y A vector of response values to be smoothed (e.g. density). If NULL,
#'          \code{formula} and \code{data} *must* be provided via \code{...}
#' @param sm_method Argument specifying which smoothing method should
#'                  be used to smooth data. Options include 
#'                  "moving-average", "moving-median", "loess", "gam",
#'                  and "smooth.spline".
#' @param subset_by An optional vector as long as \code{y}. 
#'                  \code{y} will be split by the unique values of this vector 
#'                  and the derivative for each group will be calculated 
#'                  independently of the others.
#'                  
#'                  This provides an internally-implemented approach similar
#'                  to \code{dplyr::group_by} and \code{dplyr::mutate}
#' @param return_fitobject logical indicating whether entire object returned
#'                         by fitting function should be returned. If FALSE,
#'                         just fitted values are returned.
#'
#' @details 
#'            For \code{moving_average} and \code{moving_median}, 
#'            passing \code{window_width} or \code{window_width_n} via 
#'            \code{...} is required. \code{window_width} sets the width
#'            of the moving window in units of \code{x}, while 
#'            \code{window_width_n} sets the width in units of number
#'            of data points. Larger values for either will produce more 
#'            "smoothed" data.
#'            
#'            For \code{loess}, the \code{span} argument sets the fraction of
#'            data points that should be included in each calculation. It's
#'            typically best to specify, since the default of 0.75 is often
#'            too large for growth curves data. Larger values of \code{span} 
#'            will produce more more "smoothed" data
#'            
#'            For \code{gam}, both arguments to \code{gam} and \code{s} can
#'            be provided via \code{...}. Most frequently, the \code{k} 
#'            argument to \code{s} sets the number of "knots" the
#'            spline-fitting can use. Smaller values will be more "smoothed".
#'            
#'            When using \code{sm_method = "gam"}, advanced users may also modify 
#'            other parameters of \code{s()}, including the smoothing basis 
#'            \code{bs}. These bases can be thin plate (\code{bs = "tp"}, 
#'            the default), cubic regressions (\code{bs = "cr"}), or many other 
#'            options (see \code{?mcgv::s}). I recommend leaving the default 
#'            thin plate regressions, whose main drawback is that they are 
#'            computationally intensive to calculate. For growth curves data, 
#'            this is unlikely to be relevant.
#'            
#'            As an alternative to passing \code{y}, for more advanced needs 
#'            with \code{loess} or \code{gam}, \code{formula} and \code{data} 
#'            can be passed to \code{smooth_data} via the \code{...} argument 
#'            (in lieu of \code{y}).
#'          
#'            In this case, the formula should specify the response (e.g. density) 
#'            and predictors. For \code{gam} smoothing, the formula should
#'            typically be of the format: y ~ s(x), which uses 
#'            \code{mgcv::s} to smooth the data. The data argument should be a 
#'            \code{data.frame} containing the variables in the formula.
#'            In such cases, \code{subset_by} can still be specified as a vector
#'            with length \code{nrow(data)}
#' 
#' @return If \code{return_fitobject == FALSE:}
#' 
#'         A vector, the same length as \code{y}, with the now-smoothed y values
#'         
#'         If \code{return_fitobject == TRUE:}
#'         
#'         A list the same length as unique(subset_by) where each element is
#'         an object of the same class as returned by the smoothing method
#'         (typically a named list-like object)
#' 
#' @export
smooth_data <- function(..., x = NULL, y = NULL, sm_method, subset_by = NULL,
                        return_fitobject = FALSE) {
  if(!sm_method %in% c("moving-average", "moving-median", "gam", 
                       "loess", "smooth.spline")) {
    stop("sm_method must be one of: moving-average, moving-median, gam, loess, or smooth.spline")
  }
  
  check_grouped(name_for_error = "smooth_data", subset_by = subset_by)
  
  #Parse x and y, and/or ... args, into formula and data
  if(!is.null(y)) {
    if (any(c("formula", "data") %in% names(list(...)))) {
      warning("y is specified, ignoring formula and data arguments")
    }
    if(is.null(x)) {x <- 1:length(y)}
    if(length(x) != length(y)) {stop("x and y must be the same length")}
    data <- data.frame(x, y)
    if(sm_method != "gam") {
      formula <- y ~ x
    } else {
      formula <- y ~ s(x)
      #Detect any args that need to be passed within s() and
      # put them into the formula manually
      if(any(names(list(...)) %in% names(formals(mgcv::s)))) {
        idxs <- which(names(list(...)) %in% names(formals(mgcv::s)))
        for(idx in idxs) {
          formula[[3]][[length(formula[[3]])+1]] <- list(...)[[idx]]
          names(formula[[3]])[[length(formula[[3]])]] <- names(list(...))[idx]
        }
        names(formula[[3]])[1:2] <- c("", "")
      }
    }
  } else { #y is null
    if(!all(c("formula", "data") %in% names(list(...)))) {
      stop("specify either y or (via ...) formula and data")
    }
    formula <- list(...)$formula
    data <- list(...)$data
  }
  
  if (sm_method == "gam") {
    if(!requireNamespace("mgcv", quietly = TRUE)) {
      stop("Package \"mgcv\" must be installed to use gam", call. = FALSE)}
    if(substr(as.character(formula[3]), 1, 2) != "s(") {
      warning("gam method is called without 's()' to smooth\n")}
  }
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
    if (sm_method == "moving-average") {
      smoothed_object <- 
        list(
          moving_average(formula = formula, 
                         data = data[subset_by == unique(subset_by)[i], ],
                         ...))
      names(smoothed_object) <- "fitted"
    } else if (sm_method == "moving-median") {
      smoothed_object <-
        list(
          moving_median(formula = formula,
                        data = data[subset_by == unique(subset_by)[i], ],
                        ...))
      names(smoothed_object) <- "fitted"
    } else if (sm_method == "loess") {
      smoothed_object <- 
        stats::loess(formula = formula, 
                     data = data[subset_by == unique(subset_by)[i], ],
                     na.action = "na.exclude", ...)
    } else if (sm_method == "gam") {
      smoothed_object <- 
        parse_dots(
          FUN = mgcv::gam,
          formula = formula, data = data[subset_by == unique(subset_by)[i], ],
          na.action = "na.exclude", ...)
    } else if (sm_method == "smooth.spline") {
      smoothed_object <-
        parse_dots(
          FUN = gc_smooth.spline,
          SUBFUN = stats::smooth.spline,
          x = x, y = y,
          ...)
    }
    
    #Store results as requested
    if (return_fitobject) {
      fits_list[[i]] <- smoothed_object
    } else {
      #Fill in output if needed
      if(sm_method %in% c("gam", "loess")) {
        out[subset_by == unique(subset_by)[i]] <-
          stats::predict(smoothed_object, newdata = data[subset_by == unique(subset_by)[i], ])
      } else if (sm_method == "smooth.spline") {
        out[subset_by == unique(subset_by)[i]] <- smoothed_object$y
      } else {
        out[subset_by == unique(subset_by)[i]] <- smoothed_object[["fitted"]]
      }
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
#' @param window_width Width of the moving average window (in units of \code{x})
#' @param na.rm logical whether NA's should be removed before analyzing
#' 
#' @return Vector of smoothed data, with NA's appended at both ends
#' 
#' @export   
moving_average <- function(formula, data, window_width_n = NULL, 
                           window_width = NULL, na.rm = TRUE) {
  if(is.null(window_width) & is.null(window_width_n)) {
    stop("window_width or window_width_n must be provided")}
  
  #Check window width
  if(!is.null(window_width_n) && window_width_n %% 2 == 0) {
      stop("window_width_n must be an odd number")}
  
  #Check formula formatting
  if (length(formula) < 3) {stop("No response variable specified")}
  if (length(formula[[3]]) > 1) {stop("Multiple predictors in formula")}
  
  #Parse formula
  response_var <- as.character(formula[[2]])
  predictor_var <- as.character(formula[[3]])
  
  #Check for vars in data
  stopifnot(response_var %in% colnames(data),
            predictor_var %in% colnames(data))
  
  #Check x for being correct format
  if(!is.numeric(data[, predictor_var])) {
    if (!canbe.numeric(data[, predictor_var])) {
      warning(paste0("data is being sorted by order(", predictor_var,
                    "), but ", predictor_var, " is not numeric\n"))
    } else {x <- as.numeric(data[, predictor_var])} #it can be coerced
  }
  
  #Check y for being the correct format
  data[, response_var] <- make.numeric(data[, response_var], response_var)
  
  #remove nas
  narm_temp <- rm_nas(x = data[, predictor_var], y = data[, response_var], 
                      na.rm = na.rm, stopifNA = TRUE)
  
  #Reorder data
  order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
  
  #Make temp vectors of x and y
  x <- order_temp[["x"]]
  y <- order_temp[["y"]]
  
  #Get windows
  windows <- get_windows(x = x, y = y, window_width_n = window_width_n,
                         window_width = window_width, edge_NA = TRUE)
  #Calculate average
  results <- sapply(windows, y = y, FUN = function(x, y) {mean(y[x])})
  #Put back in original order
  results <- results[order(order_temp[["order"]])]
  #Add NA's
  results <- add_nas(x = results, 
                     nas_indices_removed = narm_temp[["nas_indices_removed"]])[["x"]]
  
  return(results)
}

#' Moving median smoothing
#' 
#' This function uses a moving median to smooth data
#' 
#' @param formula Formula specifying the numeric response (density) 
#'                and numeric predictor (time).
#' @param data Dataframe containing variables in \code{formula}
#' @param window_width_n Number of data points wide the moving median window is
#'                     (therefore, must be an odd number of points)
#' @param window_width Width of the moving median window (in units of \code{x})|
#' @param na.rm logical whether NA's should be removed before analyzing
#' 
#' @return Vector of smoothed data, with NA's appended at both ends
#' 
#' @export   
moving_median <- function(formula, data, window_width_n = NULL, 
                          window_width = NULL, na.rm = TRUE) {
  if(is.null(window_width) & is.null(window_width_n)) {
    stop("window_width or window_width_n must be provided")}
  
  #Check window width
  if(!is.null(window_width_n) && window_width_n %% 2 == 0) {
    stop("window_width_n must be an odd number")}
  
  #Check formula formatting
  if (length(formula) < 3) {stop("No response variable specified")}
  if (length(formula[[3]]) > 1) {stop("Multiple predictors in formula")}
  
  #Parse formula
  response_var <- as.character(formula[[2]])
  predictor_var <- as.character(formula[[3]])
  
  #Check for vars in data
  stopifnot(response_var %in% colnames(data),
            predictor_var %in% colnames(data))
  
  #Check x for being correct format
  if(!is.numeric(data[, predictor_var])) {
    if (!canbe.numeric(data[, predictor_var])) {
      warning(paste0("data is being sorted by order(", predictor_var,
                    "), but ", predictor_var, " is not numeric\n"))
    } else {x <- as.numeric(data[, predictor_var])} #it can be coerced
  }
  
  #Check y for being the correct format
  data[, response_var] <- make.numeric(data[, response_var], response_var)
  
  #remove nas
  narm_temp <- rm_nas(x = data[, predictor_var], y = data[, response_var], 
                      na.rm = na.rm, stopifNA = TRUE)  
  #Reorder data
  order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
  
  #Make temp vectors of x and y
  x <- order_temp[["x"]]
  y <- order_temp[["y"]]
  
  #Get windows
  windows <- get_windows(x = x, y = y, window_width_n = window_width_n,
                         window_width = window_width, edge_NA = TRUE)
  #Calculate median
  results <- sapply(windows, y = y, FUN = function(x, y) {stats::median(y[x])})
  #Put back in original order
  results <- results[order(order_temp[["order"]])]
  #Add NA's
  results <- 
    add_nas(x = results, 
            nas_indices_removed = narm_temp[["nas_indices_removed"]])[["x"]]
  
  return(results)
}

#' Fit a Smoothing Spline
#' 
#' This function is a wrapper for \code{stats::smooth.spline}, which fits 
#' a cubic smoothing spline to the supplied data, but includes the option
#' to remove \code{NA} values, and returns values in the original order.
#' 
#' @param x A vector giving the values of the predictor variable.
#' @param y A vector giving the values of the response variable. If \code{y} is
#'          missing or \code{NULL}, the responses are assumed to be specified
#'          by \code{x}, with \code{x} the index vector.
#' @param ... Additional arguments passed to \code{stats::smooth.spline}.
#' @param na.rm logical whether NA's should be removed before analyzing.
#'              Required to be TRUE if any \code{x} or \code{y} values are NA.
#'              
#' @details See \code{stats::smooth.spline}              
#' 
#' @return Similar to \code{stats::smooth.spline}, an object of class 
#'         "\code{smooth.spline}" with many components. Differs in that
#'         x, y, and w have NA's at any indices where \code{x} or \code{y} were 
#'         NA in the inputs, and x, y, and w are returned to match the input 
#'         \code{x} in order and length
#' 
#' @export   
gc_smooth.spline <- function(x, y = NULL, ..., na.rm = TRUE) {
  #remove NAs
  xy_nas_removed <- rm_nas(x = x, y = y, 
                           na.rm = na.rm, stopifNA = TRUE)
  if(length(xy_nas_removed[["y"]]) <= 1) {
    return(list(x = rep(NA, length(x)), y = rep(NA, length(x))))}
  
  #Reorder as needed
  xy_reordered <- reorder_xy(x = xy_nas_removed[["x"]], y = xy_nas_removed[["y"]])
  
  #Calculate
  ans <- stats::smooth.spline(x = xy_reordered[["x"]],
                              y = xy_reordered[["y"]],
                              ...)
  
  #Back to original order
  # (but smooth.spline only returns distinct values of x, so this matching
  # duplicates any values that were not distinct in the input
  ans$w <- ans$w[match(xy_nas_removed[["x"]], ans$x)]
  ans$y <- ans$y[match(xy_nas_removed[["x"]], ans$x)]
  ans$x <- ans$x[match(xy_nas_removed[["x"]], ans$x)]
  
  #Add NAs
  ans$x <- add_nas(
    x = ans$x,
    nas_indices_removed = xy_nas_removed[["nas_indices_removed"]])[["x"]]
  ans$y <- add_nas(
    x = ans$y,
    nas_indices_removed = xy_nas_removed[["nas_indices_removed"]])[["x"]]
  ans$w <- add_nas(
    x = ans$w,
    nas_indices_removed = xy_nas_removed[["nas_indices_removed"]])[["x"]]
  
  return(ans)
}


# Processing: Derivatives ----

#' Calculate derivatives of vector of data
#' 
#' Provided a vector of y values, this function returns either the plain
#' or per-capita difference or derivative between sequential values
#' 
#' @param y       Data to calculate difference or derivative of
#' @param x       Vector of x values provided as a simple numeric.
#' @param return One of c("difference", "derivative") for whether the
#'               differences in \code{y} should be returned, or the
#'               derivative of \code{y} with respect to \code{x}
#' @param percapita When percapita = TRUE, the per-capita difference or
#'                  derivative is returned
#' @param x_scale Numeric to scale x by in derivative calculation
#'                
#'                Set x_scale to the ratio of the units of 
#'                x to the desired units. E.g. if x is in seconds, but the 
#'                desired derivative is in units of /minute, set 
#'                \code{x_scale = 60} (since there are 60 seconds in 1 minute).
#' @param blank   y-value associated with a "blank" where the density is 0.
#'                Is required when \code{percapita = TRUE}.
#'                
#'                If a vector of blank values is specified, blank values are
#'                assumed to be in the same order as unique(subset_by)  
#' @param subset_by An optional vector as long as \code{y}. 
#'                  \code{y} will be split by the unique values of this vector 
#'                  and the derivative for each group will be calculated 
#'                  independently of the others.
#'                  
#'                  This provides an internally-implemented approach similar
#'                  to \code{dplyr::group_by} and \code{dplyr::mutate}
#' @param window_width_n,window_width
#'                  Set how many data points are used to determine
#'                  the slope at each point.
#'                       
#'                  When both are \code{NULL}, \code{calc_deriv} 
#'                  calculates the difference or derivative
#'                  of each point with the next point, appending
#'                  \code{NA} at the end.
#'                       
#'                  When one or both are specified, a linear regression 
#'                  is fit to all points in the window to determine the 
#'                  slope.
#'                       
#'                  \code{window_width_n} specifies the width of the
#'                  window in number of data points. \code{window_width}
#'                  specifies the width of the window in units of \code{x}.
#'                       
#'                  When using \code{window_width} and \code{window_width_n} 
#'                  at the same time, windows are conservative. Points 
#'                  included in each window will meet both the 
#'                  \code{window_width} and the \code{window_width_n}.
#'                  
#'                  A value of \code{window_width_n = 3} or 
#'                  \code{window_width_n = 5} is often the most effective.
#' @param trans_y  One of \code{c("linear", "log")} specifying the
#'                 transformation of y-values.
#' 
#'                 \code{'log'} is only available when calculating per-capita
#'                 derivatives using a fitting approach (when non-default 
#'                 values are specified for \code{window_width} or 
#'                 \code{window_width_n}).
#' 
#'                 For per-capita growth expected to be exponential or 
#'                 nearly-exponential, \code{"log"} is recommended, since 
#'                 exponential growth is linear when log-transformed. However, 
#'                 log-transformations must be used with care, since y-values 
#'                 at or below 0 will become undefined and results will be 
#'                 more sensitive to incorrect values of \code{blank}.
#' @param na.rm logical whether NA's should be removed before analyzing
#' 
#' @details For per-capita derivatives, \code{trans_y = 'linear'} and
#'          \code{trans_y = 'log'} approach the same value as time resolution
#'          increases. 
#'          
#'          For instance, let's assume exponential growth \eqn{N = e^rt} with 
#'          per-capita growth rate \eqn{r}.
#'          
#'          With \code{trans_y = 'linear'}, note that \eqn{dN/dt = r e^rt = r N}. 
#'          So we can calculate per-capita growth rate as \eqn{r = dN/dt * 1/N}. 
#'          
#'          With \code{trans_y = 'log'}, note that \eqn{log(N) = log(e^rt) = rt}.
#'          So we can calculate per-capita growth rate as the slope of a linear
#'          fit of \eqn{log(N)} against time, \eqn{r = log(N)/t}.
#' 
#' @return A vector of values for the plain (if \code{percapita = FALSE})
#'         or per-capita (if \code{percapita = TRUE}) difference 
#'         (if \code{return = "difference"}) or derivative 
#'         (if \code{return = "derivative"}) between \code{y} values. Vector
#'         will be the same length as \code{y},  with \code{NA} values 
#'         at the ends
#' 
#' @export   
calc_deriv <- function(y, x = NULL, return = "derivative", percapita = FALSE,
                       x_scale = 1, blank = NULL, subset_by = NULL, 
                       window_width = NULL, window_width_n = NULL, 
                       trans_y = "linear", na.rm = TRUE) {
  #Check inputs
  if(!is.null(window_width_n) && window_width_n %% 2 == 0) {
    stop("window_width_n must be an odd number")}
  
  if(!return %in% c("derivative", "difference")) {
    stop("return must be one of c('derivative', 'difference')")}
  
  if(return == "difference" && 
     (!is.null(window_width_n) | !is.null(window_width))) {
    stop("return must be 'derivative' when window_width or window_width_n are used")}
  
  if(!trans_y %in% c("linear", "log")) {
    stop("trans_y must be one of c('linear', 'log')")}
  
  if(trans_y == "log" && (return == "difference" || percapita == FALSE)) {
    stop("when trans_y = 'log', return must be 'derivative' and percapita must be 'TRUE'")}
  
  if(is.null(y)) {stop("y must be provided")}
  if(length(x_scale) > 1) {stop("x_scale must be a single value")}
  
  x_scale <- make.numeric(x_scale, "x_scale")
  if(is.na(x_scale)) {stop("x_scale cannot be NA")}
  
  x <- make.numeric(x, "x")
  y <- make.numeric(y, "y")
  
  check_grouped(name_for_error = "calc_deriv", subset_by = subset_by)

  #Set up subset_by
  if(is.null(subset_by)) {subset_by <- rep("A", length(y))}
  
  #Set up blank values
  if(is.null(blank)) {
    if(percapita == TRUE) {stop("percapita == TRUE but blank is NULL")}
  } else { #blank is not NULL
    blank <- check_input_dimensions(
      blank, "blank", needed_len = length(unique(subset_by)),
      needed_name = "the number of unique subset_by values") 
  }

  #Calc derivative
  ans <- rep(NA, length(y))
  for (i in 1:length(unique(subset_by))) {
    indices <- which(subset_by == unique(subset_by)[i])
    
    sub_y <- y[indices]
    sub_x <- x[indices]
    
    #Blank subtraction
    if(!is.null(blank)) {sub_y <- sub_y - blank[i]}
    
    if(trans_y == "log") {
      caught_log <- gcTryCatch(log(sub_y))
      if(!is.null(caught_log$warning)) {
        warning(paste("during log-transformation,", caught_log$warning))
        caught_log$value[is.nan(caught_log$value)] <- NA}
      if(!is.null(caught_log$error)) {
        stop(paste("during log-transformation,", caught_log$error))}
      sub_y <- caught_log$value
      if(any(is.infinite(sub_y))) {
        warning("infinite values created during log-transformation, treating as NA's")
        sub_y[is.infinite(sub_y)] <- NA
      }
    }
    
    #remove nas
    narm_temp <- rm_nas(x = sub_x, y = sub_y, 
                        na.rm = na.rm, stopifNA = FALSE)
    
    if(length(narm_temp[["y"]]) <= 1) {ans[indices] <- NA; next}
    
    #Reorder as needed
    order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
    
    #Save vectors to temp vars
    sub_y <- order_temp[["y"]]
    sub_x <- order_temp[["x"]]
    
    if(is.null(window_width) & is.null(window_width_n)) {
      #Calculate differences
      sub_ans <- sub_y[2:length(sub_y)]-sub_y[1:(length(sub_y)-1)]
      
      #Derivative & rescale (if specified)
      if(return == "derivative") {
        sub_ans <- sub_ans/
          ((sub_x[2:length(sub_x)]-sub_x[1:(length(sub_x)-1)])/x_scale)
      }
      
      #Percapita
      # (if trans_y = 'linear', need to divide by y to make percap
      #  if trans_y = 'log', must be deriv and deriv is already percap)
      if(percapita && trans_y == 'linear') {
        sub_ans <- sub_ans/sub_y[1:(length(sub_y)-1)]}
    } else {
      sub_ans <- rep(NA, length(sub_x))

      windows <- get_windows(x = sub_x, y = sub_y, edge_NA = TRUE,
                             window_width_n = window_width_n, 
                             window_width = window_width)
      for (j in which(!is.na(windows))) {
        if(any(is.na(sub_y[windows[[j]]]) | is.infinite(sub_y[windows[[j]]]))) {
          sub_ans[j] <- NA
        } else {
          #get slope
          # (if trans_y = 'linear', slope is derivative
          #  if trans_y = 'log', slope is already percap deriv)
          lmoutput <- stats::lm(myy ~ myx, 
                            data = data.frame(myy = sub_y[windows[[j]]],
                                              myx = sub_x[windows[[j]]]))
          sub_ans[j] <- lmoutput$coefficients["myx"]*x_scale
          if(percapita == TRUE && trans_y == 'linear') {
            sub_ans[j] <- 
              sub_ans[j]/lmoutput$fitted.values[which(windows[[j]] == j)]
          }
        }
      }
    }
    
    #Back to original order
    sub_ans <- c(sub_ans[order(order_temp[["order"]])])
    
    #Add NA's
    sub_ans <- 
      add_nas(x = sub_ans,
              nas_indices_removed = narm_temp[["nas_indices_removed"]])[["x"]]
  
    #Save results
    ans[indices] <- sub_ans
  }
  return(ans)
}



#' Calculate doubling time equivalent of per-capita growth rate
#' 
#' Provided a vector of per-capita growth rates, this function returns 
#' the vector of equivalent doubling times
#' 
#' @param y       Vector of per-capita derivative data to calculate the 
#'                equivalent doubling time of
#' @param x_scale Numeric to scale per-capita derivative values by
#'                
#'                Set x_scale to the ratio of the the units of 
#'                y to the desired units. E.g. if y is in per-second, but the 
#'                desired doubling time is in minutes, \code{x_scale = 60} 
#'                (since there are 60 seconds in 1 minute).
#' 
#' @return A vector of values for the doubling time equivalent to the
#'         per-capita growth rate supplied for \code{y}
#' 
#' @export   
doubling_time <- function(y, x_scale = 1) {
  #Check inputs
  y <- make.numeric(y, "y")
  x_scale <- make.numeric(x_scale, "x_scale")

  return(log(2)/(x_scale * y))
}

# Analyze ----

#' Find local extrema of a numeric vector
#' 
#' These functions take a vector of \code{y} values and identify local extrema.
#'   
#' @param y Numeric vector of y values in which to identify local extrema
#' @param x Optional numeric vector of corresponding x values
#' @param window_width Width of the window (in units of \code{x}) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param window_width_n The maximum number of data points a single 
#'                      extrema-search step is allowed to take. For example,
#'                      when maxima-finding, the function will not pass
#'                      a valley consisting of more than \code{window_width_n}
#'                      data points.
#'                      
#'                      A smaller \code{window_width_n} will be more sensitive 
#'                      to narrow local maxima/minima, while a larger 
#'                      \code{window_width_n} will be less sensitive to 
#'                      narrow local maxima/minima.
#' @param window_height The maximum change in \code{y} a single extrema-search
#'                     step is allowed to take.  For example, when 
#'                     maxima-finding, the function will not pass a
#'                     valley deeper than \code{window_height}.
#'                     
#'                     A smaller \code{window_height} will be more sensitive 
#'                     to shallow local maxima/minima, while a larger 
#'                     \code{window_height} will be less sensitive to 
#'                     shallow maxima/minima.
#' @param return One of c("index", "x", "y"), determining whether the function
#'               will return the index, x value, or y value associated with the
#'               identified extremas
#' @param return_maxima,return_minima logical for which classes of local extrema
#'                                    to return
#' @param return_endpoints Should the first and last values in \code{y}
#'                         be included if they are in the returned 
#'                         vector of extrema?
#' @param subset A vector of logical values indicating which x and y values
#'               should be included (TRUE) or excluded (FALSE).
#'               
#'               If \code{return = "index"}, index will be for the whole 
#'               vector and not the subset of the vector
#' @param na.rm logical whether NA's should be removed before analyzing
#' @param width_limit Deprecated, use \code{window_width} instead
#' @param width_limit_n Deprecated, use \code{window_width_n} instead
#' @param height_limit Deprecated, use \code{window_height} instead
#' @param ... (for \code{first_maxima} and \code{first_minima}), other 
#'            parameters to pass to \code{find_local_extrema}
#' 
#' @details 
#' For \code{find_local_extrema}, one of \code{window_width}, 
#' \code{window_width_n}, or \code{window_height} must be provided.
#' 
#' For \code{first_minima} and \code{first_maxima}, if none of 
#' \code{window_width}, \code{window_width_n}, or \code{window_height} are 
#' provided, \code{window_width_n} is set to 20% of the data by default.
#' 
#' If multiple of \code{window_width}, \code{window_width_n}, or 
#' \code{window_height} are provided, steps are limited conservatively 
#' (a single step must meet all criteria)
#' 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'  
#' In the case of exact ties in \code{y} values within a window, only the 
#' first local extrema is returned.
#' 
#' @return 
#'    \code{find_local_extrema} returns a vector corresponding to all the 
#'    found local extrema.
#' 
#'    \code{first_maxima} returns only the first maxima, so is a shortcut for 
#'    \code{find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]}
#' 
#'    \code{first_minima} returns only the first minima, so is a shortcut for
#'    \code{find_local_extrema(return_maxima = FALSE, return_minima = TRUE)[1]}
#' 
#'    If \code{return = "index"}, the returned value(s) are the indices
#'    corresponding to local extrema in the data
#'           
#'    If \code{return = "x"}, the returned value(s) are the x value(s) 
#'    corresponding to local extrema in the data
#'          
#'    If \code{return = "y"}, the returned value(s) are the y value(s)
#'    corresponding to local extrema in the data
#' 
#' @name ExtremaFunctions
NULL


#' @rdname ExtremaFunctions
#' @export                             
find_local_extrema <- function(y, x = NULL, 
                               window_width = NULL,
                               window_width_n = NULL,
                               window_height = NULL,
                               return = "index",
                               return_maxima = TRUE, return_minima = TRUE,
                               return_endpoints = TRUE,
                               subset = NULL, na.rm = TRUE,
                               width_limit = NULL, width_limit_n = NULL,
                               height_limit = NULL) {
  #width_limit, width_limit_n, and height_limit were deprecated in 11.2.9000
  if(!missing("width_limit")) {
    warning("width_limit deprecated, use window_width instead")
    window_width <- width_limit
  }
  if(!missing("width_limit_n")) {
    warning("width_limit_n deprecated, use window_width_n instead")
    window_width_n <- width_limit_n
  }
  if(!missing("height_limit")) {
    warning("height_limit deprecated, use window_width instead")
    window_height <- height_limit
  }
  
  #Check inputs
  if (!return_maxima & !return_minima) {
    stop("Both return_maxima and return_minima are FALSE, at least one must be TRUE")
  }
  #Check inputs
  if (is.null(window_width_n) & is.null(window_height) & is.null(window_width)) {
    stop("Either window_width, window_width_n, or window_height must be provided")
  }
  if (!is.null(window_width) & is.null(x)) {
    stop("window_width is specified, but x is not provided")
  }
  if (!is.null(window_width_n) && window_width_n%%2 == 0) {
    stop("window_width_n must be an odd number")}
  
  if (!return %in% c("x", "y", "index")) {
    stop('return must be one of "x", "y", or "index"')
  }
  if(!is.null(x) & length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(is.null(x) & return == "x") {stop('return = "x" but x is not provided')}
  
  #Numeric checks/coercion
  y <- make.numeric(y, "y")
  if(is.null(x)) {x <- 1:length(y)
  } else {x <- make.numeric(x)}
  
  #Take subset
  subset_temp <- take_subset(x = x, y = y, subset = subset)
  if(length(subset_temp$y) == 0) {return(NA)}
  
  #remove nas
  narm_temp <- rm_nas(x = subset_temp$x, y = subset_temp$y, 
                      na.rm = na.rm, stopifNA = TRUE)
  if(length(narm_temp$y) == 0) {return(NA)}
  
  #reorder
  order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
  
  #Save to temp vars
  x <- order_temp[["x"]]
  y <- order_temp[["y"]]
  
  windows <- get_windows(x = x, y = y, window_width_n = window_width_n,
              window_width = window_width, window_height = window_height,
              edge_NA = FALSE, force_height_multi_n = TRUE)
  
  #ID extrema as those points that are the local max or min
  # in the window centered on them (all non-local extrema should point to
  # some other point in the window centered on them)
  maxima_list <- 
    which(1:length(windows) ==
          sapply(X = windows, y = y, FUN = function(x, y) {x[which.max(y[x])]}))
  minima_list <- 
    which(1:length(windows) ==
          sapply(X = windows, y = y, FUN = function(x, y) {x[which.min(y[x])]}))
  
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
  
  #Return as specified
  if (return == "index") {
    #return to original order
    output <- order_temp[["order"]][output]
    
    #Change indices to account for NA's removed
    for (index in narm_temp$nas_indices_removed) {
      output[output >= index] <- (output[output >= index] + 1)
    }
    
    #Change indices to account for subset being used
    output <- subset_temp$indices[output]
    
    return(output[order(output)])
  } else if (return == "x") {
    #return using the subsetted, re-ordered, and na-removed x vals & indices
    return(x[output])
  } else if (return == "y") {
    #return using the subsetted, re-ordered, and na-removed y vals & indices
    return(y[output])
  }
}

            
#' @rdname ExtremaFunctions
#' @export 
first_maxima <- function(y, x = NULL, 
                       window_width = NULL,
                       window_width_n = NULL,
                       window_height = NULL,
                       return = "index", return_endpoints = TRUE, 
                       ...) {
  if(is.null(window_width) & is.null(window_width_n) & is.null(window_height)) {
    window_width_n <- round(0.2*length(y)) - (1 - round(0.2*length(y))%%2)
  }
  
  if (any(c("return_maxima", "return_minima") %in% names(list(...)))) {
    stop("return_maxima and return_minima cannot be changed in first_peak, 
use find_local_extrema for more flexibility")
  }
  
  return(find_local_extrema(y = y, x = x,
                            return_maxima = TRUE,
                            return_minima = FALSE,
                            return_endpoints = return_endpoints,
                            window_width = window_width,
                            window_width_n = window_width_n,
                            window_height = window_height,
                            return = return, ...)[1])
}

#' @rdname ExtremaFunctions
#' @export 
first_minima <- function(y, x = NULL, 
                         window_width = NULL,
                         window_width_n = NULL,
                         window_height = NULL,
                         return = "index", return_endpoints = TRUE, 
                         ...) {
  if(is.null(window_width) & is.null(window_width_n) & is.null(window_height)) {
    window_width_n <- round(0.2*length(y)) - (1 - round(0.2*length(y))%%2)
  }
  
  if (any(c("return_maxima", "return_minima") %in% names(list(...)))) {
    stop("return_maxima and return_minima cannot be changed in first_peak, 
use find_local_extrema for more flexibility")
  }
  
  return(find_local_extrema(y = y, x = x,
                            return_maxima = FALSE,
                            return_minima = TRUE,
                            return_endpoints = return_endpoints,
                            window_width = window_width,
                            window_width_n = window_width_n,
                            window_height = window_height,
                            return = return, ...)[1])
}


#' Find point(s) when a numeric vector crosses some threshold
#' 
#' These functions take a vector of \code{y} values and identify points where
#' the \code{y} values cross some \code{threshold} y value.
#' 
#' @param y Numeric vector of y values in which to identify threshold
#'          crossing event(s)
#' @param x Optional numeric vector of corresponding x values
#' @param threshold Threshold y value of interest
#' @param return One of \code{c("index", "x")}, determining whether the function
#'               will return the \code{index} or \code{x} value associated with the
#'               threshold-crossing event.
#'               
#'               If \code{index}, it will refer to the data point immediately after
#'               the crossing event.
#'               
#'               If \code{x}, it will use linear interpolation and the data
#'               points immediately before and after the threshold-crossing
#'               to return the exact \code{x} value when the threshold crossing
#'               occurred
#' @param subset A vector of logical values indicating which x and y values
#'               should be included (TRUE) or excluded (FALSE).
#'               
#'               If \code{return = "index"}, index will be for the whole 
#'               vector and not the subset of the vector
#' @param return_rising logical for whether crossing events where \code{y}
#'                      rises above \code{threshold} should be returned
#' @param return_falling logical for whether crossing events where \code{y}
#'                      falls below \code{threshold} should be returned
#' @param return_endpoints logical for whether startpoint should be returned
#'                      when the startpoint is above \code{threshold} and
#'                      \code{return_rising = TRUE}, or when the startpoint is
#'                      below \code{threshold} and \code{return_falling = TRUE}
#' @param na.rm logical whether NA's should be removed before analyzing.
#'              If \code{return = 'index'}, indices will refer to the original
#'              \code{y} vector *including* \code{NA} values
#' @param ... (for \code{first_above} and \code{first_below}) other arguments 
#'            to pass to \code{find_threshold_crosses}
#'              
#' @details 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'  
#' @return 
#'    \code{find_threshold_crosses} returns a vector corresponding to all the 
#'    threshold crossings.
#' 
#'    \code{first_above} returns only the first time the \code{y} values
#'    rise above the threshold, so is a shortcut for 
#'    \code{find_threshold_crosses(return_rising = TRUE, return_falling = FALSE)[1]}
#' 
#'    \code{first_below} returns only the first time the \code{y} values
#'    fall below the threshold, so is a shortcut for 
#'    \code{find_threshold_crosses(return_rising = FALSE, return_falling = TRUE)[1]}
#' 
#'    If \code{return = "index"}, the returned value(s) are the indices
#'    immediately following threshold crossing(s)
#'           
#'    If \code{return = "x"}, the returned value(s) are the x value(s) 
#'    corresponding to threshold crossing(s)
#'    
#'    If no threshold-crossings are detected that meet the criteria, will
#'    return \code{NA}
#'          
#' @name ThresholdFunctions
NULL


#' @rdname ThresholdFunctions
#' @export 
find_threshold_crosses <- function(y, x = NULL, threshold,
                                   return = "index",
                                   return_rising = TRUE, return_falling = TRUE,
                                   return_endpoints = TRUE,  
                                   subset = NULL, na.rm = TRUE) {
  if (!return %in% c("x", "index")) {
    stop('return must be "x" or "index"')
  }
  if(!is.null(x) & length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(return == "x" & is.null(x)) {stop("return = 'x' but x is not provided")}
  
  #Numeric checks/coercion
  y <- make.numeric(y, "y")
  x <- make.numeric(x, "x")
  
  #Take subset
  subset_temp <- take_subset(x = x, y = y, subset = subset)
  if(length(subset_temp$y) == 0) {return(NA)}
  
  #remove nas
  narm_temp <- rm_nas(x = subset_temp$x, y = subset_temp$y, 
                      na.rm = na.rm, stopifNA = TRUE)
  if(length(narm_temp$y) == 0) {return(NA)}
  
  #reorder
  order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
  
  #Save to temp vars
  x <- order_temp[["x"]]
  y <- order_temp[["y"]]
  
  #Check startpoint
  out_idx <- NULL
  if(return_endpoints) {
    if(return_rising & y[1] >= threshold) {out_idx <- c(out_idx, 1)}
    if(return_falling & y[1] <= threshold) {out_idx <- c(out_idx, 1)}
  }
  
  #Find indices of crossing events
  # (saving the index of the value *before* the cross has happened)
  if(return_rising) {
    out_idx <- c(out_idx, 1+which(y[2:length(y)] > threshold &
                                  y[1:(length(y)-1)] <= threshold))
  }
  if(return_falling) {
    out_idx <- c(out_idx, 1+which(y[2:length(y)] < threshold &
                                  y[1:(length(y)-1)] >= threshold))
  }
  
  if(length(out_idx) == 0) {return(NA)
  } else {out_idx <- out_idx[order(out_idx)]}
  
  if(return == "index") {
    #return to original order
    out_idx <- order_temp[["order"]][out_idx]
    
    #Change indices to account for NA's removed
    for (index in narm_temp$nas_indices_removed) {
      out_idx[out_idx >= index] <- (out_idx[out_idx >= index] + 1)
    }
  
    #Change indices to account for subset being used
    out_idx <- subset_temp$indices[out_idx]
    
    return(out_idx[order(out_idx)])
    
  } else { #return = "x"
    if(out_idx[1] == 1) {x2 <- x[1]; out_idx <- out_idx[-1]
    } else {x2 <- NULL}
    
    if(length(out_idx) > 0) {
      x2 <- c(x2, solve_linear(x1 = x[(out_idx-1)], y1 = y[(out_idx-1)],
                          x2 = x[out_idx], y2 = y[out_idx],
                          y3 = threshold, named = FALSE))
    }
    return(x2)
  }
}

#' @rdname ThresholdFunctions
#' @export  
first_below <- function(y, x = NULL, threshold, return = "index",
                        return_endpoints = TRUE, ...) {
  if(any(c("return_rising", "return_falling") %in% names(list(...)))) {
    stop("return_rising and return_falling cannot be changed in first_below,
please use find_threshold_crosses for more flexibility")
  }
  return(find_threshold_crosses(y = y, x = x,
                                threshold = threshold,
                                return = return, 
                                return_endpoints = return_endpoints,
                                return_rising = FALSE,
                                return_falling = TRUE,
                                ...)[1])
}

#' @rdname ThresholdFunctions
#' @export 
first_above <- function(y, x = NULL, threshold, return = "index",
                        return_endpoints = TRUE, ...) {
  if(any(c("return_rising", "return_falling") %in% names(list(...)))) {
    stop("return_rising and return_falling cannot be changed in first_below,
please use find_threshold_crosses for more flexibility")
  }
  return(find_threshold_crosses(y = y, x = x,
                                threshold = threshold,
                                return = return,
                                return_endpoints = return_endpoints,
                                return_rising = TRUE,
                                return_falling = FALSE,
                                ...)[1])
}

#' Calculate area under the curve
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
#' @param blank Value to be subtracted from \code{y} values before calculating
#'              area under the curve
#' @param subset A vector of logical values indicating which x and y values
#'               should be included (TRUE) or excluded (FALSE).
#' @param na.rm a logical indicating whether missing values should be removed
#' @param neg.rm a logical indicating whether \code{y} values below zero should 
#'               be treated as zeros. If \code{FALSE}, area under the curve
#'               for negative \code{y} values will be calculated normally,
#'               effectively subtracting from the returned value.
#' 
#' @details 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'
#' @return A scalar for the total area under the curve
#'             
#' @export
auc <- function(x, y, xlim = NULL, blank = 0, subset = NULL,
                na.rm = TRUE, neg.rm = FALSE) {
  if(!is.vector(x)) {stop(paste("x is not a vector, it is class:", class(x)))}
  if(!is.vector(y)) {stop(paste("y is not a vector, it is class:", class(y)))}
  
  x <- make.numeric(x)
  y <- make.numeric(y)
  
  #take subset
  subset_temp <- take_subset(x = x, y = y, subset = subset)
  
  #remove nas
  dat <- rm_nas(x = subset_temp$x, y = subset_temp$y, 
                na.rm = na.rm, stopifNA = TRUE)
  
  if(length(dat$y) <= 1) {return(NA)}
  
  #reorder
  dat <- reorder_xy(x = dat[["x"]], y = dat[["y"]])
  
  x <- dat[["x"]]
  y <- dat[["y"]]
  
  y <- y - blank

  #Check if xlim has been specified
  if(!is.null(xlim)) {
    stopifnot(is.vector(xlim), length(xlim) == 2, any(!is.na(xlim)))
    if(is.na(xlim[1])) {xlim[1] <- x[1]}
    if(is.na(xlim[2])) {xlim[2] <- x[length(x)]}
    if(xlim[1] < x[1]) {
      warning("xlim specifies lower limit below the range of x\n")
      xlim[1] <- x[1]
    } else { #add lower xlim to the x vector and the interpolated y to y vector
      if (!(xlim[1] %in% x)) {
        x <- c(x, xlim[1])
        xndx <- max(which(x < xlim[1]))
        y <- c(y, solve_linear(x1 = x[xndx], y1 = y[xndx],
                               x2 = x[xndx+1], y2 = y[xndx+1],
                               x3 = xlim[1], named = FALSE))
                               
        #reorder
        dat <- reorder_xy(x = x, y = y)
        
        x <- dat[["x"]]
        y <- dat[["y"]]
      }
    }
       
    if(xlim[2] > x[length(x)]) {
      warning("xlim specifies upper limit above the range of x\n")
      xlim[2] <- x[length(x)]
    } else { #add upper xlim to the x vector and the interpolated y to y vector
      if (!(xlim[2] %in% x)) {
        x <- c(x, xlim[2])
        xndx <- max(which(x < xlim[2]))
        y <- c(y, solve_linear(x1 = x[xndx], y1 = y[xndx],
                               x2 = x[xndx+1], y2 = y[xndx+1],
                               x3 = xlim[2], named = FALSE))
        
        #reorder
        dat <- reorder_xy(x = x, y = y)
        
        x <- dat[["x"]]
        y <- dat[["y"]]
      }
    }
    y <- y[(x >= xlim[1]) & (x <= xlim[2])]
    x <- x[(x >= xlim[1]) & (x <= xlim[2])]
  }
  
  if(any(y < 0)) {
    if(neg.rm == TRUE) {y[y < 0] <- 0
    } else {warning("some y values are below 0")}
  }
  
  #Calculate auc
  # area = 0.5 * (y1 + y2) * (x2 - x1)
  return(sum(0.5 * 
               (y[1:(length(y)-1)] + y[2:length(y)]) *
               (x[2:length(x)] - x[1:(length(x)-1)])))
}


#' Calculate lag time
#' 
#' Lag time is calculated by projecting a tangent line at the point
#' of maximum (per-capita) derivative backwards to find the time when it
#' intersects with the starting y-value
#' 
#' @param x Vector of x values (typically time)
#' @param y Vector of y values (typically density)
#' @param deriv Vector of derivative values (typically per-capita derivative)
#' @param trans_y  One of \code{c("linear", "log")} specifying the
#'                 transformation of y-values.
#' 
#'                 \code{'log'} is the default, producing calculations of
#'                 lag time assuming a transition to exponential growth
#'                 
#'                 \code{'linear'} is available for alternate uses
#' @param na.rm a logical indicating whether missing values should be removed
#' @param slope Slope to project from x1,y1 to y0 (typically per-capita growth
#'              rate). If not provided, will be calculated as \code{max(deriv)}
#' @param x1 x value (typically time) to project slope from. If not provided,
#'           will be calculated as \code{x[which.max(deriv)]}.
#' @param y1 y value (typically density) to project slope from. If not provided,
#'           will be calculated as \code{y[which.max(deriv)]}.
#' @param y0 y value (typically density) to find intersection of slope from
#'             x1, y1 with. If not provided, will be calculated as \code{min(y)}
#' @details 
#' For most typical uses, simply supply \code{x}, \code{y}, and \code{deriv}
#' (using the per-capita derivative and \code{trans_y = 'log'}).
#' 
#' Advanced users may wish to use alternate values for the slope, origination
#' point, or initial y-value. In that case, values can be supplied to
#' \code{slope}, \code{x1}, \code{y1}, and/or \code{y0}, which will override
#' the default calculations. If and only if all of \code{slope}, \code{x1}, 
#' \code{y1}, and \code{y0} are provided, \code{lag_time} is vectorized on
#' their inputs and will return a vector of lag time values.
#' 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'
#' @return Typically a scalar of the lag time in units of x. See Details for
#' cases when value will be a vector.
#'             
#' @export
lag_time <- function(x = NULL, y = NULL, deriv = NULL, 
                     trans_y = "log", na.rm = TRUE,
                     slope = NULL, x1 = NULL, y1 = NULL, y0 = NULL) {
  x <- make.numeric(x, "x")
  y <- make.numeric(y, "y")
  deriv <- make.numeric(deriv, "deriv")
  slope <- make.numeric(slope, "slope")
  y0 <- make.numeric(y0, "y0")
  y1 <- make.numeric(y1, "y1")
  x1 <- make.numeric(x1, "x1")
  
  narm_temp <- rm_nas(x = x, y = y, deriv = deriv, na.rm = na.rm)
  x <- narm_temp[["x"]]
  y <- narm_temp[["y"]]
  deriv <- narm_temp[["deriv"]]
  
  if(trans_y == "log") {
    if(!is.null(y) && length(y) > 0) {
      caught_log <- gcTryCatch(log(y))
      if(!is.null(caught_log$warning)) {
        warning(paste("during log-transformation,", caught_log$warning))
        caught_log$value[is.nan(caught_log$value)] <- NA}
      if(!is.null(caught_log$error)) {
        stop(paste("during log-transformation,", caught_log$error))}
      y <- caught_log$value
      if(any(is.infinite(y))) {
        warning("infinite values created during log-transformation, treating as NA's")
        y[is.infinite(y)] <- NA
      }
    }
    if(!is.null(y0)) {y0 <- log(y0)}
    if(!is.null(y1)) {y1 <- log(y1)}
  }
  
  narm_temp <- rm_nas(x = x, y = y, deriv = deriv, na.rm = na.rm)
  x <- narm_temp[["x"]]
  y <- narm_temp[["y"]]
  deriv <- narm_temp[["deriv"]]
  #(cases where ~all values are removed handled with return(NA) code below)
  
  if(is.null(slope)) {
    if(is.null(deriv)) {stop("deriv or slope must be provided")}
    if(length(deriv) < 1) {return(NA)}
    slope <- max(deriv, na.rm = na.rm)
  }
  if(length(y) < 2 && (is.null(y0) || is.null(y1))) {return(NA)}
  if(is.null(y0)) {
    if(is.null(y)) {stop("y or y0 must be provided")}
    if(length(y) < 1) {return(NA)}
    y0 <- min(y, na.rm = na.rm)
  }
  if(xor(is.null(x1), is.null(y1))) {
    stop("both x1 and y1, or neither, must be specified")
  } else if(is.null(y1)) {
    if(is.null(deriv)) {
      if(is.null(y)) {stop("y1, or deriv and y, must be provided")}
      if(is.null(x)) {stop("x1, or deriv and x, must be provided")}
    }
    if(length(deriv) < 1 || length(y) < 1 || length(x) < 1) {return(NA)}
    idxs <- which(deriv == max(deriv, na.rm = na.rm))
    if(length(idxs) > 1) {
      warning("multiple timepoints have the maximum derivative, using the first")}
    y1 <- y[idxs[1]]
    x1 <- x[idxs[1]]
  }

  if(!all_same(c(length(y0), length(y1), length(slope), length(x1)))) {
    warning("Only returning the first lag time value")
    y0 <- y0[1]; y1 <- y1[1]; slope <- slope[1]; x1 <- x1[1]
  }
  
  return(solve_linear(x1 = x1, y1 = y1, m = slope, y2 = y0, named = FALSE))
}

# Legacy Code ----

#' Find the first local maxima of a numeric vector
#' 
#' This function has been deprecated in favor of the identical new 
#' function \code{first_maxima}
#' 
#' This function takes a vector of \code{y} values and returns the index
#' (by default) of the first local maxima. It serves as a shortcut
#' for \code{find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]}
#' 
#' @seealso [first_maxima()]
#' 
#' @param y Numeric vector of y values in which to identify local extrema
#' @param x Optional numeric vector of corresponding x values
#' @param return One of c("index", "x", "y"), determining whether the function
#'               will return the index, x value, or y value associated with the
#'               first maxima in y values
#' @param window_width Width of the window (in units of \code{x}) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param window_width_n The maximum number of data points a single 
#'                      extrema-search step is allowed to take. For example,
#'                      when maxima-finding, the function will not pass
#'                      a valley consisting of more than \code{window_width_n}
#'                      data points.
#'                      
#'                      A smaller \code{window_width_n} will be more sensitive 
#'                      to narrow local maxima/minima, while a larger 
#'                      \code{window_width_n} will be less sensitive to 
#'                      narrow local maxima/minima.
#'                      
#'                      If not provided, defaults to ~0.2*length(y)
#' @param window_height The maximum change in \code{y} a single extrema-search
#'                     step is allowed to take.  For example, when 
#'                     maxima-finding, the function will not pass a
#'                     valley deeper than \code{window_height}.
#'                     
#'                     A smaller \code{window_height} will be more sensitive 
#'                     to shallow local maxima/minima, while a larger 
#'                     \code{window_height} will be less sensitive to 
#'                     shallow maxima/minima.
#' @param return_endpoints Should the first or last value in \code{y}
#'                         be allowed to be returned?
#' @param ... Other parameters to pass to \code{find_local_extrema}
#' 
#' @return If \code{return = "index"}, a vector of indices corresponding 
#'           to local extrema in the data
#'           
#'         If \code{return = "x"}, a vector of x values corresponding
#'           to local extrema in the data
#'          
#'         If \code{return = "y"}, a vector of y values corresponding
#'           to local extrema in the data
#' 
#' @details 
#' If none of \code{window_width}, \code{window_width_n}, or 
#' \code{window_height} are provided, default value of \code{window_width_n}
#' will be used.
#' 
#' This function is designed to be compatible for use within
#'  \code{dplyr::group_by} and \code{dplyr::summarize}
#'                    
#' @export    
first_peak <- function(y, x = NULL, 
                       window_width = NULL,
                       window_width_n = NULL,
                       window_height = NULL,
                       return = "index", return_endpoints = TRUE, 
                       ...) {
  #First deprecated in v1.1.0.9000
  .Deprecated("first_maxima")
  return(first_maxima(y = y, x = x, window_width = window_width,
                      window_width_n = window_width_n,
                      window_height = window_height,
                      return = return, return_endpoints = return_endpoints,
                      ...))
}


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
#'              5. a logical for whether this pattern should be filled byrow
#'              
#' @return a tidy-shaped \code{data.frame} containing all the design elements
#' 
#' @export
make_tidydesign <- function(nrows = NULL, ncols = NULL,
                            block_row_names = NULL, block_col_names = NULL,
                            wellnames_sep = "", wellnames_colname = "Well",
                            wellnames_Excel = TRUE, lookup_tbl_start = 1,
                            pattern_split = "", colnames_first = FALSE,
                            ...) {
  #Deprecated in v0.10.2
  .Deprecated("make_design")
  
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
    } else {block_row_names <- paste0("R", 1:nrows)}
  }
  if (is.null(block_col_names)) {
    if (wellnames_Excel) {block_col_names <- 1:ncols
    } else {block_col_names <- paste0("C", 1:ncols)}
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
      if (!canbe.numeric(pattern_list)) {
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
                    names(dot_args)[i], "\n"))
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
#' @export
block_tidydesign <- function(tidydesign, collapse = NULL,
                             wellnames_sep = "_", wellnames_colname = "Well") {
  #Deprecated in v0.10.2
  .Deprecated("make_design")
  
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
