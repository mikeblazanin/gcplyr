
# Read functions ----

#' An internal function that handles name inference logic for other functions
#' 
#' This function takes in the fully-read `data.frame` alongside information
#' about whether rows and columns have been specified, and whether `header`
#' and `sider` have been specified.
#' 
#' @param df The `data.frame`
#' @param startrow,endrow,startcol,endcol The rows & columns specified by user
#' @param header,sider Whether the file contains a header or sider
#'                     (can be `TRUE`, `FALSE`, or `NA`)
#' 
#' @details None of the specified arguments should be a `vector`, they should
#'          all be single values or `NA`'s
#' 
#' @return a `list`: `list(startrow, endrow, startcol, endcol, rownames_col, colnames_row)`
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
#' @param filetype The `vector` of filetypes (often extensions) or `NULL`
#' @param files The `vector` of filenames/paths
#' @param needed_len Parameter to pass to `check_input_dimensions` for
#'                   desired length of output `vector`
#' @param needed_name Parameter to pass to `check_input_dimensions` for
#'                    name of desired length for error message
#' @param ... Other arguments to pass to `check_input_dimensions`, most
#'            frequently `needed_name` for what the desired length 
#'            corresponds to (e.g. number of files)
#' 
#' @return `vector` of filetypes
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
#' @param filestrings The `vector` of file/path `string`s
#' @param keep_dot Should the leading dot and slash be kept and returned
#' @param keep_path Should the path to the file be kept and returned
#' @param keep_ext Should the file extension be kept and returned
#' 
#' @return `vector` of filestrings modified accordingly
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
#' @param na.strings `string`s to be interpreted as `NA`
#' @param sheet What sheet to read (only needed for 'xls' and 'xlsx')
#' 
#' @details None of the specified arguments should be a `vector`, they should
#'          all be single values
#' 
#' @return The `data.frame` resulting from reading the file
#' 
#' @noRd
read_gcfile <- function(file, filetype, na.strings, sheet = NULL, ...) {
  if(any(filetype %in% c("xls", "xlsx"))) {
    if(!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package \"readxl\" must be installed to read xls or xlsx files",
           call. = FALSE)
    }
    if(!requireNamespace("cellranger", quietly = TRUE)) {
      stop("Package \"cellranger\" must be installed to read xls or xlsx files",
           call. = FALSE)
      #readxl imports cellranger, so cellranger should usually be installed
      # if readxl is
    }
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
                     sheet = sheet, na = na.strings,
                     range = cellranger::cell_limits(ul = c(1, 1)),
                     ...)))
  } else if (filetype == "xlsx") {
    suppressMessages(
      readgcfile_temp <- 
        as.data.frame(
          parse_dots(readxl::read_xlsx, path = file, 
                     col_names = FALSE, col_types = "text", 
                     sheet = sheet, na = na.strings,
                     range = cellranger::cell_limits(ul = c(1, 1)),
                     ...)))
  } else {stop("read_gcfile was passed an invalid filetype")}
  return(readgcfile_temp)
}


#' A function that gets metadata from a `row`,`col`. Gives a warning if out of range
#' returns `NA`, otherwise returns the value
#' 
#' @param df The `data.frame` containing metadata at `row`,`col`
#' @param row The row where metadata should be
#' @param col The column where metadata should be
#' 
#' @return The metadata, or `NA` with a warning if out of range
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


#' Read blocks
#' 
#' A function that reads blocks into the R environment
#' 
#' @param files A `vector` of filepaths relative to the current working directory
#'              where each filepath is a single plate read
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#' "csv", "xls", or "xlsx".
#'                  
#' "tbl" or "table" to use [read.table] to read the file,
#'                  "csv2" to use [read.csv2], "delim" to 
#'                  use [read.delim], or "delim2" to use [read.delim2].
#'                  
#' If none provided, `read_blocks` will infer filetype(s) 
#'                  from the extension(s) in `files`. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the measures data are located in `files`.
#'                 
#' Can be a `vector` or `list` the same length as `files`, or
#'                 a single value that applies to all `files`. Values
#'                 can be `numeric` or a `string` that will be automatically
#'                 converted to `numeric` by [from_excel].
#'                 
#' If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param sheet (optional) If data is in .xls or .xlsx files, which sheet it 
#'                 is located on. Defaults to the first sheet if not specified
#' @param metadata (optional) non-density data that should be 
#'                 associated with each read blockmeasures. A named `list` where 
#'                 each item in the `list` is either: a `vector` of length 2, or
#'                 a `list` containing two `vector`s. 
#'                 
#' In the former case, each `vector` should provide the row and 
#'                 column where the metadata is located in all of the
#'                 blockmeasures input files.
#'                 
#' In the latter case, the first `vector` should provide the rows
#'                 where the metadata is located in each of the corresponding
#'                 input files, and the second `vector` should provide the 
#'                 columns where the metadata is located in each of the
#'                 corresponding input files. (This case is typically used 
#'                 when reading multiple blocks from a single file.)
#' @param block_names (optional) `vector` of names corresponding to each plate
#'                 in `files`. If not provided, `block_names` are inferred
#'                 from the filenames
#' @param block_names_header The name of the metadata field containing the
#'                          `block_names`
#' @param block_names_dot If `block_names` are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param block_names_path If `block_names` are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param block_names_ext If `block_names` are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param header   `TRUE`, `FALSE`, or `NA`, or a `vector` of
#'                 such values, indicating whether the file(s) contains the
#'                 column names as its first line. If `header = NA`
#'                 will attempt to infer the presence of column names. If
#'                 `header = FALSE` or no column names are inferred when 
#'                 `header = NA`, column names will be generated
#'                 automatically according to `wellnames_numeric`
#' @param sider    `TRUE`, `FALSE`, or `NA`, or a `vector` of
#'                 such values, indicating whether the file(s) contains the
#'                 row names as its first column. If `sider = NA`
#'                 will attempt to infer the presence of row names. If
#'                 `sider = FALSE` or no row names are inferred when 
#'                 `sider = NA`, row names will be generated
#'                 automatically according to `wellnames_numeric`
#' @param wellnames_numeric If row names and column names are not provided in the
#'                        input `data.frame` as specified by `header`
#'                        and `sider`, then names will be generated
#'                        automatically according to `wellnames_numeric`.
#'                        
#' If `wellnames_numeric` is `TRUE`, rows and columns
#'                        will be numbered with "R" and "C" prefixes, respectively.
#'                        
#' If `wellnames_numeric` is `FALSE`, rows will be
#'                        lettered A through Z, while columns will be numbered
#' @param na.strings A `character` `vector` of `string`s which are to be interpreted
#'                   as `NA` values by [read.csv],
#'                   [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx],
#'                   or [read.table]
#' @param extension Deprecated, use `filetype` instead
#' @param block_name_header Deprecated, use `block_names_header` instead
#' @param ...   Other arguments passed to [read.csv],
#'              [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx],
#'              or [read.table]
#'
#' @details 
#'  For `metadata`, `read_blocks` can handle an arbitrary number of additional
#'  pieces of information to extract from each blockcurve file as metadata.
#'  These pieces of information are specified as a named `list` of `vector`s
#'  where each `vector` is the `c(row, column)` where the information is to be
#'  pulled from in the input files.
#' 
#'  This metadata is returned as the second `list` element of each
#'  blockcurve, e.g.:
#'  
#'   `[[1]]` 
#'   
#'   `[1]` `data` #1 
#'   
#'   `[2]` `metadata`  
#'   
#'   `[2][1]` `name` #1
#'   
#'   `[2][2]` `date-time` #1
#'   
#'   `[2][3]` `temp` #1
#'   
#'   `[[2]]` 
#'   
#'   `[1]` `data` #2 
#'   
#'   `[2]` `metadata`  
#'   
#'   `[2][1]` `name` #2
#'   
#'   `[2][2]` `date-time` #2
#'   
#'   `[2][3]` `temp` #2
#'   
#'   `...`
#' 
#'  Calling [uninterleave] on the output of `read_blocks` works on block data
#'  and the associated metadata because [uninterleave] operates on the highest 
#'  level entries of the `list` (the `[[1]]` `[[2]]` level items), 
#'  leaving the meta-data associated with the block data
#' 
#'  [trans_block_to_wide] integrates this metadata into the
#'  wide-shaped `data.frame` it produces
#' 
#' @return A `list` where each entry is a `list` containing the block `data.frame`
#'         followed by the `block_names` (or filenames, if `block_names` is not 
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
                                 inferred_rc$startcol:inferred_rc$endcol,
                                 drop = FALSE]
    
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
#' `startrow`, `endrow`, `startcol`, `endcol`, `sheet` and `filetype` 
#' can either be a single value that applies for all files or
#' `vector`s or `list`s the same length as `files`, 
#' 
#' @param files A `vector` of filepaths (relative to current working directory)
#'              where each one is a widemeasures set of data
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#' "csv", "xls", or "xlsx".
#'                  
#' "tbl" or "table" to use [read.table] to read the file,
#'                  "csv2" to use [read.csv2], "delim" to 
#'                  use [read.delim], or "delim2" to use [read.delim2].
#'                  
#' If none provided, `read_wides` will infer filetype(s) 
#'                  from the extension(s) in `files`. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the data are located in `files`.
#'                 
#' Can be a `vector` or `list` the same length as `files`, or
#'                 a single value that applies to all `files`. Values
#'                 can be `numeric` or a `string` that will be automatically
#'                 converted to `numeric` by [from_excel].
#'                 
#' If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param header `logical` for whether there is a header in the data. If `FALSE`
#'               columns are simply numbered. If `TRUE`, the first row of the
#'               data (`startrow` if specified) is used 
#'               as the column names
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#'              sheet
#' @param run_names Names to give the widemeasures read in. By default uses the
#'                   file names if not specified
#' @param run_names_header Should the run names (provided in `run_names`
#'                     or inferred from `files`) be added as a column to 
#'                     the widemeasures? If `run_names_header` is `NULL`, 
#'                     they will not be. If `run_names_header` is a `string`, 
#'                     that `string` will be the column header for the column 
#'                     where the names will be stored
#' @param run_names_dot If `run_names` are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param run_names_path If `run_names` are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param run_names_ext If `run_names` are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param metadata (optional) non-spectrophotometric data that should be 
#'                 associated with each read widemeasures. A named `list` where 
#'                 each item in the `list` is either: a `vector` of length 2, or
#'                 a `list` containing two `vector`s. 
#'                 
#' In the former case, each `vector` should provide the row and 
#'                 column where the metadata is located in all of the
#'                 blockmeasures input files.
#'                 
#' In the latter case, the first `vector` should provide the rows
#'                 where the metadata is located in each of the corresponding
#'                 input files, and the second `vector` should provide the 
#'                 columns where the metadata is located in each of the
#'                 corresponding input files. (This case is typically used 
#'                 when reading multiple blocks from a single file.)
#' @param na.strings A `character` `vector` of `string`s which are to be interpreted
#'                   as `NA` values by [read.csv],
#'                   [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx],
#'                   or [read.table]
#' @param extension Deprecated, use `filetype` instead
#' @param names_to_col Deprecated, use `run_names_header` instead
#' @param ...   Other arguments passed to [read.csv],
#'              [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx], or
#'              [read.table]
#'              
#' @return A `data.frame` containing a single widemeasures, or
#'         A `list` of widemeasures named by filename
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
      outputs[[i]] <- rawfile[(startrow[i]+1):endrow[i], 
                              startcol[i]:endcol[i],
                              drop = FALSE]
      colnames(outputs[[i]]) <- rawfile[(startrow[i]), startcol[i]:endcol[i]]
    } else { #so colnames should be numbered
      outputs[[i]] <- rawfile[startrow[i]:endrow[i], 
                              startcol[i]:endcol[i],
                              drop = FALSE]
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
#' wrapper for [read.csv], [read_xls][readxl::read_xls],
#' [read_xls][readxl::read_xls], or [read_xlsx][readxl::read_xlsx], but can handle
#' multiple files at once and has additional options for taking subsets 
#' of rows/columns rather than the entire file and for adding filename 
#' or run names as an added column in the output.
#' 
#' @param files A `vector` of filepaths (relative to current working directory)
#'              where each one is a tidy-shaped data file
#' @param filetype (optional) the type(s) of the files. Options include:
#' 
#' "csv", "xls", or "xlsx".
#'                  
#' "tbl" or "table" to use [read.table] to read the file,
#'                  "csv2" to use [read.csv2], "delim" to 
#'                  use [read.delim], or "delim2" to use [read.delim2].
#'                  
#' If none provided, `read_tidys` will infer filetype(s) 
#'                  from the extension(s) in `files`. When extension is 
#'                  not "csv", "xls", or "xlsx", will use "table".
#' @param startrow,endrow,startcol,endcol (optional) the rows and columns where 
#'                 the data are located in `files`.
#'                 
#' Can be a `vector` or `list` the same length as `files`, or
#'                 a single value that applies to all `files`. Values
#'                 can be `numeric` or a `string` that will be automatically
#'                 converted to `numeric` by [from_excel].
#'                 
#' If not provided, data is presumed to begin on the first
#'                 row and column of the file(s) and end on the last row and
#'                 column of the file(s).
#' @param sheet The sheet of the input files where data is located (if input
#'              files are .xls or .xlsx). If not specified defaults to the first
#' @param run_names Names to give the tidy files read in. By default uses the
#'                  file names if not specified. These names may be added
#'                  to the resulting `data.frame` depending on the value of
#'                  the `names_to_col` argument
#' @param run_names_header Should the run names (provided in `run_names`
#'                     or inferred from `files`) be added as a column to the
#'                     output? 
#'                     
#' If `run_names_header` is `TRUE`, they will be added with
#'                     the column name `run_name`
#'                     
#' If `run_names_header` is `FALSE`, they will not be added.
#'                     
#' If `run_names_header` is a `string`, they will be added
#'                     and the column name will be the `string` specified
#'                     for `run_names_header`.
#'                     
#' If `run_names_header` is `NULL`, they only will be 
#'                     added if there are multiple tidy `data.frame`s being read.
#'                     In which case, the column name will be `run_name`
#' @param run_names_dot If `run_names` are inferred from filenames, should 
#'                        the leading './' (if any) be retained
#' @param run_names_path If `run_names` are inferred from filenames, should 
#'                        the path (if any) be retained
#' @param run_names_ext If `run_names` are inferred from filenames, should 
#'                        the file extension (if any) be retained
#' @param na.strings A `character` `vector` of `string`s which are to be interpreted
#'                   as `NA` values by [read.csv],
#'                   [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx],
#'                   or [read.table]
#' @param extension Deprecated, use `filetype` instead
#' @param names_to_col Deprecated, use `run_names_header` instead
#' @param ...   Other arguments passed to [read.csv],
#'              [read_xls][readxl::read_xls], [read_xlsx][readxl::read_xlsx], or
#'              [read.table]
#'              sheet
#'               
#' @details
#' `startrow`, `endrow`, `startcol`, `endcol`, 
#' `sheet` and `filetype` can either be a single value that 
#' applies for all files or `vector`s or `list`s the same length as `files`
#' 
#' Note that the `startrow` is always assumed to be a header
#' 
#' @return A `data.frame` containing a single tidy `data.frame`, or
#'         A `list` of tidy-shaped `data.frame`s named by filename
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
    outputs[[i]] <- rawfile[(startrow[i]+1):endrow[i], 
                            startcol[i]:endcol[i],
                            drop = FALSE]
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
#' Function to import blockmeasures from files and return widemeasures.
#' This function acts as a wrapper to call [read_blocks], 
#' [uninterleave], then [trans_block_to_wide] in one go.
#' 
#' @param files `vector` of filenames (as `string`s), each of which is a 
#'              block-shaped file containing measures data. File formats
#'              can be .csv, .xls, or .xlsx
#' @param num_plates Number of plates. If multiple plates [uninterleave] will be
#'                   used to separate blockmeasures into those plates accordingly
#' @param plate_names (optional) Names to put onto the plates when output
#' @param wellnames_sep `string` to use as separator for well names between 
#'                      rowname and column name
#' @param ... Other arguments to pass to [read_blocks], [uninterleave],
#'            or [trans_block_to_wide]
#' @details
#' Common arguments that you may want to provide via `...` include:
#' 
#' `startrow`, `endrow`, `startcol`, `endcol`, `sheet` - specifying the 
#'              location of design information inside `files` to [read_blocks]
#'              
#' `metadata` - specifying metadata to [read_blocks]
#'              
#' See [read_blocks] for more details
#'              
#' If you find yourself needing more control, you can run the 
#'              steps manually, first reading with [read_blocks], 
#'              separating plates as needed with [uninterleave], 
#'              then transforming to wide with [trans_block_to_wide].
#'              
#' @return If `num_plates = 1`, a wide-shaped `data.frame`
#'         containing the measures data.
#'         
#' if `num_plates` is greater than one, a `list` of 
#'         `data.frame`'s, where each `data.frame` is wide-shaped.
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
#' This function acts as a wrapper that calls [read_blocks], 
#' [paste_blocks], [trans_block_to_wide], [trans_wide_to_tidy], 
#' and [separate_tidy]
#'
#' @param files A `vector` of filepaths relative to the current working directory
#'              where each filepath is a single plate read to be read by
#'              [read_blocks].
#' @param block_names
#'              `vector` of names corresponding to each design element (each
#'              block). Inferred from filenames, if not specified.
#'              
#' When `keep_blocknames = TRUE`, the output will have
#'              a column containing these values, with the column name
#'              specified by `block_name_header`.
#'              
#' When `join_as_cols = TRUE`, the `block_names` are
#'              also used as the output column names for each separated
#'              design column.
#' @param block_name_header 
#'              When `keep_blocknames = TRUE`, the column name of the
#'              column containing the `block_names`.
#' @param join_as_cols `logical` indicating whether blocks (if there are multiple)
#'              should be joined as columns (i.e. describe the same plate) in 
#'              the tidy output. If `FALSE`, blocks are joined as rows
#'              (i.e. describe different plates) in the tidy output.
#' @param sep   If designs have been pasted together, this specifies the
#'              `string` they should be split apart by via [separate_tidy].
#' @param values_colname When `join_as_cols = FALSE` and `sep` is
#'              not specified, all the design values will be in a column
#'              named by `values_colname`. For other cases, see the
#'              **Value** section.
#' @param into  When `sep` is specified, `into` sets the names
#'              of the columns after splitting (see **Value** section
#'              for behavior when `into` is not set).
#' @param keep_blocknames `logical` indicating whether the column containing
#'              `block_names` (or those inferred from file names) should
#'              be retained in the output. By default, blocknames are retained
#'              only if `join_as_cols = FALSE`.
#' @param warn_joinrows_nointo `logical` indicating whether warning should
#'              be raised when multiple blocks are joined as rows 
#'              (`join_as_cols = FALSE`) and `sep` is specified,
#'              but `into` is not specified.
#' @param join_designs Deprecated, use `join_as_cols` instead            
#' @param ...   Other arguments to pass to [read_blocks], 
#'              [paste_blocks], [trans_block_to_wide],
#'              [trans_wide_to_tidy], or [separate_tidy].
#'              
#' See Details for more information
#'              
#' @details     Other common arguments that you may want to provide via 
#'              `...` include:
#' 
#' `startrow`, `endrow`, `startcol`, `endcol`, 
#'              `sheet` - specifying the location of design information 
#'              inside `files` to [read_blocks].
#'              
#' `wellnames_sep` - specifying what character (or "" for 
#'              none) should be used when pasting together the rownames and
#'              column names. Note that this should be chosen to match
#'              the well names in your measures.
#'              
#' `into` - specifying the column names resulting from
#'              using [separate_tidy] on the `values_colname` column.
#'              
#' Note that `import_blockdesigns` cannot currently handle
#'              metadata specified via the `metadata` argument of
#'              [read_blocks].
#'              
#' If you find yourself needing more control, you can run the 
#'              steps manually, first reading with [read_blocks],
#'              pasting as needed with [paste_blocks], 
#'              transforming to tidy with [trans_block_to_wide] and
#'              [trans_wide_to_tidy], and separating as needed with
#'              [separate_tidy].
#'              
#' @return A tidy-shaped `data.frame` containing the design information
#'         from `files`. This always includes a `Well` column. 
#'         
#' If `keep_blocknames = TRUE`, this includes a column with the
#'         column name specified by `block_name_header` and containing
#'         `block_names` (or block names inferred from file names).
#'         
#' The layout of the design values varies depending on the inputs:
#'         
#' If `join_as_cols = TRUE`, each block was joined as a column,
#'         with the columns named according to `block_names` (or block
#'         names inferred from file names). In this case, if `sep` was 
#'         specified, each column was split by `sep` into columns named by 
#'         splitting the corresponding block name by `sep` (post-split 
#'         column names can alternatively be specified directly via `into`).
#'         
#' Otherwise, when `join_as_cols = FALSE`, each block was joined
#'         as rows, with the column containing all design values named by
#'         `values_colname`. In this case, if `sep` was specified,
#'         that single design column was split by `sep` into columns
#'         named by splitting `values_colname` (post-split column names
#'         can alternatively be specified directly via `into`).
#' 
#' @export
import_blockdesigns <- function(
    files, block_names = NULL, block_name_header = "block_name", 
    join_as_cols = TRUE, 
    sep = NULL, values_colname = "Designs", into = NULL,
    keep_blocknames = !join_as_cols,
    warn_joinrows_nointo = TRUE, join_designs = NULL, ...) {

  if(!missing("join_designs")) {
    warning("join_designs deprecated, use join_as_cols instead")
    join_as_cols <- join_designs
  }
  if(warn_joinrows_nointo && join_as_cols == FALSE && 
     !is.null(sep) && is.null(into)) {
    warning("Separating blocks joined as rows, but 'into' is not specified")
  }
  
  blocks <- parse_dots(read_blocks, 
                       block_names = block_names, files = files, 
                       block_name_header = block_name_header, ...)
  
  if(join_as_cols && length(blocks) > 1) {
    if(is.null(sep)) {sep <- find_char_for_sep(blocks, nested_metadata = TRUE)[1]}
    blocks_pasted <- parse_dots(paste_blocks, blocks = blocks,
                                sep = sep, nested_metadata = TRUE, ...)
  } else {blocks_pasted <- blocks}
  
  wides <- parse_dots(trans_block_to_wide, blocks = blocks_pasted,
                      nested_metadata = TRUE, ...)
  
  #If needed, save block_name_column value to use as col name
  if(join_as_cols) {values_colname <- wides[1, block_name_header]}
  
  #If needed, drop the block name col
  if(!keep_blocknames) {
    wides <- wides[, -which(block_name_header == colnames(wides))]}
  
  #Transform to tidy
  tidys <- parse_dots(
    trans_wide_to_tidy, 
    wides = wides, 
    data_cols = colnames(wides)[colnames(wides) != block_name_header], 
    values_to = values_colname, values_to_numeric = FALSE,
    ...)
  
  if(!is.null(sep)) {
    tidy_sep <- parse_dots(separate_tidy, data = tidys, sep = sep, 
                           col = values_colname, into = into, ...)
  } else {tidy_sep <- tidys}
  
  return(tidy_sep)
}


# Design functions ----

#' Make design `data.frame`(s)
#' 
#' This is a function to easily input experimental design elements
#' for later merging with read data
#' 
#' @details 
#' Note that either `nrows` or `block_row_names` must be provided
#' and that either `ncols` or `block_col_names` must be provided
#' 
#' @param nrows,ncols Number of rows and columns in the plate data
#' @param block_row_names,block_col_names Names of the rows, columns
#'                                     of the plate blockmeasures data
#' @param block_name_header The name of the field containing the
#'                          `block_names`
#' @param output_format One of `c("blocks", "blocks_pasted", "wide", "tidy")`
#'                      denoting the format of the resulting `data.frame`
#'                      
#' For easy merging with tidymeasures, leave as default
#'                      of 'tidy'. 
#'                      
#' For human-readability to confirm design
#'                      is correct, choose 'blocks' or 'blocks_pasted'. 
#'                      
#' For writing to block-shaped file(s), choose 'blocks' or
#'                      'blocks_pasted'.
#' @param wellnames_numeric If `block_row_names` or `block_col_names`
#'                        are not specified, then names will be generated
#'                        automatically according to `wellnames_numeric`.
#'                        
#' If `wellnames_numeric` is `TRUE`, rows and columns
#'                        will be numbered with "R" and "C" prefixes, respectively.
#'                        
#' If `wellnames_numeric` is `FALSE`, rows will be
#'                        lettered A through Z, while columns will be numbered
#' @param wellnames_sep A `string` used when concatenating rownames and column
#'                      names to create well names, when 
#'                      `output_format = "wide"` or 
#'                      `output_format = "tidy"`
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names, when `output_format = "tidy"`
#' @param colnames_first  When wellnames are created for 
#'                        `output_format = "wide"` or 
#'                        `output_format = "tidy"` by [paste]-ing the
#'                        rownames and column names, should the column names
#'                        come first. 
#' @param lookup_tbl_start Value in the lookup table for the split pattern values
#'                         that corresponds to the first value in the `vector`.
#'                         
#' Lookup table by default is 
#'                         `c(1,2,...,8,9,A,B,...Y,Z,a,b,...,y,z)`. If,
#'                         for example, `lookup_tbl_start = "A"`, then the lookup
#'                         table will now be `c(A,B,...Y,Z,a,b,...,y,z)`
#' @param pattern_split character to split pattern elements provided in
#'                      `...` by, if they're not already a `vector`
#' @param ... Each `...` argument must be named, and must be a `list` with 
#'            five elements:
#' 
#' 1. a `vector` of the values
#'              
#' 2. a `vector` of the rows the pattern should be applied to
#'              
#' 3. a `vector` of the columns the pattern should be applied to
#'              
#' 4. a `string` or `vector` denoting the pattern in which the
#'                 values should be filled into the rows and columns specified.
#'                 
#'    If it's a `string`, will be split by `pattern_split`.
#'                 Pattern will be used as the indices of the values `vector`.
#'               
#'    0's refer to `NA`. The pattern will be recycled as necessary
#'                 to fill all the wells of the rows and columns specified.
#'               
#' 5. a `logical` for whether this pattern should be filled byrow
#'
#' @return Depends on `output_format`:
#' 
#' If `output_format = "blocks"`, a `list` of `data.frame`'s
#'         where each `data.frame` is block-shaped containing the
#'         information for a single design element
#'         
#' If `output_format = "blocks_pasted"`, a single 
#'         `data.frame` containing the [paste]-ed information
#'         for all design elements
#'         
#' If `output_format = "wide"`, a wide-shaped `data.frame`
#'         containing all the design elements
#'         
#' If `output_format = "tidy"`, a tidy-shaped `data.frame`
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
      sep <- find_char_for_sep(output, nested_metadata = TRUE)[1]
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
#' A helper function for use with [make_design]
#' 
#' @param values `vector` of values to use
#' @param rows `vector` of rows where pattern applies
#' @param cols `vector` of cols where pattern applies
#' @param pattern `numeric` pattern itself, where numbers refer to entries
#'                in `values`
#' @param byrow `logical` for whether pattern should be created by row
#' 
#' @return `list(values, rows, cols, pattern, byrow)`
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
#' @seealso [make_design]
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

#' Fill `output` `data.frame` with `data` and `metadata`
#'
#' This is an internal function used by [write_blocks] that does the 
#' fill data and metadata step
#' 
#' @param output `data.frame` for data and metadata to be filled into
#' @param input One data and metadata to fill into `output`
#' @param rs index of the row in `output` where data and metadata should
#'           start being filled in
#' @param metadata_include Indices of which metadata items should be included
#'                         
#' Use `NULL` for no metadata inclusion
#'                         
#' @return A `list`, where `[[1]]` is `output` returned containing the data and
#'         metadata from `input` but properly formatted for writing.
#'         
#' And `[[2]]` is the updated `rs` to be used for subsequent
#'         calls to `fill_data_metadata`
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
#' This function writes block-shaped `list`s (as created by
#' [read_blocks] or [make_design]) to csv files, including
#' both `data` and `metadata` in a variety of output formats
#' 
#' 
#' @param blocks `list` of block-shaped data to be written to file
#' @param file `NULL`, a `character` `string` naming a file to write to, or a 
#'             `vector` of `character` `string`s naming files to write to. 
#' 
#' A file name is required when `output_format = "single"`
#'             
#' A file name can be specified when `output_format = "pasted"`,
#'             or `file` can be set to `NULL` as long as 
#'             `block_name_location = "filename"` (where pasted 
#'             `block_name` metadata will be used for the file name)
#'             
#' File names can be specified when `output_format = "multiple"`,
#'             or `file` can be set to `NULL` as long as 
#'             `block_name_location = "filename"` (where the 
#'             `block_name` metadata will be used for the file names)
#'             
#' @param output_format One of "single", "pasted", "multiple".
#' 
#' "single" will write all blocks into a single
#'                      csv file, with an empty row between successive
#'                      blocks.
#'                      
#' "pasted" will paste all blocks together using a
#'                      `paste_sep`, and then write that now-pasted
#'                      block to a single csv file.
#'                      
#' "multiple" will write each block to its own csv file.
#' @param block_name_location Either `NULL`, 'filename' or 'file'.
#' 
#' If `NULL`, `block_name_location` will 
#'                           be automatically selected based on 
#'                           `output_format`. For 
#'                           `output_format = 'single'` and 
#'                           `output_format = 'pasted'`, 
#'                           `block_name_location` defaults to 'file'.
#'                           For `output_format = 'multiple'`, 
#'                           `block_name_location` defaults to 'filename'
#' 
#' If 'filename', the `block_name` metadata 
#'                           will be used as the output file name(s) when
#'                           no file name(s) are provided, or appended to
#'                           file name(s) when they have been provided.
#'                           
#' If 'file', the `block_name` metadata will be
#'                           included as a row in the output file.
#' @param block_name_header The name of the field containing the
#'                          `block_names`
#' @param paste_sep When `output_format = 'pasted'`, what character
#'                  will be used to paste together blocks.   
#' @param filename_sep What character will be used to paste together 
#'                     filenames when `block_name_location = 'filename'`.   
#' @param na The `string` to use for missing values in the data.
#' @param dir The directory that file(s) will be written into. When 
#'            `dir = NULL`, writes to the current working directory.
#'            (Can only be used when `file = NULL`)
#' @param ... Other arguments passed to [write.table]
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
#' @param blocks Blocks, either a single `data.frame` or a `list` of
#'                      `data.frame`s
#' @param wellnames_sep `string` to use as separator for well names between 
#'                      row name and column name (ordered according to
#'                      `colnames_first`
#' @param nested_metadata A `logical` indicating the existence of nested metadata
#'                        in the blockmeasures `list`, e.g. as is typically
#'                        output by [read_blocks]. If `NULL`, will 
#'                        attempt to infer existence of nested metadata
#' @param colnames_first  In the wellnames created by [paste]-ing the
#'                        rownames and column names, should the column names
#'                        come first?
#'
#' @return A single wide-shaped `data.frame`
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

#' Pivot wide-shaped into tidy
#' 
#' Essentially a wrapper for [pivot_longer][tidyr::pivot_longer] that works on both a 
#' single wide-shaped `data.frame` as well as a `list` of wide-shaped `data.frame`'s
#' 
#' @param wides A single wide-shaped `data.frame`, or a `list` of wide-shaped
#'                   `data.frame`'s
#' @param data_cols,id_cols Specifies which columns have data vs are ID's
#'                          (in [pivot_longer][tidyr::pivot_longer] parlance). Each can be
#'                          a single `vector` (which will be applied for all
#'                          `data.frame`s) or a `list` of `vector`s, with each
#'                          `vector` corresponding to the same-index `data.frame`
#'                          in `wides`
#'                          
#' Entries that are `NA` in the `list` will not be used
#'                          
#' If neither `data_cols` nor `id_cols` are 
#'                          specified, user must provide arguments to 
#'                          [pivot_longer][tidyr::pivot_longer] via `...` for at 
#'                          least the `cols` argument and these arguments 
#'                          provided via `...` will be used for all 
#'                          `wides` `data.frame`'s
#' @param names_to,values_to Specifies the output column names created by
#'                           [pivot_longer][tidyr::pivot_longer]. Each can be provided 
#'                           as `vector`s the same length as `wides`
#' @param values_to_numeric `logical` indicating whether values will be coerced
#'                          to `numeric`. See below for when this may be
#'                          overridden by arguments passed in `...`
#' @param ... Other arguments to be passed to [pivot_longer][tidyr::pivot_longer]
#'            Note that including `values_transform` here will override the
#'            behavior of `values_to_numeric`
#' @return Pivoted longer `data.frame` (if `wides` is a single `data.frame`)
#'         or `list` of pivoted longer `data.frame`s (if `wides` is
#'         a `list` of `data.frame`s)
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

#' Collapse a `list` of `data.frame`s, or merge two `data.frame`s together
#' 
#' This function is essentially a wrapper for any of `dplyr`'s
#' [mutate-joins][dplyr::mutate-joins] (by default, a [full_join][dplyr::full_join]). 
#' The most typical use of this function is to merge designs with measures 
#' data, or to use the `collapse` functionality to merge a `list` of `data.frame`s 
#' into a single `data.frame`. Merging is done by column names that match 
#' between `x` and `y`.
#'  
#' @param x First `data.frame`, or `list` of `data.frame`s, to be joined
#' @param y Second `data.frame`, or `list` of `data.frame`s, to be joined
#' @param by A `character` `vector` of variables to join by, passed directly
#'           to the join function
#' @param drop Should only `complete_cases` of the resulting
#'             `data.frame` be returned?
#' @param collapse A `logical` indicating whether `x` or `y` is a `list` containing
#'                 `data.frame`s that should be merged together before
#'                 being merged with the other
#' @param names_to Column name for where `names(x)` or `names(y)` 
#'                 will be entered in if `collapse = TRUE`.
#'                 
#' If a value of `NA` then `names(x)` or 
#'                 `names(y)` will not be put into a column in the
#'                 returned `data.frame`
#' @param join     Type of join used to merge `x` and `y`. Options
#'                 are 'full' (default), 'inner', 'left', and 'right'.
#'                 
#'                 
#' - A `full` join keeps all observations in `x` and 
#'   `y`
#' - A `left` join keeps all observations in `x`
#' - A `right` join keeps all observations in `y`
#' - An `inner` join only keeps observations found in
#'   both `x` and `y` (inner joins are not appropriate
#'   in most cases because observations are frequently dropped).
#'                 
#'                 
#' See [full_join][dplyr::full_join], [left_join][dplyr::left_join], 
#'                 [right_join][dplyr::right_join], or [inner_join][dplyr::inner_join] for 
#'                 more details
#' @param warn_morerows `logical`, should a warning be passed when the output
#'                      has more rows than `x` and more rows than `y`?
#' @param ... Other arguments to pass to the underlying join function. See 
#'            [full_join][dplyr::full_join], [left_join][dplyr::left_join], 
#'            [right_join][dplyr::right_join], or [inner_join][dplyr::inner_join] for options.
#' 
#' @return `data.frame` containing merged output of `x` and
#'         `y`
#' 
#' @export
merge_dfs <- function(x, y = NULL, by = NULL, drop = FALSE,
                      collapse = FALSE, names_to = NA,
                      join = "full", warn_morerows = TRUE, ...) {
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
      if(warn_morerows && nrow(output) > nrow(x) && nrow(output) > nrow(y)) {
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

#' Paste a `list` of blocks into a single block
#' 
#' This function uses [paste] to concatenate the same-location entries
#' of a `list` of `data.frame`s together (i.e. all the first row-first column
#' values are pasted together, all the second row-first column values are
#' pasted together, etc.)
#' 
#' @param blocks Blocks, either a single `data.frame` or a `list` of
#'                      `data.frame`s
#' @param sep `string` to use as separator for output pasted values
#' @param nested_metadata A `logical` indicating the existence of nested metadata
#'                        in the blockmeasures `list`, e.g. as is typically
#'                        output by [read_blocks]. If `NULL`, will attempt to
#'                        infer existence of nested metadata
#' 
#' @return If `nested_metadata = TRUE` (or is inferred to be `TRUE`), a `list`
#'         containing a `list` containing: 1. a `data.frame` with the
#'         pasted data values from `blocks`, and 2. a `vector` with the 
#'         pasted metadata values from `blocks`
#'         
#' If `nested_metadata = FALSE` (or is inferred to be `FALSE`), a `list`
#'         containing `data.frame`'s with the pasted values from
#'         `blocks`
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
#' This function is primarily a wrapper for [separate][tidyr::separate], which
#' turns a single `character` column into multiple columns
#' 
#' @param data A `data.frame`
#' @param col  Column name or position
#' @param into A `character` `vector` of the new column names. Use `NA` to
#'             omit the variable in the output.
#'             
#' If `NULL`, `separate_tidy` will attempt to infer the new
#'             column names by splitting the column name of `col`
#' @param sep Separator between columns passed to [separate][tidyr::separate]:
#' 
#' If `character`, `sep` is interpreted as a regular expression.
#'            
#' If `numeric`, `sep` is interpreted as character positions
#'            to split at. Positive values start at 1 at the far-left of the
#'            `string`; negative values start at -1 at the far-right of the
#'            `string`. The length of `sep` should be one less than 
#'            `into`
#' @param coerce_NA  `logical` dictating if `string`s matching any of 
#'                   `na.strings` will be coerced into  `NA` values 
#'                   after separating.
#' @param na.strings A `character` `vector` of `string`s which are to be interpreted
#'                   as `NA` values if `coerce_NA == TRUE`
#' @param message_inferred_into `logical` whether column names for `into`
#'                              should be printed in a message when inferred
#' @param ... Other arguments passed to [separate][tidyr::separate]
#' 
#' @return A `data.frame` containing new columns in the place of `col`
#' 
#' @export
separate_tidy <- function(data, col, into = NULL, sep = "_",
                          coerce_NA = TRUE, na.strings = "NA",
                          message_inferred_into = TRUE, ...) {
  if(is.null(into)) {
    if(col %in% colnames(data)) {
      into <- strsplit(col, split = sep)[[1]]
    } else if (is.numeric(col)) {
      into <- strsplit(colnames(data)[col], split = sep)[[1]]
    } else {
      stop("into is NULL, but col is neither numeric nor a column name in data")
    }
    if(message_inferred_into) {
      message(paste("Inferred 'into' column names as:", paste(into, collapse = ", ")))
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
#' @param ... Arguments passed to [loess], [gam][mgcv::gam],
#'            [moving_average], [moving_median], or 
#'            [smooth.spline]. Typically includes tuning 
#'            parameter(s), which in some cases are required.
#'            See Details for more information.
#' @param x An (often optional) `vector` of predictor values to smooth along 
#'          (e.g. time)
#' @param y A `vector` of response values to be smoothed (e.g. density). If `NULL`,
#'          `formula` and `data` *must* be provided via `...`
#' @param sm_method Argument specifying which smoothing method should
#'                  be used to smooth data. Options include 
#'                  "moving-average", "moving-median", "loess", "gam",
#'                  and "smooth.spline".
#' @param subset_by An optional `vector` as long as `y`. 
#'                  `y` will be split by the unique values of this `vector` 
#'                  and the smoothed data for each group will be calculated 
#'                  independently of the others.
#'                  
#' This provides an internally-implemented approach similar
#'                  to [group_by][dplyr::group_by] and [mutate][dplyr::mutate]
#' @param return_fitobject `logical` whether entire object returned
#'                         by fitting function should be returned. If `FALSE`,
#'                         just fitted values are returned.
#' @param warn_ungrouped `logical` whether warning should be issued when
#'                       `smooth_data` is being called on ungrouped data
#'                       and `subset_by = NULL`.
#' @param warn_gam_no_s `logical` whether warning should be issued when [gam][mgcv::gam] is 
#'                      used without [s][mgcv::s] in the formula.
#'
#' @details 
#' For [moving_average] and [moving_median], 
#'            passing `window_width` or `window_width_n` via 
#'            `...` is required. `window_width` sets the width
#'            of the moving window in units of `x`, while 
#'            `window_width_n` sets the width in units of number
#'            of data points. Larger values for either will produce more 
#'            "smoothed" data.
#'            
#' For [loess], the `span` argument sets the fraction of
#'            data points that should be included in each calculation. It's
#'            typically best to specify, since the default of 0.75 is often
#'            too large for growth curves data. Larger values of `span` 
#'            will produce more "smoothed" data
#'            
#' For [gam][mgcv::gam], both arguments to [gam][mgcv::gam] and 
#'            [s][mgcv::s] can be provided via `...`. Most frequently, 
#'            the `k` argument to [s][mgcv::s] sets the number of 
#'            "knots" the spline-fitting can use. Smaller values of `k` will 
#'            be more "smoothed".
#'            
#' When using `sm_method = "gam"`, advanced users may also modify 
#'            other parameters of [s][mgcv::s], including the smoothing basis 
#'            `bs`. These bases can be thin plate (`bs = "tp"`, 
#'            the default), cubic regressions (`bs = "cr"`), or many other 
#'            options (see [s][mgcv::s]). I recommend leaving the default 
#'            thin plate regressions, whose main drawback is that they are 
#'            computationally intensive to calculate. For growth curves data, 
#'            this is unlikely to be relevant.
#'            
#' As an alternative to passing `y`, for more advanced needs 
#'            with [loess] or [gam][mgcv::gam], `formula` and `data` 
#'            can be passed to `smooth_data` via the `...` argument 
#'            (in lieu of `y`).
#'          
#' In this case, the formula should specify the response (e.g. density) 
#'            and predictors. For [gam][mgcv::gam] smoothing, the formula should
#'            typically be of the format: `y ~ s(x)`, which uses 
#'            [s][mgcv::s] to smooth the data. The data argument should be a 
#'            `data.frame` containing the variables in the formula.
#'            In such cases, `subset_by` can still be specified as a `vector`
#'            with length `nrow(data)`
#' 
#' @return If `return_fitobject == FALSE:`
#' 
#' A `vector`, the same length as `y`, with the now-smoothed `y` values
#'         
#' If `return_fitobject == TRUE:`
#'         
#' A `list` the same length as `unique(subset_by)` where each element is
#'         an object of the same class as returned by the smoothing method
#'         (typically a named `list`-like object)
#' 
#' @export
smooth_data <- function(..., x = NULL, y = NULL, sm_method, subset_by = NULL,
                        return_fitobject = FALSE, warn_ungrouped = TRUE,
                        warn_gam_no_s = TRUE) {
  if(!sm_method %in% c("moving-average", "moving-median", "gam", 
                       "loess", "smooth.spline")) {
    stop("sm_method must be one of: moving-average, moving-median, gam, loess, or smooth.spline")
  }
  
  if(warn_ungrouped) {
    check_grouped(name_for_error = "smooth_data", subset_by = subset_by)}
  
  if (sm_method == "gam" && !requireNamespace("mgcv", quietly = TRUE)) {
      stop("Package \"mgcv\" must be installed to use gam", call. = FALSE)}
  
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
  
  if (sm_method == "gam" && warn_gam_no_s && 
      substr(as.character(formula[3]), 1, 2) != "s(") {
      warning("gam method is called without 's()' to smooth\n")
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
    return_NA <- FALSE #reset placeholder when too many NA vals for gam, loess
    
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
    } else if (sm_method %in% c("loess", "gam")) {
      parsed <- 
        parse_formula_data(formula = formula, 
                           data = data[subset_by == unique(subset_by)[i], ])
      parsed_noNA <- rm_nas(x = parsed[["x"]], y = parsed[["y"]], na.rm = TRUE)
      if(length(parsed_noNA[["x"]]) < 2) {return_NA <- TRUE
      } else {
        if(sm_method == "loess") {
          smoothed_object <- 
            stats::loess(formula = formula, 
                         data = data[subset_by == unique(subset_by)[i], ],
                         na.action = "na.exclude", ...)
        } else if (sm_method == "gam") {
          smoothed_object <- 
            parse_dots(
              FUN = mgcv::gam,
              formula = formula, 
              data = data[subset_by == unique(subset_by)[i], ],
              na.action = "na.exclude", ...)
        }
      }
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
      if(return_NA) {fits_list[[i]] <- list(NA)
      } else {fits_list[[i]] <- smoothed_object}
    } else {
      #Fill in output if needed
      if(sm_method %in% c("gam", "loess")) {
        if(return_NA) {
          out[subset_by == unique(subset_by)[i]] <- as.numeric(NA)
        } else {
          out[subset_by == unique(subset_by)[i]] <-
            stats::predict(smoothed_object, 
                           newdata = data[subset_by == unique(subset_by)[i], ])
        }
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

#' Do all setup steps for moving window smoothing function
#' 
#' @param formula Formula specifying the `numeric` response (density) 
#'                and `numeric` predictor (time).
#' @param data `data.frame` containing variables in `formula`
#' @param x A `vector` of predictor values to smooth along (e.g. time)
#' @param y A `vector` of response values to be smoothed (e.g. density).
#' @param window_width_n Number of data points wide the moving window is
#'                     (therefore, must be an odd number of points)
#' @param window_width Width of the moving window (in units of `x`)
#' @param window_width_n_frac Width of the window (as a fraction of the total
#'                          number of data points).
#' @param window_width_frac Width of the window (as a fraction of the range of
#'                          `x`)
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing
#' @param warn_nonnumeric_sort `logical` whether warning should be issued when 
#'                             predictor variable that data is sorted by is 
#'                             non-`numeric`.
#'                             
#' @return `list` containing: `x` values, `y` values,
#'                          indices for original order, 
#'                          indices where `NA`'s were removed,
#'                          `window_width_n`, 
#'                          `window_width`, 
#'                          `window_width_n_frac`,
#'                          `window_width_frac`,
#' 
#' @noRd
setup_moving_smooth <- function(formula, data, x, y,
                             window_width_n, 
                             window_width, 
                             window_width_n_frac,
                             window_width_frac,
                             na.rm, warn_nonnumeric_sort) {

  window_width_n <- NA_to_NULL(window_width_n)
  window_width <- NA_to_NULL(window_width)
  window_width_n_frac <- NA_to_NULL(window_width_n_frac)
  window_width_frac <- NA_to_NULL(window_width_frac)
  
  if(is.null(window_width) && is.null(window_width_n) && 
     is.null(window_width_frac) && is.null(window_width_n_frac)) {
    stop("window_width, window_width_n, window_width_frac, or window_width_n_frac\nmust be provided")}
  
  if((is.null(formula) || is.null(data)) && (is.null(x) || is.null(y))) {
    stop("formula and data, or x and y, must be provided")}
  
  if(!is.null(formula) && !is.null(data)) {
    temp_parsed_formula_data <- parse_formula_data(formula, data)
    x <- temp_parsed_formula_data[["x"]]
    y <- temp_parsed_formula_data[["y"]]
  }
  
  #Check x for being correct format
  if(!is.numeric(x)) {
    if(canbe.numeric(x)) {x <- as.numeric(x) #it can be coerced
    } else if(warn_nonnumeric_sort) {
      if(!is.null(formula) && !is.null(data)) {
        warning(paste0("data is being sorted by order(", 
                       temp_parsed_formula_data[["predictor"]], "), but ", 
                       temp_parsed_formula_data[["predictor"]], 
                       " is not numeric\n"))
      } else {
        warning("data is being sorted by order(x), but x is not numeric")
      }
    }
  }
  #Check y for being the correct format
  y <- make.numeric(y, "y")
  
  #remove nas
  narm_temp <- rm_nas(x = x, y = y, na.rm = na.rm, stopifNA = TRUE)
  
  #Reorder data
  order_temp <- reorder_xy(x = narm_temp[["x"]], y = narm_temp[["y"]])
  
  #Make temp vectors of x and y
  x <- order_temp[["x"]]
  y <- order_temp[["y"]]
  
  if(!is.null(window_width_n) && length(x) != length(unique(x))) {
    warning("not all x values are unique, window_width_n should not be used")}
  if(!is.null(window_width_n_frac) && length(x) != length(unique(x))) {
    warning("not all x values are unique, window_width_n_frac should not be used")}
  
  return(list(x = x, y = y,
              order = order_temp[["order"]],
              nas_indices_removed = narm_temp[["nas_indices_removed"]],
              window_width_n = window_width_n, 
              window_width = window_width, 
              window_width_n_frac = window_width_n_frac,
              window_width_frac = window_width_frac))
}

#' Moving window smoothing
#' 
#' These functions use a moving window to smooth data
#' 
#' @param formula Formula specifying the `numeric` response (density) 
#'                and `numeric` predictor (time).
#' @param data `data.frame` containing variables in `formula`
#' @param x A `vector` of predictor values to smooth along (e.g. time)
#' @param y A `vector` of response values to be smoothed (e.g. density).
#' @param window_width_n Number of data points wide the moving window is
#'                     (therefore, must be an odd number of points)
#' @param window_width Width of the moving window (in units of `x`)
#' @param window_width_n_frac Width of the window (as a fraction of the total
#'                          number of data points).
#' @param window_width_frac Width of the window (as a fraction of the range of
#'                          `x`)
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing
#' @param warn_nonnumeric_sort `logical` whether warning should be issued when 
#'                             predictor variable that data is sorted by is 
#'                             non-`numeric`.
#' 
#' @details Either `x` and `y` or `formula` and `data`
#'          must be provided.
#'          
#' Values of `NULL` or `NA` will be ignored for any of
#'          `window_width_n`, `window_width`,
#'          `window_width_n_frac`, or `window_width_frac`
#'          
#' @return `vector` of smoothed data, with `NA`'s appended at both ends
#' 
#' @name MovingWindowFunctions
NULL

#' @rdname MovingWindowFunctions
#' @export
moving_average <- function(formula = NULL, data = NULL, x = NULL, y = NULL,
                           window_width_n = NULL, 
                           window_width = NULL, 
                           window_width_n_frac = NULL,
                           window_width_frac = NULL,
                           na.rm = TRUE, warn_nonnumeric_sort = TRUE) {

  #Run all setup steps and checks
  setup <- setup_moving_smooth(formula = formula, data = data, x = x, y = y,
                            window_width_n = window_width_n,
                            window_width = window_width,
                            window_width_n_frac = window_width_n_frac,
                            window_width_frac = window_width_frac,
                            na.rm = na.rm, 
                            warn_nonnumeric_sort = warn_nonnumeric_sort)
  
  if(length(setup[["x"]]) == 0) { #All values were removed NA
    return(rep(as.numeric(NA), length(setup[["nas_indices_removed"]])))
  } else {
    #Get windows
    windows <- get_windows(x = setup[["x"]], y = setup[["y"]], 
                           window_width_n = setup[["window_width_n"]],
                           window_width = setup[["window_width"]],
                           window_width_n_frac = setup[["window_width_n_frac"]], 
                           window_width_frac = setup[["window_width_frac"]],
                           edge_NA = TRUE)
    
    #Calculate average
    results <- sapply(windows, y = setup[["y"]], 
                      FUN = function(x, y) {mean(y[x])})
    #Put back in original order
    results <- results[order(setup[["order"]])]
  }
  
  #Add NA's
  results <- add_nas(x = results, 
                     nas_indices_removed = setup[["nas_indices_removed"]])[["x"]]
  
  return(results)
}

#' @rdname MovingWindowFunctions
#' @export 
moving_median <- function(formula = NULL, data = NULL, x = NULL, y = NULL,
                          window_width_n = NULL, 
                          window_width = NULL, 
                          window_width_n_frac = NULL,
                          window_width_frac = NULL,
                          na.rm = TRUE, warn_nonnumeric_sort = TRUE) {
  
  #Run all setup steps and checks
  setup <- setup_moving_smooth(formula = formula, data = data, x = x, y = y,
                            window_width_n = window_width_n,
                            window_width = window_width,
                            window_width_n_frac = window_width_n_frac,
                            window_width_frac = window_width_frac,
                            na.rm = na.rm, 
                            warn_nonnumeric_sort = warn_nonnumeric_sort)
  
  if(length(setup[["x"]]) == 0) { #All values were removed NA
    return(rep(as.numeric(NA), length(setup[["nas_indices_removed"]])))
  } else {
    #Get windows
    windows <- get_windows(x = setup[["x"]], y = setup[["y"]], 
                           window_width_n = setup[["window_width_n"]],
                           window_width = setup[["window_width"]],
                           window_width_n_frac = setup[["window_width_n_frac"]], 
                           window_width_frac = setup[["window_width_frac"]],
                           edge_NA = TRUE)
    
    #Calculate median
    results <- sapply(windows, y = setup[["y"]], 
                      FUN = function(x, y) {stats::median(y[x])})
    #Put back in original order
    results <- results[order(setup[["order"]])]
  }
  
  #Add NA's
  results <- 
    add_nas(x = results, 
            nas_indices_removed = setup[["nas_indices_removed"]])[["x"]]
  
  return(results)
}

#' Predict data by linear interpolation from existing data
#' 
#' @param x A `vector` of known predictor values.
#' @param y A `vector` of known response values.
#' @param newdata A `vector` of new predictor values for which the response
#'                value will be predicted
#' @param extrapolate_predictions 
#'                Boolean indicating whether values of `newdata` that are 
#'                out of the domain of `x` should be predicted (by 
#'                extrapolating the slope from the endpoints of `x`). If
#'                `FALSE`, such values will be returned as `NA`.
#' @param na.rm `logical` whether `NA`'s should be removed before making predictions
#'
#' @return A `vector` of response values for each predictor value in 
#'         `newdata`
#'         
#' @export
predict_interpolation <- function(
    x, y, newdata, extrapolate_predictions = TRUE, na.rm = TRUE) {
  x <- make.numeric(x, "x")
  y <- make.numeric(y, "y")
  newdata <- make.numeric(newdata, "newdata")
  
  nas_removed <- rm_nas(x = x, y = y, na.rm = na.rm)
  
  if(length(nas_removed[["x"]]) == 0) {return(rep(NA), length(newdata))}
  
  dat <- data.frame(x = nas_removed[["x"]], y = nas_removed[["y"]])
  
  #If any newdata are out of known domain, project endpoint slopes
  # out to needed newdata points
  if(any(!is.na(newdata) & newdata < min(dat$x))) {
    if(!extrapolate_predictions) {newdata[newdata < min(x)] <- NA
    } else {dat <- 
      rbind(data.frame(x = min(newdata),
                       y = solve_linear(x1 = dat$x[1], x2 = dat$x[2],
                                        y1 = dat$y[1], y2 = dat$y[2],
                                        x3 = min(newdata))),
            dat)
    }
  }
  if(any(!is.na(newdata) & newdata > max(dat$x))) {
    if(!extrapolate_predictions) {newdata[newdata > max(x)] <- NA
    } else {dat <- 
      rbind(dat,
            data.frame(x = max(newdata),
                       y = solve_linear(x1 = dat$x[nrow(dat)], 
                                        x2 = dat$x[nrow(dat)-1],
                                        y1 = dat$y[nrow(dat)], 
                                        y2 = dat$y[nrow(dat)-1],
                                        x3 = max(newdata))))
    }
  }
  
  nas_removed <- rm_nas(newdata = newdata, na.rm = TRUE)
  
  if(length(nas_removed[["newdata"]]) == 0) {
    return(rep(as.numeric(NA), length(newdata)))
  } else {
    newdata <- nas_removed[["newdata"]]
  
    out <- solve_linear(
      x1 = sapply(newdata, 
                  function(newdata, x) {x[max(which(x <= newdata))]}, x = dat$x),
      x2 = sapply(newdata, 
                  function(newdata, x) {x[min(which(x >= newdata))]}, x = dat$x),
      y1 = sapply(newdata, function(newdata, x, y) {y[max(which(x <= newdata))]}, 
                  x = dat$x, y = dat$y),
      y2 = sapply(newdata, function(newdata, x, y) {y[min(which(x >= newdata))]},
                  x = dat$x, y = dat$y),
      x3 = newdata,
      named = FALSE)
    out <- 
      add_nas(out,
              nas_indices_removed = nas_removed[["nas_indices_removed"]])[["x"]]
    
    return(out)
  }
}


#' Fit a Smoothing Spline
#' 
#' This function is a wrapper for [smooth.spline], which fits 
#' a cubic smoothing spline to the supplied data, but includes the option
#' to remove `NA` values, and returns values in the original order.
#' 
#' @param x A `vector` giving the values of the predictor variable.
#' @param y A `vector` giving the values of the response variable. If `y` is
#'          missing or `NULL`, the responses are assumed to be specified
#'          by `x`, with `x` the index `vector`.
#' @param ... Additional arguments passed to [smooth.spline].
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing.
#'              Required to be `TRUE` if any `x` or `y` values are `NA`.
#'              
#' @details See [smooth.spline]              
#' 
#' @return Similar to [smooth.spline], an object of class 
#'         "`smooth.spline`" with many components. Differs in that
#'         `x`, `y`, and `w` have `NA`'s at any indices where `x` or `y` were 
#'         `NA` in the inputs, and `x`, `y`, and `w` are returned to match the input 
#'         `x` in order and length
#' 
#' @export   
gc_smooth.spline <- function(x, y = NULL, ..., na.rm = TRUE) {
  #remove NAs
  xy_nas_removed <- rm_nas(x = x, y = y, 
                           na.rm = na.rm, stopifNA = TRUE)
  if(length(xy_nas_removed[["y"]]) <= 1) {
    return(list(x = rep(as.numeric(NA), length(x)), 
                y = rep(as.numeric(NA), length(x))))}
  
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


# Processing: Cross-validation ----

#' Create method argument for train of growth curve smoothers
#' 
#' This function generates a `list` which is compatible to be used as the
#' `method` argument to [train][caret::train]. This enables users to
#' call [train][caret::train] directly themselves with [smooth_data]
#' smoothing functions.
#' 
#' @param sm_method Argument specifying which smoothing method should
#'                  be used. Options include "moving-average", "moving-median", 
#'                  "loess", "gam", and "smooth.spline".
#' @param tuneGrid A `data.frame` with possible tuning values. The columns should 
#'                 be named the same as the tuning parameters.
#'                 
#' Note that, when using [train][caret::train], the `tuneGrid`
#'                 must be passed both to this function as well as directly
#'                 to [train][caret::train].
#' 
#' @return A `list` that can be used as the `method` argument to
#'         [train][caret::train]. Contains elements:
#'         `library`, `type`, `prob`, `fit`,
#'         `parameters`, `grid`, `fit`, and `predict`.
#'         
#' See documentation on using a custom model model in 
#'         [train][caret::train] for more details.
#'
#' @export
makemethod_train_smooth_data <- function(sm_method, tuneGrid = NULL) {
  #Create baseline list
  gcmethod_out <- 
    list(library = "gcplyr", type = "Regression", prob = NULL)
  
  #Set up fitting function element
  gcmethod_out$fit <- function(x, y, wts, param, lev = NULL, last,
                           weights, classProbs, ...) {
    sm_args <- c(as.list(param), list(...),
                 sm_method = sm_method, warn_ungrouped = FALSE)
    sm_args[["x"]] <- x$x
    sm_args[["y"]] <- y
    sm_args[["return_fitobject"]] <- TRUE
    return(list(x = x, y = y, modelout = do.call(gcplyr::smooth_data, sm_args)[[1]]))
  }
  
  #Set up parameters and grid elements
  if(!is.null(tuneGrid)) {
    stopifnot(is.data.frame(tuneGrid))
    gcmethod_out$parameters <- data.frame(parameter = colnames(tuneGrid),
                                      class = "numeric", 
                                      label = colnames(tuneGrid))
    gcmethod_out$grid <- function(x, y, len, search) {
      stop("tuneGrid was specified, this grid function should not be called")}
  } else {
    
    #Define the default parameters & grid when tuneGrid is not specified
    if(sm_method %in% c("moving-average", "moving-median")) {
      gcmethod_out$parameters <- 
        data.frame(parameter = "window_width_n",
                   class = "numeric", label = "window_width_n")
      gcmethod_out$grid <-
        function(x, y, len, search) {
          if(search == "grid") {
            myseq <- round(seq(from = 1, to = 13, length.out = len))
            return(data.frame(window_width_n = myseq - (1 - myseq %% 2)))
          } else {
            return(data.frame(
              window_width_n = 
                base::sample(x = seq(from = 1, to = 13, by = 2), size = len)))
          }
        }
    } else if(sm_method == "loess") {
      gcmethod_out$parameters <- 
        data.frame(parameter = "span", class = "numeric", label = "span")
      gcmethod_out$grid <-
        function(x, y, len, search) {
          if(search == "grid") {
            return(data.frame(
              span = seq(from = 0.01, to = 0.2, length.out = len)))
          } else {
            return(data.frame(
              span = stats::runif(min = 0.01, max = 0.2, n = len)))
          }
        }
    } else if(sm_method == "gam") {
      gcmethod_out$parameters <-
        data.frame(parameter = "k", class = "numeric", label = "k")
      gcmethod_out$grid <-
        function(x, y, len, search) {
          if(search == "grid") {
            return(data.frame(
              k = round(seq(from = 10, to = 0.75*nrow(x), length.out = len))))
          } else {
            return(data.frame(
              k = round(stats::runif(min = 10, max = 0.75*nrow(x), n = len))))
          }
        }
    } else if(sm_method == "smooth.spline") {
      gcmethod_out$parameters <- 
        data.frame(parameter = "spar", class = "numeric", label = "spar")
      gcmethod_out$grid <-
        function(x, y, len, search) {
          if(search == "grid") {
            return(data.frame(spar = seq(from = 0, to = 0.5, length.out = len)))
          } else {
            return(data.frame(spar = stats::runif(min = 0, max = 0.5, n = len)))
          }
        }
    }
  }
  
  #Define predict function for each method
  if(sm_method %in% c("moving-average", "moving-median")) {
    gcmethod_out$predict <- 
      function(modelFit, newdata, preProc = NULL, submodels = NULL) {
        return(predict_interpolation(x = modelFit[["x"]]$x, 
                                      y = modelFit[["modelout"]][["fitted"]], 
                                      newdata = newdata$x))}
  } else if(sm_method %in% c("loess", "gam")) {
    gcmethod_out$predict <-
      function(modelFit, newdata, preProc = NULL, submodels = NULL) {
        return(stats::predict(object = modelFit[["modelout"]], newdata = newdata))
      }
  } else if(sm_method == "smooth.spline") {
    gcmethod_out$predict <-
      function(modelFit, newdata, preProc = NULL, submodels = NULL) {
        return(stats::predict(object = modelFit[["modelout"]], x = newdata$x)$y)
      }
  }
  
  return(gcmethod_out)
}


#' Test efficacy of different smoothing parameters
#' 
#' This function is based on [train][caret::train], which runs models
#' (in our case different smoothing algorithms) on data across different 
#' parameter values (in our case different smoothness parameters).
#' 
#' @param ... Arguments passed to [smooth_data]. These arguments cannot
#'            overlap with any of those to be tuned.
#' @param x A `vector` of predictor values to smooth along (e.g. time)
#' @param y A `vector` of response values to be smoothed (e.g. density).
#' @param sm_method Argument specifying which smoothing method should
#'                  be used. Options include "moving-average", "moving-median", 
#'                  "loess", "gam", and "smooth.spline".
#' @param preProcess A `string` `vector` that defines a pre-processing of the
#'                   predictor data. The default is no pre-processing.
#'                   See [train][caret::train] for more details.
#' @param weights    A `numeric` `vector` of case weights. This argument currently
#'                   does not affect any `train_smooth_data` models.
#' @param metric     A `string` that specifies what summary metric will be
#'                   used to select the optimal model. By default, possible
#'                   values are "RMSE" and "Rsquared" for regression.
#'                   See [train][caret::train] for more details.
#' @param maximize   A `logical`: should the metric be maximized or minimized?
#' @param trControl  A `list` of values that define how this function acts.
#'                   See [train][caret::train] and [trainControl][caret::trainControl]
#'                   for more details.
#' @param tuneGrid A `data.frame` with possible tuning values, or a named `list`
#'                 containing `vector`s with possible tuning values. If a data 
#'                 frame, the columns should be named the same as the tuning 
#'                 parameters. If a `list`, the elements of the `list` should be
#'                 named the same as the tuning parameters. If a `list`,
#'                 [expand.grid] will be used to make all possible
#'                 combinations of tuning parameter values.
#' @param tuneLength An `integer` denoting the amount of granularity in
#'                   the tuning parameter grid. By default, this argument
#'                   is the number of levels for each tuning parameter that
#'                   should be generated. If `trControl` has the option
#'                   `search = "random"`, this is the maximum number
#'                   of tuning parameter combinations that will be generated
#'                   by the random search. (NOTE: If given, this argument
#'                   must be named.)
#' @param return_trainobject A `logical` indicating whether the entire result
#'                           of [train][caret::train] should be returned, or
#'                           only the `results` element.
#'                           
#' @details See [train][caret::train] for more information.
#' 
#' The default method is k-fold cross-validation 
#'          (`trControl = caret::trainControl(method = "cv")`). 
#'          
#' For less variable, but more computationally costly, cross-validation,
#'          users may choose to increase the number of folds. This can be
#'          done by altering the `number` argument in 
#'          [trainControl][caret::trainControl], or by setting `method = "LOOCV"` 
#'          for leave one out cross-validation where the number of folds is 
#'          equal to the number of data points. 
#'          
#' For less variable, but more computationally costly, cross-validation,
#'          users may alternatively choose `method = "repeatedcv"` for 
#'          repeated k-fold cross-validation.
#' 
#' For more control, advanced users may wish to call 
#'          [train][caret::train] directly, using 
#'          [makemethod_train_smooth_data] to specify the `method` 
#'          argument.
#' 
#' @return If `return_trainobject = FALSE` (the default), a `data.frame`
#'         with the values of all tuning parameter combinations and the
#'         training error rate for each combination (i.e. the `results`
#'         element of the output of [train][caret::train]).
#'         
#' If `return_trainobject = TRUE`, the output of [train][caret::train]
#' 
#' @export   
train_smooth_data <- function(..., x = NULL, y = NULL, sm_method,
                     preProcess = NULL, weights = NULL,
                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                     maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE", "logLoss"), FALSE, TRUE),
                     trControl = caret::trainControl(method = "cv"), 
                     tuneGrid = NULL,
                     tuneLength = ifelse(trControl$method == "none", 1, 3),
                     return_trainobject = FALSE) {

  if(!requireNamespace("caret", quietly = TRUE)) {
    stop("Package \"caret\" must be installed to use train_smooth_data", call. = FALSE)}
  if("subset_by" %in% names(list(...))) {
    stop("subset_by cannot be used with train_smooth_data, use dplyr::group_by
and dplyr::reframe instead")}
  
  check_grouped(func_name = "reframe", name_for_error = "train_smooth_data",
                subset_by = NULL)
  
  #Parse tuneGrid
  if(!is.null(tuneGrid)) {
    if(is.list(tuneGrid)) {tuneGrid <- expand.grid(tuneGrid)
    } else if (!is.data.frame(tuneGrid)) {
      warning("tuneGrid is not a list nor data.frame")
    }
  }
  
  #Create method argument
  gcmethod <- 
    makemethod_train_smooth_data(sm_method = sm_method, tuneGrid = tuneGrid, ...)
  
  #Run train
  train <- caret::train(x = data.frame(x = x), y = y, method = gcmethod,
                        preProcess = preProcess, weights = weights,
                        metric = metric, maximize = maximize,
                        trControl = trControl,
                        tuneGrid = tuneGrid, tuneLength = tuneLength, ...)
  
  if(return_trainobject) {return(train)} else {return(train$results)}
}


# Processing: Derivatives ----

#' Calculate derivatives of `vector` of data
#' 
#' Provided a `vector` of `y` values, this function returns either the plain
#' or per-capita difference or derivative between sequential values
#' 
#' @param y       Data to calculate difference or derivative of
#' @param x       `vector` of `x` values provided as a simple `numeric`.
#' @param return One of `c("difference", "derivative")` for whether the
#'               differences in `y` should be returned, or the
#'               derivative of `y` with respect to `x`
#' @param percapita When `percapita = TRUE`, the per-capita difference or
#'                  derivative is returned
#' @param x_scale `numeric` to scale `x` by in derivative calculation
#'                
#' Set `x_scale` to the ratio of the units of 
#'                `x` to the desired units. E.g. if `x` is in seconds, but the 
#'                desired derivative is in units of /minute, set 
#'                `x_scale = 60` (since there are 60 seconds in 1 minute).
#' @param blank   `y`-value associated with a "blank" where the density is 0.
#'                Is required when `percapita = TRUE`.
#'                
#' If a `vector` of `blank` values is specified, `blank` values are
#'                assumed to be in the same order as `unique(subset_by)`  
#' @param subset_by An optional `vector` as long as `y`. 
#'                  `y` will be split by the unique values of this `vector` 
#'                  and the derivative for each group will be calculated 
#'                  independently of the others.
#'                  
#' This provides an internally-implemented approach similar
#'                  to [group_by][dplyr::group_by] and [mutate][dplyr::mutate]
#' @param window_width,window_width_n,window_width_frac,window_width_n_frac
#'                  Set how many data points are used to determine
#'                  the slope at each point.
#'                       
#' When all are `NULL`, `calc_deriv` 
#'                  calculates the difference or derivative
#'                  of each point with the next point, appending
#'                  `NA` at the end.
#'                       
#' When one or multiple are specified, a linear regression 
#'                  is fit to all points in the window to determine the 
#'                  slope.
#'                       
#' `window_width_n` specifies the width of the
#'                  window in number of data points. `window_width`
#'                  specifies the width of the window in units of `x`.
#'                  `window_width_n_frac` specifies the width of the
#'                  window as a fraction of the total number of data points.
#'                       
#' When using multiple window specifications at the same 
#'                  time, windows are conservative. Points 
#'                  included in each window will meet all of the 
#'                  `window_width`, `window_width_n`, and
#'                  `window_width_n_frac`.
#'                  
#' A value of `window_width_n = 3` or 
#'                  `window_width_n = 5` is often a good default.
#' @param trans_y  One of `c("linear", "log")` specifying the
#'                 transformation of `y`-values.
#' 
#' `'log'` is only available when calculating per-capita
#'                 derivatives using a fitting approach (when non-default 
#'                 values are specified for `window_width` or 
#'                 `window_width_n`).
#' 
#' For per-capita growth expected to be exponential or 
#'                 nearly-exponential, `"log"` is recommended, since 
#'                 exponential growth is linear when log-transformed. However, 
#'                 log-transformations must be used with care, since `y`-values 
#'                 at or below 0 will become undefined and results will be 
#'                 more sensitive to incorrect values of `blank`.
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing
#' @param warn_ungrouped `logical` whether warning should be issued when
#'                       `calc_deriv` is being called on ungrouped data
#'                       and `subset_by = NULL`.
#' @param warn_logtransform_warnings `logical` whether warning should be issued 
#'                             when `log(y)` produced warnings.
#' @param warn_logtransform_infinite `logical` whether warning should be issued 
#'                             when `log(y)` produced infinite values that will
#'                             be treated as `NA`.
#' @param warn_window_toosmall `logical` whether warning should be issued 
#'                             when only one data point is in the window
#'                             set by `window_width_n`, 
#'                             `window_width`, or `window_width_n_frac`,
#'                             and so `NA` will be returned.
#' 
#' @details For per-capita derivatives, `trans_y = 'linear'` and
#'          `trans_y = 'log'` approach the same value as time resolution
#'          increases. 
#'          
#' For instance, let's assume exponential growth $N = e^rt$ with 
#'          per-capita growth rate $r$.
#'          
#' With `trans_y = 'linear'`, note that $dN/dt = r e^rt = r N$. 
#'          So we can calculate per-capita growth rate as $r = dN/dt * 1/N$. 
#'          
#' With `trans_y = 'log'`, note that $log(N) = log(e^rt) = rt$.
#'          So we can calculate per-capita growth rate as the slope of a linear
#'          fit of $log(N)$ against time, $r = log(N)/t$.
#' 
#' @return A `vector` of values for the plain (if `percapita = FALSE`)
#'         or per-capita (if `percapita = TRUE`) difference 
#'         (if `return = "difference"`) or derivative 
#'         (if `return = "derivative"`) between `y` values. `vector`
#'         will be the same length as `y`,  with `NA` values 
#'         at the ends
#' 
#' @export   
calc_deriv <- function(y, x = NULL, return = "derivative", percapita = FALSE,
                       x_scale = 1, blank = NULL, subset_by = NULL, 
                       window_width = NULL, 
                       window_width_n = NULL,
                       window_width_frac = NULL,
                       window_width_n_frac = NULL,
                       trans_y = "linear", na.rm = TRUE,
                       warn_ungrouped = TRUE,
                       warn_logtransform_warnings = TRUE,
                       warn_logtransform_infinite = TRUE,
                       warn_window_toosmall = TRUE) {
  #Check inputs
  if(!return %in% c("derivative", "difference")) {
    stop("return must be one of c('derivative', 'difference')")}
  
  if(return == "difference" && 
     (!is.null(window_width_n) | !is.null(window_width) | 
      !is.null(window_width_n_frac))) {
    stop("return must be 'derivative' when window_width, window_width_n, or 
window_width_n_frac are used")}
  
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
  
  if(warn_ungrouped) {
    check_grouped(name_for_error = "calc_deriv", subset_by = subset_by)}

  #Set up subset_by
  if(is.null(subset_by)) {subset_by <- rep("A", length(y))}
  
  #Set up blank values
  if(is.null(blank)) {
    if(percapita == TRUE) {stop("blank must be provided when percapita = TRUE")}
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
        if(warn_logtransform_warnings) {
          warning(paste("during log-transformation,", caught_log$warning))}
        caught_log$value[is.nan(caught_log$value)] <- NA}
      if(!is.null(caught_log$error)) {
        stop(paste("during log-transformation,", caught_log$error))}
      sub_y <- caught_log$value
      if(any(is.infinite(sub_y))) {
        if(warn_logtransform_infinite) {
          warning("infinite values created during log-transformation, treating as NA's")}
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
    
    if(is.null(window_width) & is.null(window_width_n) & 
       is.null(window_width_n_frac)) {
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
                             window_width = window_width,
                             window_width_n_frac = window_width_n_frac,
                             window_width_frac = window_width_frac)
      for (j in which(!is.na(windows))) {
        if(any(is.na(sub_y[windows[[j]]]) | is.infinite(sub_y[windows[[j]]]))) {
          sub_ans[j] <- NA
        } else if(length(windows[[j]]) < 2) {
          if(warn_window_toosmall) {
            warning("window only contains one data point, returning NA")}
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
#' Provided a `vector` of per-capita growth rates, this function returns 
#' the `vector` of equivalent doubling times
#' 
#' @param y       `vector` of per-capita derivative data to calculate the 
#'                equivalent doubling time of
#' @param x_scale `numeric` to scale per-capita derivative values by
#'                
#' Set `x_scale` to the ratio of the the units of 
#'                `y` to the desired units. E.g. if `y` is in per-second, but the 
#'                desired doubling time is in minutes, `x_scale = 60` 
#'                (since there are 60 seconds in 1 minute).
#' 
#' @return A `vector` of values for the doubling time equivalent to the
#'         per-capita growth rate supplied for `y`
#' 
#' @export   
doubling_time <- function(y, x_scale = 1) {
  #Check inputs
  y <- make.numeric(y, "y")
  x_scale <- make.numeric(x_scale, "x_scale")

  return(log(2)/(x_scale * y))
}

# Analyze ----

#' Find local extrema of a `numeric` `vector`
#' 
#' These functions take a `vector` of `y` values and identify local extrema.
#'   
#' @param y `numeric` `vector` of `y` values in which to identify local extrema
#' @param x Optional `numeric` `vector` of corresponding `x` values
#' @param window_width,window_width_n,window_height,window_width_frac,window_width_n_frac
#'                   Arguments that set the width/height of the window used to
#'                   search for local extrema.
#'                   
#' `window_width` is in units of `x`.
#'                   
#' `window_width_n` is in units of number of data points.
#'                   
#' `window_height` is the maximum change in `y` 
#'                   a single extrema-search step is allowed to take.
#'                   
#' `window_width_n_frac` is as a fraction of the total
#'                   number of data points.
#'                   
#' For example, the function will not pass a peak or valley
#'                   more than `window_width_n` data points wide, nor
#'                   a peak/valley taller or deeper than `window_height`.
#'                   
#' A narrower width will be more sensitive to narrow local 
#'                   maxima/minima, while a wider width will be less sensitive 
#'                   to local maxima/minima. A smaller height will be more 
#'                   sensitive to shallow local maxima/minima, while a larger 
#'                   height will be less sensitive to shallow maxima/minima.
#' @param return One of `c("index", "x", "y")`, determining whether the function
#'               will return the index, `x` value, or `y` value associated with the
#'               identified extremas
#' @param return_maxima,return_minima `logical` for which classes of local extrema
#'                                    to return
#' @param return_endpoints Should the first and last values in `y`
#'                         be included if they are in the returned 
#'                         `vector` of extrema?
#' @param subset A `vector` of `logical` values indicating which `x` and `y` values
#'               should be included (`TRUE`) or excluded (`FALSE`).
#'               
#' If `return = "index"`, index will be for the whole 
#'               `vector` and not the subset of the `vector`
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing
#' @param width_limit Deprecated, use `window_width` instead
#' @param width_limit_n Deprecated, use `window_width_n` instead
#' @param height_limit Deprecated, use `window_height` instead
#' @param ... (for `first_maxima` and `first_minima`), other 
#'            parameters to pass to `find_local_extrema`
#' 
#' @details 
#' For `find_local_extrema`, one of `window_width`, 
#' `window_width_n`, `window_height`, or `window_width_n_frac`
#' must be provided.
#' 
#' For `first_minima` or `first_maxima`, set 
#' `window_width_n_frac = NULL` to override default width behavior.
#' 
#' If multiple of `window_width`, `window_width_n`,
#' `window_height`, or `window_width_n_frac` are provided, steps 
#' are limited conservatively (a single step must meet all criteria).
#' 
#' In the case of exact ties in `y` values within a window, only the 
#' first local extrema is returned.
#' 
#' @return 
#'    `find_local_extrema` returns a `vector` corresponding to all the 
#'    found local extrema.
#' 
#'    `first_maxima` returns only the first maxima, so is a shortcut for 
#'    `find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]`
#' 
#'    `first_minima` returns only the first minima, so is a shortcut for
#'    `find_local_extrema(return_maxima = FALSE, return_minima = TRUE)[1]`
#' 
#'    If `return = "index"`, the returned value(s) are the indices
#'    corresponding to local extrema in the data
#'           
#'    If `return = "x"`, the returned value(s) are the `x` value(s) 
#'    corresponding to local extrema in the data
#'          
#'    If `return = "y"`, the returned value(s) are the `y` value(s)
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
                               window_width_frac = NULL,
                               window_width_n_frac = NULL,
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
  if (!return_maxima && !return_minima) {
    stop("Both return_maxima and return_minima are FALSE, at least one must be TRUE")
  }
  #Check inputs
  if (is.null(window_width_n) && is.null(window_height) &&
      is.null(window_width) && is.null(window_width_n_frac)) {
    stop("window_width, window_width_n, window_height, or window_width_n_frac must be specified")
  }
  if (!is.null(window_width) && is.null(x)) {
    stop("window_width is specified, but x is not provided")
  }
  
  if (!return %in% c("x", "y", "index")) {
    stop('return must be one of "x", "y", or "index"')
  }
  if(!is.null(x) && length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if(is.null(x) && return == "x") {stop('return = "x" but x is not provided')}
  
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
  
  windows <- get_windows(x = x, y = y, 
                         window_width_n = window_width_n,
                         window_width = window_width, 
                         window_height = window_height,
                         window_width_n_frac = window_width_n_frac,
                         window_width_frac = window_width_frac,
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
                         window_width_frac = NULL,
                         window_width_n_frac = 0.2,
                       return = "index", return_endpoints = TRUE, 
                       ...) {
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
                            window_width_frac = window_width_frac,
                            window_width_n_frac = window_width_n_frac,
                            return = return, ...)[1])
}

#' @rdname ExtremaFunctions
#' @export 
first_minima <- function(y, x = NULL, 
                         window_width = NULL,
                         window_width_n = NULL,
                         window_height = NULL,
                         window_width_frac = NULL,
                         window_width_n_frac = 0.2,
                         return = "index", return_endpoints = TRUE, 
                         ...) {
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
                            window_width_frac = window_width_frac,
                            window_width_n_frac = window_width_n_frac,
                            return = return, ...)[1])
}


#' Find point(s) when a `numeric` `vector` crosses some threshold
#' 
#' These functions take a `vector` of `y` values and identify points where
#' the `y` values cross some `threshold` `y` value.
#' 
#' @param y `numeric` `vector` of `y` values in which to identify threshold
#'          crossing event(s)
#' @param x Optional `numeric` `vector` of corresponding `x` values
#' @param threshold Threshold `y` value of interest
#' @param return One of `c("index", "x")`, determining whether the function
#'               will return the `index` or `x` value associated with the
#'               threshold-crossing event.
#'               
#' If `index`, it will refer to the data point immediately after
#'               the crossing event.
#'               
#' If `x`, it will use linear interpolation and the data
#'               points immediately before and after the threshold-crossing
#'               to return the exact `x` value when the threshold crossing
#'               occurred
#' @param subset A `vector` of `logical` values indicating which `x` and `y` values
#'               should be included (`TRUE`) or excluded (`FALSE`).
#'               
#' If `return = "index"`, index will be for the whole 
#'               `vector` and not the subset of the `vector`
#' @param return_rising `logical` for whether crossing events where `y`
#'                      rises above `threshold` should be returned
#' @param return_falling `logical` for whether crossing events where `y`
#'                      falls below `threshold` should be returned
#' @param return_endpoints `logical` for whether startpoint should be returned
#'                      when the startpoint is above `threshold` and
#'                      `return_rising = TRUE`, or when the startpoint is
#'                      below `threshold` and `return_falling = TRUE`
#' @param na.rm `logical` whether `NA`'s should be removed before analyzing.
#'              If `return = 'index'`, indices will refer to the original
#'              `y` `vector` *including* `NA` values
#' @param ... (for `first_above` and `first_below`) other arguments 
#'            to pass to `find_threshold_crosses`
#'  
#' @return 
#'    `find_threshold_crosses` returns a `vector` corresponding to all the 
#'    threshold crossings.
#' 
#'    `first_above` returns only the first time the `y` values
#'    rise above the `threshold`, so is a shortcut for 
#'    `find_threshold_crosses(return_rising = TRUE, return_falling = FALSE)[1]`
#' 
#'    `first_below` returns only the first time the `y` values
#'    fall below the `threshold`, so is a shortcut for 
#'    `find_threshold_crosses(return_rising = FALSE, return_falling = TRUE)[1]`
#' 
#'    If `return = "index"`, the returned value(s) are the indices
#'    immediately following threshold crossing(s)
#'           
#'    If `return = "x"`, the returned value(s) are the `x` value(s) 
#'    corresponding to threshold crossing(s)
#'    
#'    If no threshold-crossings are detected that meet the criteria, will
#'    return `NA`
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
#' This function takes a `vector` of `x` and `y` values
#' and returns a scalar for the area under the curve, calculated using 
#' the trapezoid rule
#'  
#' @param x `numeric` `vector` of `x` values
#' @param y `numeric` `vector` of `y` values
#' @param xlim `vector`, of length 2, delimiting the `x` range over which the
#'             area under the curve should be calculated (where `NA` can be
#'             provided for the area to be calculated from the start or to
#'             the end of the data)
#' @param blank Value to be subtracted from `y` values before calculating
#'              area under the curve
#' @param subset A `vector` of `logical` values indicating which `x` and `y` values
#'               should be included (`TRUE`) or excluded (`FALSE`).
#' @param na.rm a `logical` indicating whether missing values should be removed
#' @param neg.rm a `logical` indicating whether `y` values below zero should 
#'               be treated as zeros. If `FALSE`, area under the curve
#'               for negative `y` values will be calculated normally,
#'               effectively subtracting from the returned value.
#' @param warn_xlim_out_of_range `logical` whether warning should be issued when 
#'                             `xlim` is lower than the lowest `x` value or higher
#'                             than the highest `x` value.
#' @param warn_negative_y `logical` whether warning should be issued when 
#'                        `neg.rm == FALSE` but some `y` values are below 0.
#' 
#' @return A scalar for the total area under the curve
#'             
#' @export
auc <- function(x, y, xlim = NULL, blank = 0, subset = NULL,
                na.rm = TRUE, neg.rm = FALSE,
                warn_xlim_out_of_range = TRUE, warn_negative_y = TRUE) {
  if(!is.vector(x)) {stop(paste("x is not a vector, it is class:", class(x)))}
  if(!is.vector(y)) {stop(paste("y is not a vector, it is class:", class(y)))}
  
  x <- make.numeric(x)
  y <- make.numeric(y)
  
  #take subset
  subset_temp <- take_subset(x = x, y = y, subset = subset)
  
  #Save original min and max x values so we don't warn when xlim is larger
  #than range of x after NA's have been removed
  x_orig <- c("min" = min(x, na.rm = TRUE), "max" = max(x, na.rm = TRUE))
  
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
      if(warn_xlim_out_of_range && xlim[1] < x_orig[1]) {
        warning("xlim specifies lower limit below the range of x\n")} 
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
      if(warn_xlim_out_of_range && xlim[2] > x_orig[2]) {
        warning("xlim specifies upper limit above the range of x\n")}
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
    } else if(warn_negative_y) {warning("some y values are below 0")}
  }
  
  if(length(unique(x)) < length(x)) {warning("Not all x values are unique")}
  
  #Calculate auc
  # area = 0.5 * (y1 + y2) * (x2 - x1)
  return(sum(0.5 * 
               (y[1:(length(y)-1)] + y[2:length(y)]) *
               (x[2:length(x)] - x[1:(length(x)-1)])))
}


#' Calculate centroid
#' 
#' This function takes a `vector` of `x` and `y` values
#' and returns the `x` and/or `y` position of the centroid of mass of the
#' area under the curve
#'  
#' @param x `numeric` `vector` of `x` values
#' @param y `numeric` `vector` of `y` values
#' @param return One of `c("x", "y", "both")`, determining whether the function
#'               will return the `x` value of the centroid, the `y` value
#'               of the centroid, or a `vector` containing `x` then `y`
#' @param xlim `vector`, of length 2, delimiting the `x` range over which the
#'             centroid should be calculated (where `NA` can be
#'             provided for the area to be calculated from the start or to
#'             the end of the data)
#' @param blank Value to be subtracted from `y` values before calculating
#'              the centroid
#' @param subset A `vector` of `logical` values indicating which `x` and `y` values
#'               should be included (`TRUE`) or excluded (`FALSE`).
#' @param na.rm a `logical` indicating whether missing values should be removed
#' @param neg.rm a `logical` indicating whether `y` values below zero should 
#'               be treated as zeros. If `FALSE`, the centroid
#'               for negative `y` values will be calculated normally,
#'               effectively pulling the centroid towards the `x` axis.
#' @param warn_xlim_out_of_range `logical` whether warning should be issued when 
#'                             `xlim` is lower than the lowest `x` value or higher
#'                             than the highest `x` value.
#' @param warn_negative_y `logical` whether warning should be issued when 
#'                        `neg.rm == FALSE` but some `y` values are below 0.
#' @param ... Other arguments to pass to `centroid`
#' 
#' @return A scalar for the `x` value (if `return = 'x'`) or
#'         `y` value (if `return = 'y'`) of the centroid of the data
#'         
#' @details
#' This function uses [st_centroid][sf::st_centroid] to calculate the centroid of mass
#'         
#' @name CentroidFunctions
NULL  

#' @rdname CentroidFunctions
#' @export
centroid <- function(x, y, return, xlim = NULL, blank = 0, subset = NULL,
                na.rm = TRUE, neg.rm = FALSE,
                warn_xlim_out_of_range = TRUE, warn_negative_y = TRUE) {
  if(!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" must be installed to calculate centroids",
         call. = FALSE)
  }
  
  if(!is.vector(x)) {stop(paste("x is not a vector, it is class:", class(x)))}
  if(!is.vector(y)) {stop(paste("y is not a vector, it is class:", class(y)))}
  
  x <- make.numeric(x)
  y <- make.numeric(y)
  
  #take subset
  subset_temp <- take_subset(x = x, y = y, subset = subset)
  
  #Save original min and max x values so we don't warn when xlim is larger
  #than range of x after NA's have been removed
  x_orig <- c("min" = min(x, na.rm = TRUE), "max" = max(x, na.rm = TRUE))
  
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
      if(warn_xlim_out_of_range && xlim[1] < x_orig[1]) {
        warning("xlim specifies lower limit below the range of x\n")} 
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
      if(warn_xlim_out_of_range && xlim[2] > x_orig[2]) {
        warning("xlim specifies upper limit above the range of x\n")}
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
    } else if(warn_negative_y) {warning("some y values are below 0")}
  }
  
  #Calculate centroid
  # note that st_centroid must circle back to the original point
  # to close the centroid
  centroid_vals <- 
    as.vector(sf::st_centroid(sf::st_polygon(
      x = list(matrix(
        ncol = 2,
        data = c(
          #xvals
          x[c(1, 1:length(x), length(x), 1)],
          #yvals
          c(0, y[1:length(y)], 0, 0)))))))
  
  #Add blank value back
  centroid_vals[2] <- centroid_vals[2] + blank
  
  if(return == "x") {return(centroid_vals[1])
  } else if (return == "y") {return(centroid_vals[2])
  } else if (return == "both") {return(centroid_vals)
  } else {stop("return must be 'x', 'y', or 'both'")}
}

#' @rdname CentroidFunctions
#' @export
centroid_x <- function(x, y, return = "x", ...) {
  if ("return" %in% names(list(...))) {
    stop("return cannot be changed in centroid_x, use centroid for more flexibility")
  }
  
  return(centroid(x = x, y = y, return = return, ...))
}

#' @rdname CentroidFunctions
#' @export
centroid_y <- function(x, y, return = "y", ...) {
  if ("return" %in% names(list(...))) {
    stop("return cannot be changed in centroid_y, use centroid for more flexibility")
  }
  
  return(centroid(x = x, y = y, return = return, ...))
}

#' @rdname CentroidFunctions
#' @export
centroid_both <- function(x, y, return = "both", ...) {
  if ("return" %in% names(list(...))) {
    stop("return cannot be changed in centroid_both, use centroid for more flexibility")
  }
  
  return(centroid(x = x, y = y, return = return, ...))
}


#' Calculate lag time
#' 
#' Lag time is calculated by projecting a tangent line at the point
#' of maximum (per-capita) derivative backwards to find the time when it
#' intersects with the minimum `y`-value
#' 
#' @param x `vector` of `x` values (typically time)
#' @param y `vector` of `y` values (typically density)
#' @param deriv `vector` of derivative values (typically per-capita derivative)
#' @param blank   `y`-value associated with a "blank" where the density is 0.
#'                Is required when `trans_y = TRUE`.
#'                
#' A `vector` of `blank` values may be specified only when all of 
#'                `slope`, `x1`, `y1`, and `y0` are provided
#'                as `vector`s
#' @param trans_y  One of `c("linear", "log")` specifying the
#'                 transformation of `y`-values.
#' 
#' `'log'` is the default, producing calculations of
#'                 lag time assuming a transition to exponential growth
#'                 
#' `'linear'` is available for alternate uses
#' @param na.rm a `logical` indicating whether missing values or values that
#'              become `NA` or infinite during log-transformation should 
#'              be removed
#' @param slope Slope to project from `x1`,`y1` to `y0` (typically per-capita growth
#'              rate). If not provided, will be calculated as `max(deriv)`
#' @param x1 `x` value (typically time) to project slope from. If not provided,
#'           will be calculated as `x[which.max(deriv)]`.
#' @param y1 `y` value (typically density) to project slope from. If not provided,
#'           will be calculated as `y[which.max(deriv)]`.
#' @param y0 `y` value (typically density) to find intersection of slope from
#'             `x1`, `y1` with. If not provided, will be calculated as `min(y)`
#' @param warn_logtransform_warnings `logical` whether warning should be issued 
#'                             when `log(y)` produced warnings.
#' @param warn_logtransform_infinite `logical` whether warning should be issued 
#'                             when `log(y)` produced infinite values that will
#'                             be treated as `NA`.
#' @param warn_min_y_mismatch `logical` whether warning should be issued when 
#'                            `min(y)` does not equal 
#'                            `min(y[!is.na(x)])`.
#' @param warn_multiple_maxderiv `logical` whether warning should be issued when 
#'                             there are multiple points in `deriv` that
#'                             are tied for the highest, and only the first
#'                             will be used.
#' @param warn_one_lag `logical` whether warning should be issued when 
#'                     some, but not all, inputs are vectorized, and
#'                     only one lag time value will be returned.
#' @param warn_no_lag `logical` whether warning should be issued when calculated
#'                    lag time is less than the minimum value of `x`.
#' @param warn_blank_length `logical` whether warning should be issued when an
#'                          unexpected number of `blank` values was 
#'                          provided and only the first will be used
#'                            
#' @details 
#' For most typical uses, simply supply `x`, `y`, and `deriv`
#' (using the per-capita derivative and `trans_y = 'log'`).
#' 
#' Advanced users may wish to use alternate values for the slope of the tangent
#' line (`slope`), origination point of the tangent line (`x1`, 
#' `y1`), or minimum `y`-value `y0`. If specified, these values will 
#' override the default calculations. If and only if all of `slope`, 
#' `x1`, `y1`, and `y0` are provided, `lag_time` is 
#' vectorized on their inputs and will return a `vector` of lag time values.
#'
#' @return Typically a scalar of the lag time in units of `x`. See Details for
#' cases when value will be a `vector`.
#'             
#' @export
lag_time <- function(x = NULL, y = NULL, deriv = NULL, blank = NULL, 
                     trans_y = "log", na.rm = TRUE,
                     slope = NULL, x1 = NULL, y1 = NULL, y0 = NULL,
                     warn_logtransform_warnings = TRUE,
                     warn_logtransform_infinite = TRUE,
                     warn_min_y_mismatch = TRUE,
                     warn_multiple_maxderiv = TRUE,
                     warn_one_lag = TRUE, warn_no_lag = TRUE,
                     warn_blank_length = TRUE) {
  x <- make.numeric(x, "x")
  y <- make.numeric(y, "y")
  deriv <- make.numeric(deriv, "deriv")
  slope <- make.numeric(slope, "slope")
  y0 <- make.numeric(y0, "y0")
  y1 <- make.numeric(y1, "y1")
  x1 <- make.numeric(x1, "x1")
  blank <- make.numeric(blank, "blank")
  
  #Subtract blank values
  if(is.null(blank)) {
    if(trans_y == "log") {stop("blank must be provided when trans_y = 'log'")}
  } else {
    if(!is.null(y1)) {
      if(length(blank) != 1 && length(blank) != length(y1)) {
        if(warn_blank_length) {
          warning("Number of blank values not the same as number of y1 values,
only using the first for normalizing 'y1'")
        }
        y1 <- y1 - blank[1]
      } else {y1 <- y1 - blank}
    }
    if(!is.null(y0)) {
      if(length(blank) != 1 && length(blank) != length(y0)) {
        if(warn_blank_length) {
          warning("Number of blank values not the same as number of y0 values,
only using the first for normalizing 'y0'")
        }
        y0 <- y0 - blank[1]
      } else {y0 <- y0 - blank}
    }
    if(!is.null(y)) {
      if(length(blank) > 1 && warn_blank_length) {
        warning("Multiple blank values provided, only using the first for normalizing 'y'")
      }
      y <- y - blank[1]
    }
  }
  
  #Log-transform y values, as requested
  if(trans_y == "log") {
    if(!is.null(y) && length(y) > 0) {
      caught_log <- gcTryCatch(log(y))
      if(!is.null(caught_log$warning)) {
        if(warn_logtransform_warnings) {
          warning(paste("during log-transformation,", caught_log$warning))}
        caught_log$value[is.nan(caught_log$value)] <- NA}
      if(!is.null(caught_log$error)) {
        stop(paste("during log-transformation,", caught_log$error))}
      y <- caught_log$value
      if(any(is.infinite(y))) {
        if(warn_logtransform_infinite) {
          warning("infinite values created during log-transformation, treating as NA's")}
        y[is.infinite(y)] <- NA
      }
    }
    if(!is.null(y0)) {y0 <- log(y0)}
    if(!is.null(y1)) {y1 <- log(y1)}
  }
  
  #Calculate y0
  if(is.null(y0)) {
    if(is.null(y)) {stop("y or y0 must be provided")}
    y_noNAs <- rm_nas(y = y, na.rm = na.rm)[["y"]] #remove NAs from y alone
    if(length(y_noNAs) < 1 || any(is.na(y_noNAs))) {return(as.numeric(NA))}
    if(warn_min_y_mismatch && na.rm == TRUE &&
       min(y, na.rm = na.rm) != min(rm_nas(x = x, y = y, na.rm = na.rm)[["y"]])) {
      warning("min(y) does not equal min(y[!is.na(x)])")}
    #use vector of y's where the only values removed are those where y is NA
    y0 <- min(y_noNAs)
  }
  
  #Save original x values for later use
  xy_noNAs <- rm_nas(x = x, y = y, na.rm = TRUE)
  
  #remove NA's (including originals and those generated by log-transformation)
  # from any position where x, y, or deriv is NA
  narm_temp <- rm_nas(x = x, y = y, deriv = deriv, na.rm = na.rm)
  x <- narm_temp[["x"]]
  y <- narm_temp[["y"]]
  deriv <- narm_temp[["deriv"]]
  #(cases where ~all values are removed handled with return(NA) code below)
  
  if(is.null(slope)) {
    if(is.null(deriv)) {stop("deriv or slope must be provided")}
    if(length(deriv) < 1) {return(as.numeric(NA))}
    slope <- max(deriv, na.rm = na.rm)
  }
  
  if(xor(is.null(x1), is.null(y1))) {
    stop("both x1 and y1, or neither, must be specified")
  } else if(is.null(y1)) {
    if(is.null(deriv)) {
      if(is.null(y)) {stop("y1, or deriv and y, must be provided")}
      if(is.null(x)) {stop("x1, or deriv and x, must be provided")}
    }
    if(length(deriv) < 1 || length(y) < 1 || length(x) < 1) {return(as.numeric(NA))}
    idxs <- which(deriv == max(deriv, na.rm = na.rm))
    if(warn_multiple_maxderiv && length(idxs) > 1) {
      warning("multiple timepoints have the maximum derivative, using the first")}
    y1 <- y[idxs[1]]
    x1 <- x[idxs[1]]
  }

  if(!all_same(c(length(y0), length(y1), length(slope), length(x1)))) {
    if(warn_one_lag) {warning("Only returning the first lag time value")}
    y0 <- y0[1]; y1 <- y1[1]; slope <- slope[1]; x1 <- x1[1]
  }
  lagvals <- solve_linear(x1 = x1, y1 = y1, m = slope, y2 = y0, named = FALSE)

  if(warn_no_lag && length(lagvals) == 1 && !is.na(lagvals) && 
     !is.null(xy_noNAs[["x"]]) && lagvals < min(xy_noNAs[["x"]])) {
    warning("lag time is less than min(x), indicating no identifiable lag phase")}
  return(lagvals)
}

# Legacy Code ----

#' Find the first local maxima of a `numeric` `vector`
#' 
#' This function has been deprecated in favor of the identical new 
#' function [first_maxima]
#' 
#' This function takes a `vector` of `y` values and returns the index
#' (by default) of the first local maxima. It serves as a shortcut
#' for `find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]`
#' 
#' @seealso [first_maxima]
#' 
#' @param y `numeric` `vector` of `y` values in which to identify local extrema
#' @param x Optional `numeric` `vector` of corresponding `x` values
#' @param return One of `c("index", "x", "y")`, determining whether the function
#'               will return the index, `x` value, or `y` value associated with the
#'               first maxima in `y` values
#' @param window_width Width of the window (in units of `x`) used to
#'                   search for local extrema. A narrower width will be more
#'                   sensitive to narrow local maxima/minima, while a wider
#'                   width will be less sensitive to local maxima/minima.
#' @param window_width_n The maximum number of data points a single 
#'                      extrema-search step is allowed to take. For example,
#'                      when maxima-finding, the function will not pass
#'                      a valley consisting of more than `window_width_n`
#'                      data points.
#'                      
#' A smaller `window_width_n` will be more sensitive 
#'                      to narrow local maxima/minima, while a larger 
#'                      `window_width_n` will be less sensitive to 
#'                      narrow local maxima/minima.
#'                      
#' If not provided, defaults to `~0.2*length(y)`
#' @param window_height The maximum change in `y` a single extrema-search
#'                     step is allowed to take.  For example, when 
#'                     maxima-finding, the function will not pass a
#'                     valley deeper than `window_height`.
#'                     
#' A smaller `window_height` will be more sensitive 
#'                     to shallow local maxima/minima, while a larger 
#'                     `window_height` will be less sensitive to 
#'                     shallow maxima/minima.
#' @param return_endpoints Should the first or last value in `y`
#'                         be allowed to be returned?
#' @param ... Other parameters to pass to [find_local_extrema]
#' 
#' @return If `return = "index"`, a `vector` of indices corresponding 
#'           to local extrema in the data
#'           
#' If `return = "x"`, a `vector` of `x` values corresponding
#'           to local extrema in the data
#'          
#' If `return = "y"`, a `vector` of `y` values corresponding
#'           to local extrema in the data
#' 
#' @details 
#' If none of `window_width`, `window_width_n`, or 
#' `window_height` are provided, default value of `window_width_n`
#' will be used.
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


#' Make tidy design `data.frame`s
#' 
#' This is a function to easily input experimental design elements
#' for later merging with read data
#' 
#' @details 
#' Note that either `nrows` or `block_row_names` must be provided
#' and that either `ncols` or `block_col_names` must be provided
#' 
#' Examples:
#' 
#'     my_example <- make_tidydesign(nrows = 8, ncols = 12,
#'         design_element_name = list(c("Value1", "Value2", "Value3"),
#'                           rowstart:rowend, colstart:colend,
#'                           "111222333000", TRUE)
#'                           
#' To make it easier to pass arguments, use [make_designpattern]:
#' 
#'     my_example <- make_tidydesign(nrows = 8, ncols = 12,
#'       design_element_name = make_designpattern(values = c("L", "G", "C"),
#'                                                 rows = 2:7, cols = 2:11,
#'                                                 pattern = "11223300",
#'                                                 byrow = TRUE))
#' 
#' @param nrows,ncols Number of rows and columns in the plate data
#' @param block_row_names,block_col_names Names of the rows, columns
#'                                     of the plate blockmeasures data
#' @param wellnames_sep A `string` used when concatenating rownames and column
#'                      names to create well names
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names
#' @param wellnames_Excel If `block_row_names` or `block_col_names`
#'                        are not specified, should rows and columns be named
#'                        using Excel-style base-26 lettering for rows
#'                        and numbering for columns? If `FALSE`, rows and columns
#'                        will be numbered with "R" and "C" prefix.
#' @param pattern_split character to split pattern elements provided in
#'                      `...` by
#' @param lookup_tbl_start Value in the lookup table for the split pattern values
#'                         that corresponds to the first value in the `vector`.
#'                         
#' Lookup table by default is 
#'                         `c(1,2,...,8,9,A,B,...Y,Z,a,b,...,y,z)`. If,
#'                         for example, `lookup_tbl_start = "A"`, then the lookup
#'                         table will now be `c(A,B,...Y,Z,a,b,...,y,z)`
#' @param colnames_first  In the wellnames created by [paste]-ing the
#'                        rownames and column names, should the column names
#'                        come first
#' @param ... Each `...` argument must be a `list` with five elements:
#' 
#' 1. a `vector` of the values
#'              
#' 2. a `vector` of the rows the pattern should be applied to
#'              
#' 3. a `vector` of the columns the pattern should be applied to
#'              
#' 4. a `string` of the pattern itself, where numbers refer to
#'               the indices in the values `vector`
#'               
#'    0's refer to `NA`
#'               
#'    This pattern will be split using `pattern_split`, which
#'               defaults to every character
#'               
#' 5. a `logical` for whether this pattern should be filled byrow
#'              
#' @return a tidy-shaped `data.frame` containing all the design elements
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
#' @param tidydesign A tidydesign `data.frame` (e.g. as created by [make_tidydesign])
#' @param collapse `NULL` or a `string` to use for concatenating design elements
#'                 together. If `NULL` each design column will be put into its
#'                 own block. If a `string`, that `string` will be used to [paste]
#'                 together all design elements and all design elements will
#'                 be returned in a single block
#' @param wellnames_sep A `string` used when concatenating rownames and column
#'                      names to create well names
#' @param wellnames_colname Header for newly-created column containing the
#'                          well names
#' @return A `list` of blockdesign `data.frame`s (if `collapse` is not
#'         `NULL` the `list` is of length 1
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
