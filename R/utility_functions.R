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
      "##   Blazanin, Michael. ", year, ". 'gcplyr: manipulate and analyze growth\n",
      "##   curve data.' R package version ", ver, "\n",
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
#' @noRd
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
#' @param x A vector of column names in Excel-style base-26 letter format
#'          (any values that are already in base-10 will be returned as-is)
#' 
#' @return A vector of numbers in base-10
#' 
#' @export
from_excel <- function(x) {
  #Based on: https://stackoverflow.com/questions/48983939/convert-a-number-to-excel-s-base-26
  out <- rep(NA, length(x))
  already_nums <- sapply(x, canbe.numeric, infinite_num = FALSE, nan_num = FALSE)
  out[which(already_nums)] <- as.numeric(x[which(already_nums)])
  not_nums <- which(!already_nums)
  x_splt <- strsplit(x[not_nums], "")
  for (i in 1:length(x_splt)) {
    #Get indices of letters
    temp <- match(x_splt[[i]], LETTERS)
    #Multiply indices by powers of 26
    out[not_nums[i]] <- sum(temp*26**((length(temp)-1):0))
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
#' @noRd
dots_parser <- function(FUN, ...) {
  argnames <- names(formals(FUN))
  dots <- list(...)
  return(do.call(FUN, dots[names(dots) %in% argnames]))
}

#' A function that infers whether blocks have nested metadata
#' 
#' @param blocks The list of blocks
#' 
#' @return TRUE or FALSE whether the blocks contain metadata
#'         (also passes warning when inferring, or error when unable to)
#' 
#' @noRd
infer_block_metadata <- function(blocks) {
  #Infer nestedness if nested_metadata is set to NULL
  if (all(sapply(blocks, simplify = TRUE, FUN = class) == "data.frame")) {
    message("Inferring nested_metadata to be FALSE\n")
    return(FALSE)
  } else if (all(sapply(blocks, simplify = TRUE, FUN = class) == "list")) {
    message("Inferring nested_metadata to be TRUE\n")
    return(TRUE)
  } else {
    stop("Unable to infer nested_metadata, this may be because blocks vary in nestedness or are not data.frame's")
  }
}

#' A function that finds a character not present in block data nor metadata
#' 
#' Typically this is used to find a separator that can be used without
#' accidentally implying separations in the data that shouldn't be there
#' 
#' @param blocks The list of blocks
#' @param nested_metadata A Boolean for if there is nested metadata in
#'                        \code{blocks}. Will attempt to infer if left NULL
#' 
#' @return vector of characters not found in the blocks that can be used
#'         as a separator without issue (or error if none can be found)
#' 
#' @noRd
sep_finder <- function(blocks, nested_metadata = NULL) {
  if (is.null(nested_metadata)) {
    nested_metadata <- infer_block_metadata(blocks)
  }
  
  #Look for a character that is not present
  # in the data/metadata and so can be used as a separator
  sep <- c("_", " ", "-", ",", ";")
  if(nested_metadata == TRUE) {
    not_in_blocks <-
      sapply(X = sep,
             FUN = function(y) {
               !any(grepl(pattern = y, fixed = TRUE,
                          x = unlist(
                            lapply(X = blocks,
                                   FUN = function(x) {
                                     c(unlist(x[1]), unlist(x[2]))
                                   }))))
             })
  } else if (nested_metadata == FALSE) {
    not_in_blocks <-
      sapply(X = sep,
             FUN = function(y) {
               !any(grepl(pattern = y, fixed = TRUE,
                          x = unlist(
                            lapply(X = blocks,FUN = function(x) {unlist(x[1])}))))
             })
  }
  
  if(!any(not_in_blocks)) {
    stop("all of '_', ' ', '-', ',', and ';' are found in the files,
              specify a sep not found in the files")
  } else {return(sep[which(not_in_blocks)])}
}

#' A better version of is.numeric
#' 
#' This function works like is.numeric but also checks if x can be coerced to 
#' numeric without warnings. If is.numeric or can be coerced to numeric, 
#' returns TRUE. Otherwise, returns FALSE
#' 
#' @param x Vector to check
#' @param infinite_num Should infinite values (or values that are coerced
#'                     to be infinite by as.numeric) be considered as numeric?
#' @param nan_num Should NaN values (or values that are coerced
#'                     to be infinite by as.numeric) be considered as numeric?
#' @return TRUE if \code{x} is numeric or can be converted to numeric without
#'         warnings. Otherwise, FALSE
#' 
#' @noRd
canbe.numeric <- function(x, infinite_num = TRUE, nan_num = TRUE) {
  if(is.numeric(x) | 
     tryCatch(
       {
         as.numeric(x)
         #This only gets executed if the line above doesn't warn or error
         if(!infinite_num && is.infinite(as.numeric(x))) {FALSE
         } else if (!nan_num && is.nan(as.numeric(x))) {FALSE
         } else {TRUE}
       }, 
       warning = function(w) {FALSE},
       error = function(e) {stop(e)}
     )) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}

#' A function that coerces to numeric or returns an error if not possible
#' 
#' @param x Value or vector to force to numeric
#' @param varname Name of the variable to use for error message if needed
#' @return as.numeric(x) if canbe.numeric(x) == TRUE
#' 
#' @noRd
make.numeric <- function(x, varname) {
  if(is.null(x) | is.numeric(x)) {return(x)
  } else if(canbe.numeric(x)) {return(as.numeric(x))
  } else {stop(paste(varname, "cannot be coerced to numeric"))}
}

#' A function that removes na's values from a set of vectors
#' 
#' Vectors will have all indices removed where any of the vectors
#' are NA at that index
#' 
#' @param ... Vectors to remove NA's from.
#' @param na.rm Boolean, should NA's be removed
#' @param stopifNA Boolean, should an error be passed if na.rm = FALSE
#'                 and there are NA's in x or y?
#' @return A list containing: each input ... vector with NA's removed
#'                          
#'                            vector of indices where NA's were removed
#'                            (NULL when none were removed)
#' 
#' @noRd
rm_nas <- function(..., na.rm, stopifNA = FALSE) {
  out <- list(..., "nas_indices_removed" = NULL)
  if(na.rm == TRUE) {
    out[["nas_indices_removed"]] <- 
      unique(unlist(lapply(X = list(...), FUN = function(x) which(is.na(x)))))
    out[["nas_indices_removed"]] <- 
      out[["nas_indices_removed"]][order(out[["nas_indices_removed"]])]
    out[names(out) != "nas_indices_removed"] <-
      lapply(X = out[names(out) != "nas_indices_removed"],
             FUN = function(x, idx_rem) x[-idx_rem],
             idx_rem = out[["nas_indices_removed"]])
  } else { #don't remove NA's
    if(stopifNA == TRUE && any(unlist(lapply(list(...), is.na)))) {
      stop("Some values are NA but na.rm = FALSE")
    }
  }

  return(out)
}

#' A function that adds NA values to a vector or pair of vectors
#' 
#' NA's will be added to x and y at indices specified where they had 
#' been previously removed
#' 
#' @param x Vector to add NA's to
#' @param y Optional second vector to add NA's to
#' @param nas_indices_removed Indices where NA's had previously been removed
#'                            (AKA indices where NA's will be in the
#'                            *output* vector)
#' @return A list containing: x with NA's added
#' 
#'                            y with NA's added
#' 
#' @noRd
add_nas <- function(x, y = NULL, nas_indices_removed) {
  if(!is.null(nas_indices_removed)) {
    if(!is.null(y) & length(x) != length(y)) {
      stop("x and y for NA addition are not the same length")
    }
    
    return_indices <- 1:length(x)
    for (index in nas_indices_removed) {
      return_indices[return_indices >= index] <-
        return_indices[return_indices >= index] + 1
    }
    
    out <- list("x" = rep(NA, length(x) + length(nas_indices_removed)),
                "y" = NULL)
    out[["x"]][return_indices] <- x
    
    if(!is.null(y)) {
      out[["y"]] <- rep(NA, length(x) + length(nas_indices_removed))
      out[["y"]][return_indices] <- y
    }
    
    return(out)
    
  } else {return(list(x = x, y = y))}
}

#' A function that reorders x and y based on x
#' 
#' @param x Vector to reorder based on
#' @param y Vector to reorder based on x
#' @return A list containing: 
#' 
#'     [1] "x" = the reordered x
#' 
#'     [2] "y" = the reordered y
#'                            
#'     [3] "order" = the original order, such that:
#'                            
#'     \code{return[["x"]][order(return[["order"]])] == x}
#'                            
#'     and
#'                            
#'     \code{return[["y"]][order(return[["order"]])] == y}
#' 
#' @noRd
reorder_xy <- function(x = NULL, y) {
  if(!is.null(x)) {
    #Save orig order info so we can put things back at the end
    start_order <- order(x)
    #Reorder
    y <- y[start_order]
    x <- x[start_order]
  } else {start_order <- 1:length(y)}
  
  return(list(x = x, y = y, order = start_order))
}


#' A function that gets windows for moving-window like calculations
#' 
#' @param x Vector of x values
#' @param y Vector of y values
#' @param window_width Width of the window (in units of \code{x}).
#' @param window_width_n Width of the window (in number of \code{y} values).
#' @param window_height The maximum change in \code{y} within each window.
#' @param edge_NA Boolean for whether windows that pass the edge of the data
#'                should be returned as NA or simply truncated at the edge
#'                of the data.
#'                
#'                For instance, moving average-like functions typically want
#'                edge_NA = TRUE so as not to calculate a moving-average
#'                on points near the edge of the domain with smaller n
#'                
#'                In contrast, extrema-finding typically want edge_NA = FALSE
#'                so that searching simply stops at the edge of the data
#' @param force_height_multi_n 
#'                Boolean for whether windows limits set by \code{window_height}
#'                should always have at least 3 data points in them (or 2 
#'                data points for windows located at the edge of the domain)
#'                         
#'                This is necessary for window-finding with window_height 
#'                because otherwise you can end up with windows that contain 
#'                only a single data-point, counting as both a max and a min
#' @return A list of vectors, where each vector contains the indices
#'         of the data in the window centered at that point
#'      
#' @noRd   
get_windows <- function(x, y, window_width_n = NULL, window_width = NULL, 
                        window_height = NULL, edge_NA, 
                        force_height_multi_n = FALSE) {
  if(any(c(is.na(x), is.na(y)))) {
    stop("NA's must be removed before getting windows")}
  if(!all(order(x) == 1:length(x))) {
    stop("data must be ordered before getting windows")}
  
  window_starts <- matrix(data = 1, nrow = length(x), ncol = 3)
  window_ends <- matrix(data = length(x), nrow = length(x), ncol = 3)
  
  if(!is.null(window_width_n)) {
    temp <- 
      lapply(X = as.list(1:length(x)),
             xvals = x,
             FUN = function(xidx, xvals) {
               which(abs(xidx - 1:length(xvals)) <= (window_width_n - 1)/2)})
    window_starts[, 1] <- sapply(temp, min)
    window_ends[, 1] <- sapply(temp, max)
    if(edge_NA) {
      window_starts[(1:length(x) + (window_width_n - 1)/2 >= length(x)+1) |
                      (1:length(x) - (window_width_n - 1)/2 <= 0), 1] <- NA
      window_ends[(1:length(x) + (window_width_n - 1)/2 >= length(x)+1) |
                    (1:length(x) - (window_width_n - 1)/2 <= 0), 1] <- NA
    }
  }
  if(!is.null(window_width)) {
    temp <- lapply(X = as.list(1:length(x)),
                   xvals = x,
                   FUN = function(xidx, xvals) {
                     which(abs(xvals - xvals[xidx]) <= window_width/2)})
    window_starts[, 2] <- sapply(temp, min)
    window_ends[, 2] <- sapply(temp, max)
    if(edge_NA) {
      window_starts[(x + window_width/2 > max(x)) |
                      (x - window_width/2 < min(x)), 2] <- NA
      window_ends[(x + window_width/2 > max(x)) |
                    (x - window_width/2 < min(x)), 2] <- NA
    }
  }
  if(!is.null(window_height)) {
    #First calculate all points by all points whether they're close enough
    # to each other
    ygrid <- lapply(X = y, yvals = y, window_height = window_height,
                    FUN = function(y, yvals, window_height) {
                      which(abs(y-yvals) <= window_height)})
    #Then find the smallest blocks of contiguous points that are within the
    # height limit
    for (i in 1:length(ygrid)) {
      window_starts[i, 3] <- 
        ygrid[[i]][
          max(which(ygrid[[i]] <= i & c(TRUE, diff(ygrid[[i]]) != 1)))]
      window_ends[i, 3] <-
        ygrid[[i]][
          min(which(ygrid[[i]] >= i & c(diff(ygrid[[i]]) != 1, TRUE)))]
      #Force windows to include at least one point either side if specified
      if(force_height_multi_n) {
        window_starts[i, 3] <- min(max(i-1, 1), window_starts[i, 3])
        window_ends[i, 3] <- max(min(i+1, length(ygrid)), window_ends[i, 3])
      }
    }
  }
  
  #Calculate the most-conservative window starts & window ends
  # then fill in the sequence of values between them & return
  return(apply(matrix(ncol = 2, nrow = nrow(window_starts),
                      data = c(
                        apply(window_starts, MARGIN = 1, FUN = max),
                        apply(window_ends, MARGIN = 1, FUN = min))),
               MARGIN = 1,
               FUN = function(x) {
                 if(any(is.na(c(x[1], x[2])))) {NA
                 } else {seq(from = x[1], to = x[2])}
               },
               simplify = FALSE))
}

#' A function that checks if the parent function is being called within
#' mutate with grouped data or with subset_by specified
#' 
#' @param func_name Name of the function to check for
#' @param inherit_name Name of the attribute to check for
#' @param name_for_error Name of the function to add to error messages
#' @param subset_by Subset by function
#' @return Nothing, but prints warnings
#' 
#' @noRd
check_grouped <- function(func_name = "mutate", inherit_name = "grouped_df",
                          name_for_error, subset_by) {
  parents <- ls(envir = parent.frame(n = 2))
  if(all(parents == "~")) {
    ss <- sys.status()
    funcs <- sapply(ss$sys.calls, function(x) deparse(as.list(x)[[1]]))
    wf <- which(funcs == func_name)
    if(length(wf) > 0) {
      data <- eval(substitute(.data), ss$sys.frames[[max(wf)]])
      if(is.null(subset_by) & !inherits(data, "grouped_df")) {
        warning(paste(name_for_error,
                      "called on an ungrouped data.frame and subset_by = NULL"))
      }
    }
  }
  if(is.null(subset_by) && (any(parents != "~") || length(wf) == 0)) {
    warning(paste(name_for_error,
                  "called outside of dplyr::mutate and subset_by = NULL"))
  }
}


#' A function that runs tryCatch but returns the messages in a list
#' 
#' @param expr The expression to run
#' @return Returns list with $value $warning $error
#'         
#'         If successful, $warning and $error will be \code{NULL}
#'         
#'         If warning, $error will be \code{NULL} ($value will be present)
#'         
#'         If error, $warning and $value will be \code{NULL}
#'         
#' @details 
#' I didn't write this, see: https://stackoverflow.com/a/24569739/14805829
#' 
#' @noRd
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

#' Return missing information about a line
#' 
#' Takes a set of inputs that is sufficient information to infer a line
#' and then returns information not provided (either the slope, an x point
#' on the line, or a y point on the line)
#' 
#' @params x1,y1 A point on the line
#' @params x2,y2 An additional point on the line
#' @params x3,y3 An additional point on the line
#' @param m The slope of the line
#' 
#' @return A named vector with the missing information from the line:
#' 
#'         If m and x2 are provided, y2 will be returned
#'         
#'         If m and y2 are provided, x2 will be returned
#'         
#'         If x2 and y2 are provided, but neither x3 nor y3 are provided,
#'         m will be returned
#'         
#'         If x2 and y2 are provided and one of x3 or y3 are provided, the other
#'         (y3 or x3) will be returned
#' 
#' @export
solve_linear <- function(x1, y1, x2 = NULL, y2 = NULL, x3 = NULL, y3 = NULL,
                         m = NULL) {
  #provide x1 and y1
  #to get y2, provide x2 and m
  #to get x2, provide y2 and m
  #to get m, provide x2 and y2
  #to get x3, provide x1, y1, x2, y2, y3
  #to get y3, provide x1, y1, x2, y2, x3
  if(!is.null(x2) & !is.null(m)) {return(setNames(m * (x2-x1) + y1, "y2"))} #return y2
  if(!is.null(y2) & !is.null(m)) {return(setNames(x1 + (y2-y1)/m, "x2"))} #return x2
  if(!is.null(x2) & !is.null(y2)) {
    m <- (y2-y1)/(x2-x1)
    if(!is.null(x3)) {return(setNames(m * (x3-x1) + y1, "y3")) #provide y3
    } else if (!is.null(y3)) {return(setNames(x1 + (y3-y1)/m, "x3")) #provide x3
    } else {return(setNames(m, "m"))} #return m
  }
  stop("solve_linear does not have enough information")
}


