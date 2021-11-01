\name{read_blocks}
\alias{read_blocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read_blocks(files, extension = NULL, startrow = NULL, endrow = NULL, startcol = NULL, endcol = NULL, sheet = NULL, metadata = NULL, block_names = NULL, infer_colnames = TRUE, infer_rownames = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{extension}{
%%     ~~Describe \code{extension} here~~
}
  \item{startrow}{
%%     ~~Describe \code{startrow} here~~
}
  \item{endrow}{
%%     ~~Describe \code{endrow} here~~
}
  \item{startcol}{
%%     ~~Describe \code{startcol} here~~
}
  \item{endcol}{
%%     ~~Describe \code{endcol} here~~
}
  \item{sheet}{
%%     ~~Describe \code{sheet} here~~
}
  \item{metadata}{
%%     ~~Describe \code{metadata} here~~
}
  \item{block_names}{
%%     ~~Describe \code{block_names} here~~
}
  \item{infer_colnames}{
%%     ~~Describe \code{infer_colnames} here~~
}
  \item{infer_rownames}{
%%     ~~Describe \code{infer_rownames} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (files, extension = NULL, startrow = NULL, endrow = NULL, 
    startcol = NULL, endcol = NULL, sheet = NULL, metadata = NULL, 
    block_names = NULL, infer_colnames = TRUE, infer_rownames = TRUE, 
    ...) 
{
    if (is.null(startrow)) {
        startrow <- rep(NA, length(files))
    }
    else {
        startrow <- checkdim_inputs(startrow, "startrow", length(files))
    }
    if (is.null(endrow)) {
        endrow <- rep(NA, length(files))
    }
    else {
        endrow <- checkdim_inputs(endrow, "endrow", length(files))
    }
    if (is.null(startcol)) {
        startcol <- rep(NA, length(files))
    }
    else {
        startcol <- checkdim_inputs(startcol, "startcol", length(files))
    }
    if (is.null(endcol)) {
        endcol <- rep(NA, length(files))
    }
    else {
        endcol <- checkdim_inputs(endcol, "endcol", length(files))
    }
    if (!is.null(sheet)) {
        sheet <- checkdim_inputs(sheet, "sheet", length(files))
    }
    infer_colnames <- checkdim_inputs(infer_colnames, "infer_colnames", 
        length(files))
    infer_rownames <- checkdim_inputs(infer_rownames, "infer_rownames", 
        length(files))
    if (!is.null(block_names)) {
        stopifnot(length(block_names) == length(files))
    }
    if (is.null(extension)) {
        require(tools)
        extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
            USE.NAMES = FALSE)
    }
    else {
        extension <- checkdim_inputs(extension, "extension", 
            length(files))
    }
    stopifnot(extension \%in\% c("csv", "xls", "xlsx"))
    if (any(extension == "xls" | extension == "xlsx")) {
        require(readxl)
    }
    if (is.null(metadata)) {
        outputs <- rep(list(list(data = NA, metadata = c(block_name = "NA"))), 
            length(files))
    }
    else {
        metadata_vector <- rep(NA, times = length(metadata) + 
            1)
        names(metadata_vector) <- c("block_name", names(metadata))
        outputs <- rep(list(list(data = NA, metadata = metadata_vector)), 
            length(files))
    }
    for (i in 1:length(files)) {
        if (extension[i] == "csv") {
            temp <- read.csv(files[i], colClasses = "character", 
                header = FALSE)
        }
        else if (extension[i] == "xls") {
            suppressMessages(temp <- as.data.frame(readxl::read_xls(files[i], 
                col_names = FALSE, col_types = "text", sheet = sheet[i])))
        }
        else if (extension[i] == "xlsx") {
            suppressMessages(temp <- as.data.frame(readxl::read_xlsx(files[i], 
                col_names = FALSE, col_types = "text", sheet = sheet[i])))
        }
        if (is.na(endrow[i])) {
            endrow[i] <- nrow(temp)
        }
        if (is.na(endcol[i])) {
            endcol[i] <- ncol(temp)
        }
        temp_startcol <- 0
        temp_startrow <- 0
        if (infer_colnames[i] == TRUE) {
            if (!is.na(startrow[i])) {
                colnames_row <- startrow[i] - 1
                temp_startrow <- startrow[i]
            }
            else {
                if (!is.na(startcol[i])) {
                  if (temp[1, startcol[i] - 1] == "") {
                    colnames_row <- 1
                    temp_startrow <- 2
                  }
                  else {
                    colnames_row <- 0
                    temp_startrow <- 1
                  }
                }
                else {
                  if (temp[1, 1] == "") {
                    colnames_row <- 1
                    temp_startrow <- 2
                  }
                  else {
                    colnames_row <- 0
                    temp_startrow <- 1
                  }
                }
            }
        }
        else {
            colnames_row <- 0
            if (is.na(startrow[i])) {
                temp_startrow <- 1
            }
            else {
                temp_startrow <- startrow[i]
            }
        }
        if (infer_rownames[i] == TRUE) {
            if (!is.na(startcol[i])) {
                rownames_col <- startcol[i] - 1
                temp_startcol <- startcol[i]
            }
            else {
                if (!is.na(startrow[i])) {
                  if (temp[startrow[i] - 1, 1] == "") {
                    rownames_col <- 1
                    temp_startcol <- 2
                  }
                  else {
                    rownames_col <- 0
                    temp_startcol <- 1
                  }
                }
                else {
                  if (temp[1, 1] == "") {
                    rownames_col <- 1
                    temp_startcol <- 2
                  }
                  else {
                    rownames_col <- 0
                    temp_startcol <- 1
                  }
                }
            }
        }
        else {
            rownames_col <- 0
            if (is.na(startcol[i])) {
                temp_startcol <- 1
            }
            else {
                temp_startcol <- startcol[i]
            }
        }
        if (temp_startrow == 0) {
            stop("temp_startrow = 0, this shouldn't happen")
        }
        else {
            startrow[i] <- temp_startrow
        }
        if (temp_startcol == 0) {
            stop("temp_startcol = 0, this shouldn't happen")
        }
        else {
            startcol[i] <- temp_startcol
        }
        if (colnames_row == 0) {
            temp_colnames <- paste("C", 1:ncol(outputs[[i]]$data), 
                sep = ".")
        }
        else {
            temp_colnames <- temp[colnames_row, startcol[i]:endcol[i]]
        }
        if (rownames_col == 0) {
            if (length(startrow[i]:startcol[i]) > 26) {
                stop("Automatic rownames for blockmeasures with more than 26 rows is not supported")
            }
            temp_rownames <- paste("R", LETTERS[1:nrow(outputs[[i]]$data)], 
                sep = ".")
        }
        else {
            temp_rownames <- temp[startrow[i]:endrow[i], rownames_col]
        }
        outputs[[i]]$data <- temp[startrow[i]:endrow[i], startcol[i]:endcol[i]]
        colnames(outputs[[i]]$data) <- temp_colnames
        rownames(outputs[[i]]$data) <- temp_rownames
        if (!is.null(block_names)) {
            outputs[[i]]$metadata["block_name"] <- block_names[i]
        }
        else {
            outputs[[i]]$metadata["block_name"] <- sub("^\\.?/?(.*)\\.[[:alnum:]]+$", 
                "\\1", files[i])
        }
        if (!is.null(metadata)) {
            for (j in 1:length(metadata)) {
                outputs[[i]]$metadata[j + 1] <- temp[metadata[[j]][1], 
                  metadata[[j]][2]]
            }
        }
    }
    if (length(outputs) > 1 & var(sapply(outputs, simplify = TRUE, 
        FUN = function(x) {
            dim(x$data)[1]
        })) != 0) {
        warning("Not all blockmeasures have the same number of rows of data")
    }
    if (length(outputs) > 1 & var(sapply(outputs, simplify = TRUE, 
        FUN = function(x) {
            dim(x$data)[2]
        })) != 0) {
        warning("Not all blockmeasures have the same number of columns of data")
    }
    return(outputs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
