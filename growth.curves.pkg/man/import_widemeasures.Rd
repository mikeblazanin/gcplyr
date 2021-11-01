\name{import_widemeasures}
\alias{import_widemeasures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
import_widemeasures(files, extension = NULL, startrow = NULL, endrow = NULL, startcol = NULL, endcol = NULL, header = TRUE, sheet = NULL, wide_names = NULL)
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
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{sheet}{
%%     ~~Describe \code{sheet} here~~
}
  \item{wide_names}{
%%     ~~Describe \code{wide_names} here~~
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
    startcol = NULL, endcol = NULL, header = TRUE, sheet = NULL, 
    wide_names = NULL) 
{
    if (!sum(is.null(startrow), is.null(endrow), is.null(startcol), 
        is.null(endcol)) \%in\% c(0, 4)) {
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
    if (is.null(extension)) {
        require(tools)
        extension <- vapply(files, tools::file_ext, FUN.VALUE = "return strings", 
            USE.NAMES = FALSE)
    }
    else {
        extension <- checkdim_inputs(extension, "extension", 
            length(files))
        stopifnot(all(extension \%in\% c("csv", "xls", "xlsx")))
    }
    if (any(extension == "xls" | extension == "xlsx")) {
        require(readxl)
    }
    outputs <- rep(list(NA), length(files))
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
        if (header == TRUE) {
            if (is.null(startrow[i])) {
                outputs[[i]] <- temp[2:nrow(temp), ]
                colnames(outputs[[i]]) <- temp[1, ]
            }
            else {
                outputs[[i]] <- temp[startrow:endrow, startcol:endcol]
                colnames(outputs[[i]]) <- temp[startrow - 1, 
                  startcol:endcol]
            }
        }
        else {
            if (is.null(startrow[i])) {
                outputs[[i]] <- temp
            }
            else {
                outputs[[i]] <- temp[startrow:endrow, startcol:endcol]
            }
            colnames(outputs[[i]]) <- paste("V", 1:ncol(temp), 
                sep = "")
        }
    }
    if (!is.null(wide_names)) {
        stopifnot(length(wide_names) == length(files))
        names(outputs) <- wide_names
    }
    else {
        names(outputs) <- sub("^\\.?/?(.*)\\.[[:alnum:]]+$", 
            "\\1", files)
    }
    if (length(outputs) == 1) {
        return(outputs[[1]])
    }
    else {
        return(outputs)
    }
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
