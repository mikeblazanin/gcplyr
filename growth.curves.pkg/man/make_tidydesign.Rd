\name{make_tidydesign}
\alias{make_tidydesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_tidydesign(nrows = NULL, ncols = NULL, block_row_names = NULL, block_col_names = NULL, wellnames_sep = "_", wellnames_colname = "Well", pattern_split = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrows}{
%%     ~~Describe \code{nrows} here~~
}
  \item{ncols}{
%%     ~~Describe \code{ncols} here~~
}
  \item{block_row_names}{
%%     ~~Describe \code{block_row_names} here~~
}
  \item{block_col_names}{
%%     ~~Describe \code{block_col_names} here~~
}
  \item{wellnames_sep}{
%%     ~~Describe \code{wellnames_sep} here~~
}
  \item{wellnames_colname}{
%%     ~~Describe \code{wellnames_colname} here~~
}
  \item{pattern_split}{
%%     ~~Describe \code{pattern_split} here~~
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
function (nrows = NULL, ncols = NULL, block_row_names = NULL, 
    block_col_names = NULL, wellnames_sep = "_", wellnames_colname = "Well", 
    pattern_split = "", ...) 
{
    stopifnot(!(is.null(nrows) & is.null(block_row_names)), !(is.null(ncols) & 
        is.null(block_col_names)))
    if (is.null(block_row_names)) {
        block_row_names <- paste("R", 1:nrows, sep = ".")
    }
    if (is.null(block_col_names)) {
        block_col_names <- paste("C", 1:ncols, sep = ".")
    }
    if (is.null(nrows)) {
        nrows <- length(block_row_names)
    }
    if (is.null(ncols)) {
        ncols <- length(block_col_names)
    }
    dot_args <- list(...)
    output <- as.data.frame(matrix(NA, nrow = nrows * ncols, 
        ncol = 1 + length(unique(names(dot_args)))))
    output[, 1] <- paste(rep(block_row_names, each = ncols), 
        block_col_names, sep = wellnames_sep)
    colnames(output)[1] <- wellnames_colname
    for (i in 1:length(dot_args)) {
        pattern_list <- as.numeric(strsplit(dot_args[[i]][[4]], 
            split = pattern_split)[[1]])
        if (((length(dot_args[[i]][[2]]) * length(dot_args[[i]][[3]]))\%\%length(pattern_list)) != 
            0) {
            warning(paste("Total number of wells is not a multiple of pattern length for", 
                names(dot_args)[i]))
        }
        if (length(dot_args[[i]]) < 5) {
            dot_args[[i]][[5]] <- TRUE
        }
        pattern_list[pattern_list == 0] <- NA
        vals_list <- dot_args[[i]][[1]][rep(pattern_list, length.out = (length(dot_args[[i]][[2]]) * 
            length(dot_args[[i]][[3]])))]
        block_out <- matrix(NA, nrow = nrows, ncol = ncols)
        block_out[dot_args[[i]][[2]], dot_args[[i]][[3]]] <- matrix(vals_list, 
            nrow = length(dot_args[[i]][[2]]), ncol = length(dot_args[[i]][[3]]), 
            byrow = dot_args[[i]][[5]])
        block_mlt <- c(t(block_out))
        mycol <- match(names(dot_args)[i], unique(names(dot_args))) + 
            1
        output[which(!is.na(block_mlt)), mycol] <- block_mlt[which(!is.na(block_mlt))]
        colnames(output)[mycol] <- names(dot_args)[i]
    }
    return(output)
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
