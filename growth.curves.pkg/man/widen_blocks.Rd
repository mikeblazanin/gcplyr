\name{widen_blocks}
\alias{widen_blocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
widen_blocks(blockmeasures, wellnames_sep = "_", nested_metadata = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blockmeasures}{
%%     ~~Describe \code{blockmeasures} here~~
}
  \item{wellnames_sep}{
%%     ~~Describe \code{wellnames_sep} here~~
}
  \item{nested_metadata}{
%%     ~~Describe \code{nested_metadata} here~~
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
function (blockmeasures, wellnames_sep = "_", nested_metadata = NULL, 
    ...) 
{
    if (class(blockmeasures) != "list") {
        blockmeasures <- list(blockmeasures)
    }
    if (is.null(nested_metadata)) {
        if (all(sapply(blockmeasures, simplify = TRUE, FUN = class) == 
            "data.frame")) {
            nested_metadata <- FALSE
            warning("Inferring nested_metadata to be FALSE")
        }
        else if (all(sapply(blockmeasures, simplify = TRUE, FUN = class) == 
            "list")) {
            nested_metadata <- TRUE
            warning("Inferring nested_metadata to be TRUE")
        }
        else {
            stop("Unable to infer nested_metadata, this may be because blockmeasures vary in nestedness or are not data.frame's")
        }
    }
    if (length(blockmeasures) > 1) {
        if (nested_metadata) {
            if (var(sapply(blockmeasures, simplify = TRUE, FUN = function(x) {
                dim(x[[1]])[1]
            })) != 0) {
                stop("Not all blockmeasures have the same number of rows of data")
            }
            if (var(sapply(blockmeasures, simplify = TRUE, FUN = function(x) {
                dim(x[[1]])[2]
            })) != 0) {
                stop("Not all blockmeasures have the same number of columns of data")
            }
        }
        else {
            if (var(sapply(blockmeasures, simplify = TRUE, FUN = function(x) {
                dim(x)[1]
            })) != 0) {
                stop("Not all blockmeasures have the same number of rows of data")
            }
            if (var(sapply(blockmeasures, simplify = TRUE, FUN = function(x) {
                dim(x)[2]
            })) != 0) {
                stop("Not all blockmeasures have the same number of columns of data")
            }
        }
    }
    if (nested_metadata) {
        output <- data.frame(t(sapply(blockmeasures, simplify = TRUE, 
            function(x) {
                c(x[[2]], t(x[[1]]))
            })), stringsAsFactors = FALSE)
        colnames(output) <- c(names(blockmeasures[[1]][[2]]), 
            paste(rep(rownames(blockmeasures[[1]][[1]]), each = ncol(blockmeasures[[1]][[1]])), 
                colnames(blockmeasures[[1]][[1]]), sep = wellnames_sep))
    }
    else {
        output <- data.frame(t(sapply(blockmeasures, simplify = TRUE, 
            function(x) {
                c(t(x))
            })), stringsAsFactors = FALSE)
        colnames(output) <- paste(rep(rownames(blockmeasures[[1]]), 
            each = ncol(blockmeasures[[1]])), colnames(blockmeasures[[1]]), 
            sep = wellnames_sep)
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
