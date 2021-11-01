\name{block_tidydesign}
\alias{block_tidydesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
block_tidydesign(tidydesign, collapse = NULL, wellnames_sep = "_", wellnames_colname = "Well")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tidydesign}{
%%     ~~Describe \code{tidydesign} here~~
}
  \item{collapse}{
%%     ~~Describe \code{collapse} here~~
}
  \item{wellnames_sep}{
%%     ~~Describe \code{wellnames_sep} here~~
}
  \item{wellnames_colname}{
%%     ~~Describe \code{wellnames_colname} here~~
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
function (tidydesign, collapse = NULL, wellnames_sep = "_", wellnames_colname = "Well") 
{
    rownames <- sapply(strsplit(tidydesign[, wellnames_colname], 
        split = wellnames_sep), simplify = TRUE, FUN = function(x) {
        x[1]
    })
    colnames <- sapply(strsplit(tidydesign[, wellnames_colname], 
        split = wellnames_sep), simplify = TRUE, FUN = function(x) {
        x[2]
    })
    output <- rep(list(matrix(NA, nrow = length(unique(rownames)), 
        ncol = length(unique(colnames)))), length(which(colnames(tidydesign) != 
        wellnames_colname)))
    i <- 1
    for (col in which(colnames(tidydesign) != wellnames_colname)) {
        rownames(output[[i]]) <- unique(rownames)
        colnames(output[[i]]) <- unique(colnames)
        output[[i]][match(rownames, rownames(output[[i]])) + 
            (match(colnames, colnames(output[[i]])) - 1) * nrow(output[[i]])] <- tidydesign[, 
            col]
        i <- i + 1
    }
    if (!is.null(collapse)) {
        output <- list(matrix(do.call("paste", c(output, sep = collapse)), 
            nrow = length(unique(rownames)), ncol = length(unique(colnames))))
        rownames(output[[1]]) <- unique(rownames)
        colnames(output[[1]]) <- unique(colnames)
        names(output) <- paste(colnames(tidydesign)[which(colnames(tidydesign) != 
            wellnames_colname)], collapse = collapse)
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
