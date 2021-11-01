\name{import_blockmeasures}
\alias{import_blockmeasures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
import_blockmeasures(files, num_plates = 1, plate_names = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{num_plates}{
%%     ~~Describe \code{num_plates} here~~
}
  \item{plate_names}{
%%     ~~Describe \code{plate_names} here~~
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
function (files, num_plates = 1, plate_names = NULL, ...) 
{
    blockmeasures <- uninterleave(read_blocks(files = files, 
        ...), n = num_plates, ...)
    widemeasures <- rep(list(NA), num_plates)
    for (i in 1:length(blockmeasures)) {
        widemeasures[[i]] <- widen_blocks(blockmeasures[[i]], 
            wellnames_sep = wellnames_sep)
    }
    if (is.null(plate_names)) {
        names(widemeasures) <- paste("plate_", 1:length(widemeasures), 
            sep = "")
    }
    else {
        names(widemeasures) <- plate_names
    }
    if (num_plates == 1) {
        return(widemeasures[[1]])
    }
    else {
        return(widemeasures)
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
