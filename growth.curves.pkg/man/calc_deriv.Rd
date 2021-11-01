\name{calc_deriv}
\alias{calc_deriv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_deriv(density, percapita = FALSE, subset_by = NULL, time = NULL, time_normalize = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{percapita}{
%%     ~~Describe \code{percapita} here~~
}
  \item{subset_by}{
%%     ~~Describe \code{subset_by} here~~
}
  \item{time}{
%%     ~~Describe \code{time} here~~
}
  \item{time_normalize}{
%%     ~~Describe \code{time_normalize} here~~
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
function (density, percapita = FALSE, subset_by = NULL, time = NULL, 
    time_normalize = NULL) 
{
    if (!is.numeric(time)) {
        stop("time is not numeric")
    }
    if (!is.null(time_normalize)) {
        if (!is.numeric(time_normalize)) {
            stop("time_normalize is not numeric")
        }
        else if (is.null(time)) {
            stop("time_normalize is specified, but time is not provided")
        }
    }
    ans <- c(density[2:length(density)] - density[1:(length(density) - 
        1)])
    if (percapita) {
        ans <- ans/density[1:(length(density) - 1)]
    }
    if (!is.null(time_normalize)) {
        ans <- ans/(c(time[2:length(time)] - time[1:(length(time) - 
            1)])/time_normalize)
    }
    if (!is.null(subset_by)) {
        ans[subset_by[2:length(subset_by)] != subset_by[1:(length(subset_by) - 
            1)]] <- NA
    }
    return(c(ans, NA))
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
