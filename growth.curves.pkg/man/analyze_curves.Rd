\name{analyze_curves}
\alias{analyze_curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
analyze_curves(od_data, time_data, bandwidth = 10, return)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{od_data}{
%%     ~~Describe \code{od_data} here~~
}
  \item{time_data}{
%%     ~~Describe \code{time_data} here~~
}
  \item{bandwidth}{
%%     ~~Describe \code{bandwidth} here~~
}
  \item{return}{
%%     ~~Describe \code{return} here~~
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
function (od_data, time_data, bandwidth = 10, return) 
{
    prev_max_pos <- 0
    cnt_max_pos <- bandwidth
    while (cnt_max_pos != prev_max_pos) {
        prev_max_pos <- cnt_max_pos
        search_start <- pmax(cnt_max_pos - bandwidth, 1)
        search_end <- pmin(search_start + 2 * bandwidth, length(od_data))
        cnt_max_pos <- which.max(od_data[search_start:search_end]) + 
            search_start - 1
    }
    if (return == "max") {
        return(od_data[cnt_max_pos])
    }
    else if (return == "maxtime") {
        return(time_data[cnt_max_pos])
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
