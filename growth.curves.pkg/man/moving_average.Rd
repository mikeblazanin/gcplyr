\name{moving_average}
\alias{moving_average}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moving_average(my_data, window_width, subset_by)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_data}{
%%     ~~Describe \code{my_data} here~~
}
  \item{window_width}{
%%     ~~Describe \code{window_width} here~~
}
  \item{subset_by}{
%%     ~~Describe \code{subset_by} here~~
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
function (my_data, window_width, subset_by) 
{
    out_list <- rep(NA, length(my_data))
    cntr = 1
    for (my_uniq in unique(subset_by)) {
        my_sub <- as.numeric(subset(my_data, subset_by == my_uniq))
        out_list[cntr:(cntr + length(my_sub) - window_width)] <- 0
        for (i in 1:window_width) {
            out_list[(cntr):(cntr + length(my_sub) - window_width)] <- out_list[(cntr):(cntr + 
                length(my_sub) - window_width)] + my_sub[i:(length(my_sub) - 
                window_width + i)]
        }
        cntr <- cntr + length(my_sub)
    }
    out_list <- out_list/window_width
    return(out_list)
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
