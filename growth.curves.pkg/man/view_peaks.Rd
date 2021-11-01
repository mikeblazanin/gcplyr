\name{view_peaks}
\alias{view_peaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
view_peaks(data_mlt, data_out, plt_point = TRUE, numplots = 9, lwd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_mlt}{
%%     ~~Describe \code{data_mlt} here~~
}
  \item{data_out}{
%%     ~~Describe \code{data_out} here~~
}
  \item{plt_point}{
%%     ~~Describe \code{plt_point} here~~
}
  \item{numplots}{
%%     ~~Describe \code{numplots} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
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
function (data_mlt, data_out, plt_point = TRUE, numplots = 9, 
    lwd = 1) 
{
    for (start_group in seq(from = 1, to = length(unique(data_mlt$Contents)), 
        by = numplots)) {
        my_groups <- unique(data_mlt$Contents)[start_group:(start_group + 
            numplots - 1)]
        myplot <- ggplot(data = data_mlt[data_mlt$Contents \%in\% 
            my_groups, ], aes(x = Time, y = sm_od)) + geom_line(lwd = lwd) + 
            facet_wrap(~Contents) + ylab("Smoothed OD600")
        if (plt_point) {
            myplot <- myplot + geom_point(data = data_out[data_out$Contents \%in\% 
                my_groups, ], aes(x = maxtime, y = max), size = 3, 
                pch = 13)
        }
        print(myplot)
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
