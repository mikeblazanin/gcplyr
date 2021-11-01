\name{layout_data_merge}
\alias{layout_data_merge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
layout_data_merge(layout, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layout}{
%%     ~~Describe \code{layout} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (layout, data) 
{
    layout_mlt <- reshape2::melt(layout, id.vars = 1, value.name = "Contents", 
        variable.name = "column")
    layout_mlt$Well <- paste(layout_mlt[, 1], substr(layout_mlt$column, 
        2, nchar(as.character(layout_mlt$column))), sep = "")
    data_mlt <- reshape2::melt(data, id.vars = c("Time", "Temperature"), 
        variable.name = "Well", value.name = "OD600")
    data_mlt$Contents <- layout_mlt$Contents[match(as.character(data_mlt$Well), 
        as.character(layout_mlt$Well))]
    data_mlt$format <- paste(colnames(layout)[1], "_Rep", sep = "")
    return(data_mlt)
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
