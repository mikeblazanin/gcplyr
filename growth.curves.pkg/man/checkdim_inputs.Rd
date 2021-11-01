\name{checkdim_inputs}
\alias{checkdim_inputs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkdim_inputs(input, input_name, needed_len, needed_name = "the number of files")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{input_name}{
%%     ~~Describe \code{input_name} here~~
}
  \item{needed_len}{
%%     ~~Describe \code{needed_len} here~~
}
  \item{needed_name}{
%%     ~~Describe \code{needed_name} here~~
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
function (input, input_name, needed_len, needed_name = "the number of files") 
{
    if (length(input) != needed_len) {
        if (length(input) != 1) {
            stop(paste("More than one", input_name, "value is supplied, but the number\n                   of", 
                input_name, "values is not equal to", needed_name))
        }
        else {
            return(rep(input, needed_len))
        }
    }
    else {
        return(input)
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
