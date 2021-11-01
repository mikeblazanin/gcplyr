\name{pivot_wide_longer}
\alias{pivot_wide_longer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pivot_wide_longer(widemeasures, data_cols = NA, id_cols = NA, names_to = "Well", values_to = "Measurements", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widemeasures}{
%%     ~~Describe \code{widemeasures} here~~
}
  \item{data_cols}{
%%     ~~Describe \code{data_cols} here~~
}
  \item{id_cols}{
%%     ~~Describe \code{id_cols} here~~
}
  \item{names_to}{
%%     ~~Describe \code{names_to} here~~
}
  \item{values_to}{
%%     ~~Describe \code{values_to} here~~
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
function (widemeasures, data_cols = NA, id_cols = NA, names_to = "Well", 
    values_to = "Measurements", ...) 
{
    if (!is.list(id_cols)) {
        id_cols <- list(id_cols)
    }
    id_cols <- checkdim_inputs(id_cols, "id_cols", length(widemeasures))
    if (!is.list(data_cols)) {
        data_cols <- list(data_cols)
    }
    data_cols <- checkdim_inputs(data_cols, "data_cols", length(widemeasures))
    if (any(!is.na(data_cols) & !is.na(id_cols))) {
        warning("Cannot provide both data_cols and id_cols for a given widemeasures, using data_cols only")
    }
    require(tidyr)
    if (is.data.frame(widemeasures)) {
        widemeasures <- list(widemeasures)
    }
    names_to <- checkdim_inputs(names_to, "names_to", length(widemeasures))
    values_to <- checkdim_inputs(values_to, "values_to", length(widemeasures))
    outputs <- rep(list(NA), length(widemeasures))
    for (i in 1:length(widemeasures)) {
        if (!is.na(data_cols[i])) {
            outputs[[i]] <- as.data.frame(tidyr::pivot_longer(widemeasures[[i]], 
                names_to = names_to[i], values_to = values_to[i], 
                cols = data_cols[[i]], ...))
        }
        else if (!is.na(id_cols[i])) {
            outputs[[i]] <- as.data.frame(tidyr::pivot_longer(widemeasures[[i]], 
                names_to = names_to[i], values_to = values_to[i], 
                cols = which(!colnames(widemeasures[[i]]) \%in\% 
                  id_cols[[i]]), ...))
        }
        else {
            outputs[[i]] <- as.data.frame(tidyr::pivot_longer(widemeasures[[i]], 
                names_to = names_to[i], values_to = values_to[i], 
                ...))
        }
    }
    if (length(outputs) == 1) {
        return(outputs[[1]])
    }
    else {
        return(outputs)
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
