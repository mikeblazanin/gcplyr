\name{import_blockdesign}
\alias{import_blockdesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
import_blockdesign(files, startrow = 2, startcol = 2, metadata = list(design_element = c(1, 1)), field_sep = "_", id_cols = c("block_name", "design_element"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{startrow}{
%%     ~~Describe \code{startrow} here~~
}
  \item{startcol}{
%%     ~~Describe \code{startcol} here~~
}
  \item{metadata}{
%%     ~~Describe \code{metadata} here~~
}
  \item{field_sep}{
%%     ~~Describe \code{field_sep} here~~
}
  \item{id_cols}{
%%     ~~Describe \code{id_cols} here~~
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
function (files, startrow = 2, startcol = 2, metadata = list(design_element = c(1, 
    1)), field_sep = "_", id_cols = c("block_name", "design_element"), 
    ...) 
{
    blockdesigns <- read_blocks(files = files, metadata = metadata, 
        startrow = startrow, startcol = startcol, ...)
    widedesigns <- widen_blocks(blockdesigns, ...)
    tidydesigns <- pivot_wide_longer(widedesigns, id_cols = id_cols, 
        values_to = widedesigns$design_element[1], ...)
    split_blockdesign()
    layout_cleanup <- function(layout) {
        vals_and_cntr <- matrix(nrow = 0, ncol = 2)
        for (i in 1:nrow(layout)) {
            for (j in 2:ncol(layout)) {
                if (!is.na(layout[i, j])) {
                  if (nchar(layout[i, j]) > 1) {
                    current_val <- layout[i, j]
                    if (!(current_val \%in\% vals_and_cntr[, 1])) {
                      vals_and_cntr <- rbind(vals_and_cntr, c(current_val, 
                        1))
                      row <- nrow(vals_and_cntr)
                    }
                    else {
                      row <- which(vals_and_cntr[, 1] == current_val)
                    }
                  }
                  layout[i, j] <- paste(current_val, LETTERS[as.numeric(vals_and_cntr[row, 
                    2])], sep = "_")
                  vals_and_cntr[row, 2] <- as.numeric(vals_and_cntr[row, 
                    2]) + 1
                }
            }
        }
        return(layout)
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
