\name{find_local_extrema}
\alias{find_local_extrema}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find_local_extrema(values, return_maxima = TRUE, return_minima = TRUE, width_limit = NULL, height_limit = NULL, remove_endpoints = TRUE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{return_maxima}{
%%     ~~Describe \code{return_maxima} here~~
}
  \item{return_minima}{
%%     ~~Describe \code{return_minima} here~~
}
  \item{width_limit}{
%%     ~~Describe \code{width_limit} here~~
}
  \item{height_limit}{
%%     ~~Describe \code{height_limit} here~~
}
  \item{remove_endpoints}{
%%     ~~Describe \code{remove_endpoints} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
function (values, return_maxima = TRUE, return_minima = TRUE, 
    width_limit = NULL, height_limit = NULL, remove_endpoints = TRUE, 
    na.rm = FALSE) 
{
    if (!return_maxima & !return_minima) {
        stop("Both return_maxima and return_minima are FALSE, at least one must be TRUE")
    }
    if (is.null(width_limit) & is.null(height_limit)) {
        stop("Either width_limit or height_limit must be provided")
    }
    if (!is.null(width_limit)) {
        if (width_limit\%\%2 == 0) {
            warning("width_limit must be odd, will use ", width_limit - 
                1, " as width_limit")
            width_limit <- width_limit - 1
        }
    }
    if (is.null(width_limit) & !is.null(height_limit)) {
        warning("height_limit alone tends to be sensitive to height_limit parameter, use with caution")
    }
    if (na.rm == TRUE & sum(is.na(values)) > 0) {
        if (!all(is.na(values[(1 + length(values) - sum(is.na(values))):length(values)]))) {
            warning("Removing NAs found within values vector, returned indices will refer to non-NA values")
            print(values)
        }
        values <- values[!is.na(values)]
    }
    else if (any(is.na(values))) {
        stop("Some provided values are NA and na.rm = FALSE")
    }
    get_window_limits <- function(cnt_pos, width_limit = NULL, 
        height_limit = NULL, looking_for = c("minima", "maxima"), 
        values = NULL) {
        if (length(looking_for) > 1) {
            stop("looking_for must be specified")
        }
        if (!is.null(height_limit) & is.null(values)) {
            stop("height_limit is specified, but no values are provided")
        }
        if (is.null(width_limit) & is.null(height_limit)) {
            stop("Either width_limit or height_limit must be provided")
        }
        window_start <- c(NA, NA)
        if (!is.null(width_limit)) {
            window_start[1] <- max(c(1, cnt_pos - floor(width_limit/2)))
        }
        if (!is.null(height_limit)) {
            window_start[2] <- max(c(1, 1 + which(1:length(values) < 
                cnt_pos & (values >= (values[cnt_pos] + height_limit) | 
                values <= (values[cnt_pos] - height_limit)))))
            if (window_start[2] >= cnt_pos) {
                window_start[2] <- cnt_pos - 1
            }
        }
        window_end <- c(NA, NA)
        if (!is.null(width_limit)) {
            window_end[1] <- min(c(length(values), cnt_pos + 
                floor(width_limit/2)))
        }
        if (!is.null(height_limit)) {
            window_end[2] <- min(c(length(values), -1 + which(1:length(values) > 
                cnt_pos & (values <= (values[cnt_pos] - height_limit) | 
                values >= (values[cnt_pos] + height_limit)))))
            if (window_end[2] <= cnt_pos) {
                window_end[2] <- cnt_pos + 1
            }
        }
        return(c(max(window_start, na.rm = T), min(window_end, 
            na.rm = T)))
    }
    find_next_extrema <- function(cnt_pos, values, width_limit = NULL, 
        height_limit = NULL, looking_for = c("minima", "maxima")) {
        if (cnt_pos == length(values)) {
            best_pos <- cnt_pos - 1
        }
        else {
            best_pos <- cnt_pos + 1
        }
        start_pos <- cnt_pos
        if (looking_for == "maxima") {
            while (cnt_pos != best_pos) {
                best_pos <- cnt_pos
                window_lims <- get_window_limits(cnt_pos = cnt_pos, 
                  width_limit = width_limit, height_limit = height_limit, 
                  looking_for = "maxima", values = values)
                window_lims <- c(max(start_pos, window_lims[1]), 
                  max(start_pos, window_lims[2]))
                cnt_pos <- window_lims[1] - 1 + which.max(values[window_lims[1]:window_lims[2]])
            }
        }
        else if (looking_for == "minima") {
            while (cnt_pos != best_pos) {
                best_pos <- cnt_pos
                window_lims <- get_window_limits(cnt_pos = cnt_pos, 
                  width_limit = width_limit, height_limit = height_limit, 
                  looking_for = "minima", values = values)
                window_lims <- c(max(start_pos, window_lims[1]), 
                  max(start_pos, window_lims[2]))
                cnt_pos <- window_lims[1] - 1 + which.min(values[window_lims[1]:window_lims[2]])
            }
        }
        return(best_pos)
    }
    cnt_pos <- 1
    maxima_list <- c(find_next_extrema(cnt_pos, values, width_limit = width_limit, 
        height_limit = height_limit, looking_for = "maxima"))
    minima_list <- c(find_next_extrema(cnt_pos, values, width_limit = width_limit, 
        height_limit = height_limit, looking_for = "minima"))
    while (TRUE) {
        if (any(duplicated(c(minima_list, maxima_list)))) {
            break
        }
        if (length(values) \%in\% c(maxima_list, minima_list)) {
            break
        }
        cnt_pos <- max(c(minima_list, maxima_list))
        if (cnt_pos \%in\% minima_list) {
            maxima_list <- c(maxima_list, find_next_extrema(cnt_pos, 
                values, width_limit = width_limit, height_limit = height_limit, 
                looking_for = "maxima"))
        }
        else if (cnt_pos \%in\% maxima_list) {
            minima_list <- c(minima_list, find_next_extrema(cnt_pos, 
                values, width_limit = width_limit, height_limit = height_limit, 
                looking_for = "minima"))
        }
    }
    output <- c()
    if (return_maxima) {
        output <- c(output, maxima_list)
    }
    if (return_minima) {
        output <- c(output, minima_list)
    }
    if (remove_endpoints) {
        if (1 \%in\% output) {
            output <- output[-which(output == 1)]
        }
        if (length(values) \%in\% output) {
            output <- output[-which(output == length(values))]
        }
    }
    output <- unique(output)
    output <- output[order(output)]
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
