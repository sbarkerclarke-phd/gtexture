#####DISCRETIZE#####
# discretize fitness landscape values (numeric) into integer levels
#' Discretize Numeric Variable Into Categories
#'
#' Takes a numeric variable (could be of class `numeric` or `integer`) and
#' returns a discretized version, in which each element has been replaced by a
#' single integer between `1` and `nlevels`, inclusive.
#'
#' @name discretize
#' @param x either a vector (`numeric` or `integer`) or `FitLandDF` object
#' @param nlevels positive integer indicating number of discrete categories
#' @param ... potential additional arguments, currently unnecessary
#' @return discretized form of `x`
#' @import fitscape
#' @export
#' @examples
#'
#' ## discretize a numeric vector
#' vec <- 1:10
#' discretize(vec, nlevels = 5) # discretize into 5 categories
#' discretize(vec, 2)           # discretize into 2 categories
#'
#' ## discretize a fitness landscape
#' # create a 3x3x3 fitness landscape with values 1 through 27
#' fl_data <- array(1:27, dim = rep(3, 3))
#' my_fl <- fitscape::FitLandDF(fl_data)
#' discretize(my_fl, nlevels = 2) # discretize landscape into 2 categories
#' discretize(my_fl, 5)           # discretize landscape into 5 categories
discretize <- function(x, ...) {
  UseMethod("discretize")
}

# error for default behavior at the moment
discretize.default <- function(x, nlevels, ...) {
  stop("default behavior not specified for discretize")
}

# discretize numeric vectors
#' @rdname discretize
#' @export
discretize.numeric <- function(x, nlevels, ...) {
  # check parameter validity
  stopifnot(valid_pos_int(nlevels))

  # split into levels and return discrete value
  break_vals <- seq(from = 0, to = nlevels, by = 1) / nlevels * max(x)
  x %>%
    cut(breaks = break_vals) %>%
    as.integer
}

# discretize integer vectors
#' @rdname discretize
#' @export
discretize.integer <- function(x, nlevels, ...) {
  discretize.numeric(x, nlevels, ...)
}

# discretize fitness landscape
#' @rdname discretize
#' @export
discretize.FitLandDF <- function(x, nlevels, ...) {
  x$Value <- discretize.numeric(x$Value, nlevels, ...)

  return(x)
}
