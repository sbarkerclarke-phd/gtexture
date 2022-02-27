#####DISCRETIZE#####
# discretize fitness landscape values (numeric) into integer levels
#' Discretize
#'
#' @export
discretize <- function(x, ...) {
  UseMethod("discretize")
}

# error for default behavior at the moment
discretize.default <- function(x, nlevels, ...) {
  stop("default behavior not specified for discretize")
}

# discretize numeric vectors
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
discretize.integer <- function(x, nlevels, ...) {
  discretize.numeric(x, nlevels, ...)
}

# discretize fitness landscape
discretize.FitLandDF <- function(x, nlevels, ...) {
  x$Value <- discretize.numeric(x$Value, nlevels, ...)

  return(x)
}
