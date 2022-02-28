# generic to get co-occurrence matrix from objects of various classes
get_comatrix <- function(x, ...) {
  UseMethod("get_comatrix")
}

# default not defined
get_comatrix.default <- function(x, ...) {
  stop("default behavior for get_comatrix not yet defined")
}

# for fitness landscapes
get_comatrix.FitLandDF <- function(x, ...) {

}
