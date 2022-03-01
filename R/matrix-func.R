# generic to get co-occurrence matrix from objects of various classes
#' Calculate Co-Occurrence Matrix
#'
#' Calculate generalized co-occurrence matrix from a variety of objects,
#' currently including fitness landscapes stored as a `FitLandDF` instance from
#' the `fitscape` package.
get_comatrix <- function(x, ...) {
  UseMethod("get_comatrix")
}

# default not defined
get_comatrix.default <- function(x, ...) {
  stop("default behavior for get_comatrix not yet defined")
}

# for fitness landscapes
# 1. discretize FL to `nlevels` levels
# 2. initialize co-occurrence matrix with all -1 (zero if actually processed)
# 3. count co-occurrences
# 4. normalize and return
get_comatrix.FitLandDF <- function(x,
                                   nlevels,
                                   discrete = equal_discrete(nlevels),       # currently a function from factory - need to export factory
                                   neighbor = manhattan(1),                  # currently a function from factory - need to export factory
                                   normalize = function(mat) mat / sum(mat), # currently a function
                                   ...) { # doesn't do anything
  # discretize FL (`x`) to `nlevels` levels, equal to integers 1:nlevels
  x$Value <- discrete(x$Value)

  # initialize co-occurrence matrix with all -1 (zero if actually processed)
  comat <- matrix(-1, nrow = nlevels, ncol = nlevels)

  # count co-occurrences
  for (i in seq_len(nrow(comat))) {
    for (j in seq_len(ncol(comat))) {
      comat[i, j] <- count_element_occur(x, i, j, neighbor = neighbor)
    }
  }

  # make symmetric
  comat <- comat + t(comat)

  # normalize
  comat <- normalize(comat)

  # return co-occurrence matrix
  return(comat)
}
