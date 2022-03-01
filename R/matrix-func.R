# generic to get co-occurrence matrix from objects of various classes
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
                                   neighbor = "manhattan",    # doesn't do anything
                                   normalize = "sum1", ...) { # doesn't do anything
  # discretize FL (`x`) to `nlevels` levels
  x <- discretize(x, nlevels = nlevels)

  # initialize co-occurrence matrix with all -1 (zero if actually processed)
  comat <- matrix(-1, nrow = nlevels, ncol = nlevels)

  # count co-occurrences
  for (i in seq_len(nrow(comat))) {
    for (j in seq_len(ncol(comat))) {
      comat[i, j] <- count_element_occur(x, i, j)
    }
  }

  # make symmetric
  comat <- comat + t(comat)

  # normalize
  comat <- comat / sum(comat)

  # return co-occurrence matrix
  return(comat)
}
