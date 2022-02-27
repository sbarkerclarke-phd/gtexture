#####HOMOGENEITY#####
# S3 generic for GLCM homogeneity metric
# current goal to implement only for FitLandDF class; default throws error
homogeneity <- function(x, ...) {
  UseMethod("homogeneity")
}

# worry about this when generalizing homogeneity past fitness landscapes
homogeneity.default <- function(x, ...) {
  stop("default behavior has not been defined for homogeneity")
}

#####CONTRAST#####
# S3 generic for GLCM contrast metric
# current goal to implement only for FitLandDF class; default throws error
contrast <- function(x, ...) {
  UseMethod("contrast")
}

# worry about this when generalizing contrast past fitness landscapes
contrast.default <- function(x, ...) {
  stop("default behavior has not been defined for contrast")
}

#####ENTROPY#####
# S3 generic for GLCM entropy metric
# current goal to implement only for FitLandDF class; default throws error
entropy <- function(x, ...) {
  UseMethod("entropy")
}

# worry about this when generalizing entropy past fitness landscapes
entropy.default <- function(x, ...) {
  stop("default behavior has not been defined for entropy")
}
