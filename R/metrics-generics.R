# each of the metrics normalizees the GLCM, then applies the metric itself

#####HOMOGENEITY#####
# S3 generic for GLCM homogeneity metric
# current goal to implement only for FitLandDF class; default throws error
#'
homogeneity <- function(x, ...) {
  UseMethod("homogeneity")
}

# worry about this when generalizing homogeneity past fitness landscapes
homogeneity.default <- function(x, ...) {
  stop("default behavior has not been defined for homogeneity")
}

# the actual workhorse for homogeneity (others will eventually call this)
homogeneity.matrix <- function(x, ...) {

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

#####ENERGY#####
# S3 generic for GLCM energy metric
# aim to implement for matrix and FitLandDF
#' Energy Metric for a GLCM
#'
#' Calculate the energy feature or metric for a gray-level co-occurrence
#' matrix. For definition and application, see Lofstedt et al. (2019)
#' <doi:10.1371/journal.pone.0212110>.
#'
#' @name energy
#' @param x gray-level co-occurrence matrix
#' @param nlevels desired number of discrete gray levels
#' @param ... additional parameters
#' @return energy metric of `x`
#' @export
#' @examples
#' ## calculate energy of arbitrary GLCM
#' # define arbitrary GLCM
#' x <- matrix(1:16, nrow = 4)
#'
#' # normalize
#' n_x <- normalize_glcm(x)
#'
#' # calculate energy
#' energy(n_x)
#'
#' ## calculate energy of arbitrary fitness landscape
#' # create fitness landscape using FitLandDF object
#' vals <- runif(64)
#' vals <- array(vals, dim = rep(4, 3))
#' my_landscape <- fitscape::FitLandDF(vals)
#'
#' # calculate energy of fitness landscape, assuming 2 discrete gray levels
#' energy(my_landscape, nlevels = 2)
#'
#' ## confirm value of energy for fitness landscape
#' # extract normalized GLCM from fitness landscape
#' my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))
#'
#' # calculate energy of extracted GLCM
#' energy(my_glcm)  # should match value of above energy function call
energy <- function(x, ...) {
  UseMethod("energy")
}

# default behavior not defined
#' @rdname energy
#' @export
energy.default <- function(x, ...) {
  stop("default behavior has not been defined for entropy")
}

#' @rdname energy
#' @export
energy.matrix <- function(x, ...) {
  # normalization step
  n_x <- normalize_glcm(x)

  # calculation step
  sum(n_x * n_x)
}

#' @rdname energy
#' @export
energy.FitLandDF <- function(x, nlevels, ...) {
  # get normalized comatrix from fitness landscape
  x_glcm <- get_comatrix(x, discrete = equal_discrete(nlevels))

  # calculate
  energy.matrix(x_glcm)
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
