# See Table 1 in the following paper:
#   Lofstedt et al. PLoS ONE. 2019; 14(2): e0212110.

#####CALCULATIONS#####
# get an element from the GLCM
p <- function(mat, i, j) {
  mat[i, j]
}

# sum elements in a given column
p_x <- function(mat, i) {
  sum(mat[i, ])
}

# sum elements in a given row
p_y <- function(mat, j) {
  sum(mat[, j])
}

# expected column-wise marginal value
mu_x <- function(mat) {
  vapply(
    X = seq_len(ncol(mat)),
    USE.NAMES = FALSE,
    FUN.VALUE = numeric(1),
    FUN = function(i) {
      i * p_x(mat, i)
    }
  ) %>%
    sum
}

# expected row-wise marginal value
mu_y <- function(mat) {
  vapply(
    X = seq_len(nrow(mat)),
    USE.NAMES = FALSE,
    FUN.VALUE = numeric(1),
    FUN = function(j) {
      j * p_y(mat, j)
    }
  ) %>%
    sum
}



#####CHECKS#####
# returns true if `mat` is a valid GLCM; false otherwise
check_valid_glcm <- function(mat) {
  if (!is.numeric(mat)) return(FALSE)
  if (sum(is.na(mat)) > 0) return(FALSE)

  return(TRUE)
}

# need to implement
check_valid_rowcol <- function(mat, i, j) {
  return(TRUE)
}
