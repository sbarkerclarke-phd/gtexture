#####MANHATTAN DISTANCE#####
# Manhattan distance function factory (checks Manhattan distance < x)
manhattan <- function(dist = 1) {
  function(x, y) {
    stopifnot(length(x) == length(y))

    sum(abs(x - y)) <= dist
  }
}

#####EUCLIDEAN DISTANCE#####
# Euclidean distance function factory (checks Euclidean distance < x)
euclidean <- function(dist = 1) {
  function(x, y) {
    stopifnot(length(x) == length(y))

    sqrt(sum((x - y) ^ 2)) <= dist
  }
}
