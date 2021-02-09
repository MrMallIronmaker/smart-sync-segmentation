
#' Smart segment
#' 
#' Given a number of time-aligned vectors, choose a vector for each time such
#'  that the sum of the values is maximized and each run is of length at least 
#'  `streak`.
#'
#' @param ... Column vectors to choose between
#' @param streak Minimum length of each run.
#' @return Vector of the same length as any in `...` that indexes into the given vectors list
#' @export 
smart_segment <- function(..., streak) {
  smart_segment_cpp(cbind(...), streak)
}
