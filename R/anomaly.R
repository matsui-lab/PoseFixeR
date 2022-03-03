#' Internal function
#'
#' @export
#'
anomaly <- function(x, k=2) {

  result <- NULL
  result$caselist <- x >= mean(x)+k*stats::sd(x) | x <= mean(x)-k*stats::sd(x)
  result$abnormal.values <- sort(x[result$caselist])
  result
}
