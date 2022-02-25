hazureti<- function(x, k=2) {
  result <- NULL
  result$caselist <- x >= mean(x)+k*sd(x) | x <= mean(x)-k*sd(x)
  result$abnormal.values <- sort(x[result$caselist])
  result
}
