#' Create segment line for frame plot
#'
#' @param x scalar of x-coordinate
#' @param y scalar of y-coordinate
#' @description
#'
#' @export
#'
plt.segment <- function(x, y){

  col1 = grDevices::rainbow(14,alpha = 0.8)
  graphics::segments(x[2], y[2],x[3], y[3],col=col1[2],lwd = 3)
  graphics::segments(x[2], y[2],x[6], y[6],col=col1[3],lwd = 3)
  graphics::segments(x[2], y[2],x[9], y[9],col=col1[4],lwd = 3)
  graphics::segments(x[2], y[2],x[12], y[12],col=col1[5],lwd = 3)
  graphics::segments(x[3], y[3],x[4], y[4],col=col1[6],lwd = 3)
  graphics::segments(x[4], y[4],x[5], y[5],col=col1[7],lwd = 3)
  graphics::segments(x[6], y[6],x[7], y[7],col=col1[8],lwd = 3)
  graphics::segments(x[7], y[7],x[8], y[8],col=col1[9],lwd = 3)
  graphics::segments(x[9], y[9],x[10], y[10],col=col1[10],lwd = 3)
  graphics::segments(x[10], y[10],x[11], y[11],col=col1[11],lwd = 3)
  graphics::segments(x[12], y[12],x[13], y[13],col=col1[12],lwd = 3)
  graphics::segments(x[13], y[13],x[14], y[14],col=col1[13],lwd = 3)
  graphics::segments(x[9], y[9],x[12], y[12],col=col1[1],lwd = 3)
}
