#' Title Plotting frames of gait cycle
#'
#' @param obj list for reshaped object
#' @param layout vector indicating layout for display
#' @param xlim vector of 2 elements
#' @param ylim vector of 2 elements
#'
#' @return null
#' @export
#'
#'

diplay_frames = function(obj,layout = c(5,5),xlim = c(450,800), ylim = c(700,200)){
  n_frame = nrow(obj$x)
  graphics::par(mfrow=layout)
  for(j in 1:n_frame){
    plot(obj$x[j,],obj$y[j,],xlim = xlim,ylim = ylim,lwd = 2,main = paste0("T=",j))
    graphics::axis(side=1, tck=1.0, lty="dotted")
    graphics::axis(side=2, tck=1.0, lty="dotted")
    plt.segment(obj$x[j,], obj$y[j,])
  }
}
