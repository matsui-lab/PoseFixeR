#' Normalizing coordinates from OpenPose
#'
#' @export
#'
normalize = function(obj){

  xseq = obj$x
  yseq = obj$y
  pseq = obj$p

  xmeanHip = apply(cbind(xseq[,"Rhip"],xseq[,"Lhip"]),1,mean)
  ymeanHip = apply(cbind(yseq[,"Rhip"],yseq[,"Lhip"]),1,mean)
  HNgap = (cbind(xmeanHip,ymeanHip) - cbind(xseq[,"neck"],yseq[,"neck"]))^2
  realHN_dis = mean(sqrt(apply(HNgap,1,sum)),na.rm = TRUE)
  Xseq = (xseq - xseq[,2])/realHN_dis
  Yseq = (yseq - yseq[,2])/realHN_dis

  output = list(x = Xseq,y=Yseq,p=pseq)

  output
}
