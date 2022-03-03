#' Reshaping data
#'
#' @param obj
#' @param norm
#'
#' @export
#'
#'
reshaping = function(obj,norm = TRUE){

  realHN_dis = rep(NA,length(obj))
  xseq = yseq = pseq = xseq1 = yseq1 = Xseq = Yseq = matrix(NA,nrow=length(obj),ncol = nrow(obj[[1]]))
  colnames(xseq) = colnames(yseq) = colnames(pseq) = colnames(xseq) = colnames(yseq1) = colnames(Xseq) = colnames(Yseq) = rownames(obj[[1]])
  for(j in seq_along(obj)){
    sbj_ij = as.matrix(obj[[j]])
    xseq[j,] = sbj_ij[,1]
    yseq[j,] = sbj_ij[,2]
    pseq[j,] = sbj_ij[,3]
  }

  output = list(x = xseq, y = yseq, p = pseq)

  return(output)
}
