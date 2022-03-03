#' Importing json files in a batch
#'
#' @param myfiles vector of filenames.
#' @param parts
#'
#' @export
#'
import_json = function(files,parts =
                         c("nose","neck","Rshoulder","Relbow","Rwrist","Lshoulder",
                                         "Lelbow","Lwrist","Rhip","Rknee","Rankle","Lhip",
                                         "Lknee","Lankle","Reye","Leye","Rear","Lear")){
  n_file = length(files)
  f_df = vector("list",n_file)
  for(i in 1:n_file){
    output = rjson::fromJSON(file = files[i])
    f_df[[i]] = matrix(output,nrow=18,ncol=3,byrow=T)
    rownames(f_df[[i]]) = parts
    colnames(f_df[[i]])=c("X","Y","P")
  }
  f_df
}
