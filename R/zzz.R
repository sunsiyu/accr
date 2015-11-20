#' @useDynLib accr
#' @importFrom Rcpp sourceCpp
#' @import actbase
NULL

.onUnload <- function (libpath){
  library.dynam.unload("accr", libpath)
}
