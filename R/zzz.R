#' @useDynLib accr
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath){
  library.dynam.unload("accr", libpath)
}
