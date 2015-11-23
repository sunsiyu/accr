#' Calculate Euclidean distance between two signals
#'
#' @param raw1,raw2 Rawdata objects
#' @param index1,index2 Starting index for raw1 and raw2
#' @param length length to be calculated for both raw1 and raw2
#' @return a numeric value, euclidean distance
#' @examples
#' d <- dist_r(r1, r2, 1, 1, 1000)
#' @author Siyu Sun
#' @export
dist_r <- function(raw1, raw2, index1=1, index2=1, length=min(nrow(raw1), nrow(raw2))-1)
{
  stopifnot(inherits(raw1, "Rawdata"))
  stopifnot(inherits(raw2, "Rawdata"))
  if (index1 < 1 || index2 < 1 || index1 > nrow(raw1) || index2 > nrow(raw2) ||
      length > nrow(raw1)-index1+1 || length > nrow(raw2)-index2+1 || length < 1)
    stop("Range error: index1, index2 and/or length out of range")

  n1 <- index1 + length
  n2 <- index2 + length

  d <- vector("numeric", 3)

  for (i in 1:3) {
    a1 <- raw1[index1:n1, i]
    a2 <- raw2[index2:n2, i]
    m <- matrix(c(a1, a2), nrow = 2, byrow = T)
    d[i] <- as.numeric(stats::dist(m, method="euclidean"))
  }

  return(d)
}
