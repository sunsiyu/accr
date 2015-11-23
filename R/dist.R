#' Calculate Euclidean distance between two signals
#'
#' @param raw1,raw2 Rawdata objects
#' @param index1,index2 Starting index for raw1 and raw2
#' @param length length to be calculated for both raw1 and raw2
#' @param chunksize divide computation to chunks
#' @return a numeric value, euclidean distance
#' @examples
#' d <- dist_r(r1, r2, 1, 1, 1000)
#' @author Siyu Sun
#' @export
dist_r <- function(raw1, raw2, index1=1, index2=1,
                   length=min(nrow(raw1), nrow(raw2))-1,
                   chunksize = 100000)
{
  stopifnot(inherits(raw1, "Rawdata"))
  stopifnot(inherits(raw2, "Rawdata"))
  if (index1 < 1 || index2 < 1 || index1 > nrow(raw1) || index2 > nrow(raw2) ||
      length > nrow(raw1)-index1+1 || length > nrow(raw2)-index2+1 || length < 1)
    stop("Range error: index1, index2 and/or length out of range")

  nchunk <- length / chunksize + 1

  d <- vector("list", 3)
  for (i in 1:nchunk) {
    # determine size to read
    toRead <- min(chunksize, length - (i-1)*chunksize)
    i1 <- index1 + (i-1)*chunksize
    i2 <- index2 + (i-1)*chunksize

    # in this case, 3 signals to compare
    for (j in 1:3) {
      a1 <- raw1[i1:(i1+toRead), j]
      a2 <- raw2[i2:(i2+toRead), j]
      m <- matrix(c(a1, a2), nrow = 2, byrow = T)
      d[[j]] <- c(d[[j]], as.numeric(stats::dist(m, method="euclidean")))
    }
  }

  return(lapply(d, sum))
}
