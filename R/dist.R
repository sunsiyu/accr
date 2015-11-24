#' Calculate Euclidean distance between two large matrices
#'
#' @param m1,m2 matrix objects or objects of matrix shape
#' @param n1,n2 Starting index for m1 and m2
#' @param len len to be calculated for both m1 and m2
#' @param chunksize divide computation to chunks
#' @return a numeric value, euclidean distance
#' @examples
#' d <- dist_r(r1, r2, 1, 1, 1000)
#' @author Siyu Sun
#' @export
dist_r <- function(m1, m2, n1=1, n2=1, len=max(0, min(nrow(m1)-n1, nrow(m2)-n2)),
                   chn = min(ncol(m1), ncol(m2)), chunksize = 100000)
{
  # TODO: in case of absence of m2, should calculate dist within m1
  stopifnot(inherits(m1[1:nrow(m1), ], "matrix") || inherits(m1, "matrix") && is.numeric(m1))
  stopifnot(inherits(m2[1:nrow(m2), ], "matrix") || inherits(m2, "matrix") && is.numeric(m2))
  if (n1 < 1 || n2 < 1 || n1 > nrow(m1) || n2 > nrow(m2) ||
      len > nrow(m1)-n1+1 || len > nrow(m2)-n2+1 || len < 0)
    stop("Range error: n1, n2 and/or len out of range")

  chn <- ifelse(is.null(chn), 1, chn)
  nchunk <- max(1, ceiling(len / chunksize))
  d <- vector("list", nchunk)

  for (i in 1:nchunk) {
    # determine size to read
    toRead <- min(chunksize, len - (i-1)*chunksize)
    i1 <- n1 + (i-1)*chunksize
    i2 <- n2 + (i-1)*chunksize

    # need to add drop=F!
    a1 <- m1[i1:(i1+toRead), 1:chn, drop=F]
    a2 <- m2[i2:(i2+toRead), 1:chn, drop=F]

    d[[i]] <- (a1 - a2)^2
  }

  res <- sqrt(colSums(do.call(rbind, d)))
  return(res)
}
