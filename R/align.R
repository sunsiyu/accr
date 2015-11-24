#' Calculate Index for Alignment of Two matrices
#'
#' @param m1,m2 matrix objects or objects of matrix shape, m1 is the matrix to
#'  be aligned, and m2 is the matrix to be aligned to.
#' @param n1,n2 Starting index for m1 and m2
#' @param win the min. window size to be searched for best alignment
#' @param direction direction to look at during alignment
#' @return a matrix, the aligned m1 with same dimensions
#' @examples
#' a_m1 <- align(m1, m2, col=3)
#' @author Siyu Sun
#' @export
align <- function(m1, m2, n1=1, n2=1, col=3, win = 50,
                   direction = c("center", "left", "right")[1])
{
  d <- switch(direction,
              center = lapply(1:win, function(x) dist_r(m1, m2, n1-win/2+x, n2)),
              left = lapply(1:win, function(x) dist_r(m1, m2, n1-win+x, n2)),
              right = lapply(1:win, function(x) dist_r(m1, m2, n1+x, n2)))

  d <- do.call(rbind, d)
  # TODO: cases of multiple peaks instead of the global minimum in search window.
  loc_min <- which(d[, col] == min(d[, col]))

  index <- switch(direction,
                  center = n1 - win/2 + loc_min,
                  left   = n1 - win + loc_min,
                  right  = n1 + loc_min)

  if (abs(index - n1) > 5) {
    m1 <- m1[index:nrow(m1), ]
    m2 <- m2[n2:(n2+nrow(m1)-1), ]
  }
  return(list(m1, m2))
}
