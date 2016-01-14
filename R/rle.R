#' Get Run-length Encoding (RLE) of a parameter
#'
#' Get the Run-length Encoding (RLE) of a parameter from a single data frame or
#' matrix
#'
#' @param m the parameter
#' @param nrow1 start index
#' @param nrow2 end index
#' @param chunksize default 1000000 to avoid over memory use
#' @return a data frame contain 4 columns:
#'    - m the parameter
#'    - start start time index
#'    - end end time index
#'    - secs duration in unit of secs
#' @export
readkss <- function(m, nrow1 = 1, nrow2 = nrow(m), timeindex = TRUE,
                    chunksize = 100000)
{

  if (nrow1 < 1 || nrow1 > nrow2 || nrow2 > nrow(m)) {
    stop("Range error: nrow1 and/or nrow2 out of range")
  }

  n <- max(1, ceiling((nrow2 - nrow1) / chunksize))
  lrle <- vector("list", n)

  for (i in 1:n) {
    i1 <- nrow1 + (i-1) * chunksize
    toRead <- min(nrow2-i1, chunksize)
    lrle[[i]] <- data.frame(unclass(rle(m[i1:(i1+toRead-1), 4])))
  }

  dfrle <- do.call(rbind, lrle)

  if (nrow(dfrle) > 1) {
    dfrle$lengths <- cumsum(dfrle$lengths)
    tmprle <- data.frame(unclass(rle(dfrle$values)))
    index <- unique(c(1, cumsum(tmprle$lengths)))

    start <- dfrle$lengths[index[-length(index)]] + 1
    end <- dfrle$lengths[index[-1]]
  } else {
    start <- 1
    end <- dfrle$lengths
    tmprle <- dfrle
  }

  if (timeindex) {
    begin <- POSIX.fromIndex(nrow1, m@start, 100)
    start <- POSIX.fromIndex(start, begin, 100)
    end <- POSIX.fromIndex(end, begin, 100)
  }

  result <- data.frame(kss = tmprle$values,
                       start = start,
                       end = end)
  return(result)
}
