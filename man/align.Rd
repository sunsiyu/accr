% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/align.R
\name{align}
\alias{align}
\title{Calculate Index for Alignment of Two matrices}
\usage{
align(m1, m2, n1 = 1, n2 = 1, col = 3, win = 50,
  direction = c("center", "left", "right")[1])
}
\arguments{
\item{m1,m2}{matrix objects or objects of matrix shape, m1 is the matrix to
be aligned, and m2 is the matrix to be aligned to.}

\item{n1,n2}{Starting index for m1 and m2}

\item{win}{the min. window size to be searched for best alignment}

\item{direction}{direction to look at during alignment}
}
\value{
a matrix, the aligned m1 with same dimensions
}
\description{
Calculate Index for Alignment of Two matrices
}
\examples{
a_m1 <- align(m1, m2, col=3)
}
\author{
Siyu Sun
}

