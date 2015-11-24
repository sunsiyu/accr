context("Distance Functions")


test_that("dist_r assertions work", {
  expect_error(dist_r("a"))
  expect_error(dist_r(1:10, 1:10, n1=1, n2=1, len=9),
                      chn = 1, chunksize = 100000)
  expect_error(dist_r())
})


test_that("dist_r computation works", {
  expect_equal(dist_r(matrix(rep(0,3), ncol=3), matrix(rep(0,3), ncol=3)),
               matrix(c(0,0,0), ncol=3))
  expect_equal(dist_r(matrix(rep(0,3), ncol=3), matrix(rep(1,3), ncol=3)),
               matrix(c(1,1,1), ncol=3))
})
