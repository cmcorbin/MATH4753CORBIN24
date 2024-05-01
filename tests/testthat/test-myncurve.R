test_that("mu works", {
  x = myncurve(3, 5, 2)
  expect_equal(x$mu, 5)
})

test_that("sigma works", {
  x = myncurve(3, 5, 2)
  expect_equal(x$sigma, 2)
})

test_that("area works", {
  x = myncurve(3, 5, 2)
  expect_equal(x$area, 0.1587)
})
