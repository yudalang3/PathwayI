# test_that("simple case", {
#   x <- 1:3
#   y <- x ^ (1 / 3) + 0.5
#
#   xyPrime <- do_bilateral_extension_alongCurve(x, y, d = 1)
#
#   act <-
#     (xyPrime$yPrime_1 - y[1:2]) ^ 2 + (xyPrime$xPrime_1 - x[1:2]) ^ 2
#   exp <- rep(1, 2)
#
#   expect_equal(act, exp)
# })
#
# test_that("simple case2", {
#   x <- 1:3
#   y <- x + 0.5
#
#   xyPrime <- do_bilateral_extension_alongCurve(x, y, d = 1)
#
#   act <-
#     (xyPrime$yPrime_1 - y[1:2]) ^ 2 + (xyPrime$xPrime_1 - x[1:2]) ^ 2
#   exp <- rep(1, 2)
#
#   expect_equal(act, exp)
# })


test_that("simple case3", {

  x <- 1:2
  y <- x^ (1/3) + 0.5

  xyPrime <- do_bilateral_extension_alongCurve(x,y,d = 1)

  act <- (xyPrime$yPrime_1 - y[1:1])^2 + (xyPrime$xPrime_1 - x[1:1])^2
  exp <- rep(1,1)

  expect_equal(act,exp)
})
