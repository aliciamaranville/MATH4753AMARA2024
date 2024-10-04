test_that("mu, sigma, and prob are accurate", {
  expect_equal(myncurve(mu=10,sigma=5,a=6)[[1]], 10)
  expect_equal(myncurve(mu=10,sigma=5,a=6)[[2]], 5)
  expect_equal(myncurve(mu=10,sigma=5,a=6)[[3]], 0.2119)
})
rm
