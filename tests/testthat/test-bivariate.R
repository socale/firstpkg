context("Bivariate gaussian test")

test_that("correct results for bivariate gaussian", {
  expect_equal(mvnpdf(x=matrix(rep(1.96,2), nrow=2, ncol=1), Log=FALSE)$y,
               mvtnorm::dmvnorm(rep(1.96, 2)))
})
