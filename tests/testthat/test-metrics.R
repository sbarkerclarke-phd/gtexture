test_that("energy metric works", {
  # create fitness landscape using FitLandDF object
  vals <- runif(64)
  vals <- array(vals, dim = rep(4, 3))
  my_landscape <- fitscape::FitLandDF(vals)

  # calculate energy of fitness landscape, assuming 2 discrete gray levels
  a <- energy(my_landscape, nlevels = 2)

  # extract normalized GLCM from fitness landscape
  my_glcm <- get_comatrix(my_landscape, discrete = equal_discrete(2))

  # calculate energy of extracted GLCM
  b <- energy(my_glcm)

  # test
  expect_equal(a, b)
})
