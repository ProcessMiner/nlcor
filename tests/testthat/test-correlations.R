test_that("NetCor works", {
  expect_equal(object = NetCor(cors = c(-0.70, 0.93, -0.79, 0.91), pvalues = c(0.004, 0.6, 0.0007, 0.009)),
               expected = list(cor.estimate = 0.6, adjusted.p.value = 0.0136549252,
                               segment.cor = list(cor = c(-0.70, 0, -0.79, 0.91),
                                                                      p.value = c(4e-03, NA, 7e-04, 9e-03))))
})

test_that("SampleCor works", {
  expect_equal(object = SampleCor(x, y, s = 0.5),
               expected = list(cor = c(-0.6326391, -0.6362939), pvalue = c(0.0009087227, 0.0008303427)),
               tolerance = 1e-5)
})

test_that("nlcor works", {
  expect_equal(object = nlcor(x, y, plt = F, refine = 0.0),
               expected = list(cor.estimate = 0.8397742,
                               adjusted.p.value = 0.01470883),
               tolerance = 1e-5
  )
})
