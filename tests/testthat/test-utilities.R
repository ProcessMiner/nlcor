test_that("Segment works", {
  expect_equal(object = Segment(l = 6, s = 0.5),
               expected = list(c(1, 2, 3), c(4, 5, 6)))
})

test_that("FindSegmentSize works", {
  expect_equal(object = FindSegmentSize(l = 100, refine = 0.5),
               expected = 0.05)
})