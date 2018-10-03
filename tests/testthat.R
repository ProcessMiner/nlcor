library(testthat)
library(nlcor)

test_check("nlcor")

x <- c(30.7290066, 11.2860382, 33.7056647, 34.3473855, -0.6980050, 22.4137868, 16.0426999, 25.4439074, 43.2518890, 8.3392084, 21.9938673, 37.3683793, 2.8293545, 19.2157110, 6.1003547, 23.6189830, 6.8549076, 20.0403587, 43.8055397, 32.3269139, 8.5946769, 16.4174388, 3.6225708, 2.9808826, 13.9428501, -3.1371748, 25.0253936, 44.5617052, 14.7200310, 29.2489491, 32.0497153, 34.9044378, 0.4742675, 3.8238914, 25.3457852, 26.1047538, 9.0517013, 36.7013138, -0.2440629, 36.7822171, 37.0541294, 12.4842335, 41.2709037, 19.7750777, 11.6303973, 40.8340831, 19.6355959, 40.7770828)
y <- c(5.647746, 0.514578, 5.636446, 2.290837, 5.260928, 4.864901, 0.285289, 5.546851, 3.491522, 4.450483, 5.187638, 1.041484, 5.552126, 3.776473, 5.509269, 5.447783, 4.760743, 4.531908, 3.536448, 1.817413, 4.022091, 1.838514, 5.846154, 5.862163, 0.535624, 5.122967, 5.728909, 4.297905, 0.634426, 5.816508, 5.244371, 3.639807, 5.506081, 5.539262, 5.418379, 5.843574, 2.846292, 0.519035, 5.746830, 2.590377, 0.699672, 0.697708, 2.829572, 4.102792, 0.481853, 1.032161, 2.223832, 1.656842)
###### Testing utilities::Segment() ######
expect_equal(object = Segment(l = 7, s = 0.5),
             expected = list(c(1, 2, 3), c(4, 5, 6), c(7)))

expect_equal(object = Segment(l = 6, s = 0.5),
             expected = list(c(1, 2, 3), c(4, 5, 6)))

###### Testing utilities::FindSegmentSize() ######
expect_equal(object = FindSegmentSize(l = 10),
             expected = 0.55)

expect_equal(object = FindSegmentSize(l = 100),
             expected = 0.05)

###### Testing correlations::NetCor() ######
expect_equal(object = NetCor(cors = c(-0.70, 0.93, -0.79, 0.91), pvalues = c(0.004, 0.0006, 0.0007, 0.009)),
             expected = list(cor.estimate = 0.8325, segment.cor = list(cor = c(-0.70, 0.93, -0.79, 0.91),
                                                                       p.value = c(4e-03, 6e-04, 7e-04, 9e-03))))

expect_equal(object = NetCor(cors = c(-0.70, 0.93, -0.79, 0.91), pvalues = c(0.004, 0.6, 0.0007, 0.009)),
             expected = list(cor.estimate = 0.6, segment.cor = list(cor = c(-0.70, 0, -0.79, 0.91),
                                                                       p.value = c(4e-03, NA, 7e-04, 9e-03))))

###### Testing correlations::SampleCor() ######
expect_equal(object = SampleCor(x, y, s = 0.5),
             expected = list(cor = c(-0.6326391, -0.6362939), pvalue = c(0.0009087227, 0.0008303427)),
             tolerance = 1e-5)

expect_equal(object = SampleCor(x, y, s = 0.3),
             expected = list(cor = c(-0.7090867, 0.9390392, -0.7916622, 0.9193088),
                             pvalue = c(4.516445e-03, 6.491941e-07, 7.411246e-04, 9.503901e-03)),
             tolerance = 1e-5)


expect_equal(object = nlcor(x, y),
             expected = list(cor.estimate = 0.8397742,
                             segments = list(segment.cor = list(cor = c(-0.7090867,
                                                                        0.9390392,
                                                                        -0.7916622,
                                                                        0.9193088),
                                                                p.value = c(4.516445e-03,
                                                                           6.491941e-07,
                                                                           7.411246e-04,
                                                                           9.503901e-03)),
                                             segment.size = 0.3)
                             ),
             tolerance = 1e-5
             )
