test_that("multiplication works", {

  large_list <- list(Score = rnorm(130000, 80, 20),
                     Length = rnorm(6000, 200, 50),
                     Charge = rnorm(90000, 2000, 800))
  expect_s3_class(ggVennDiagram(large_list), "ggplot")
})
