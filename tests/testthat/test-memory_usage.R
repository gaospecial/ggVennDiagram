test_that("memory usage for large list", {

  large_list <- list(Score = as.character(rnorm(130000, 80, 20)),
                     Length = as.character(rnorm(6000, 200, 50)),
                     Charge = as.character(rnorm(90000, 2000, 800)),
                     Math = as.character(rnorm(100000,90,20)),
                     Total = as.character(rnorm(10000,100,30)))
  expect_s3_class(ggVennDiagram(large_list), "ggplot")
})
