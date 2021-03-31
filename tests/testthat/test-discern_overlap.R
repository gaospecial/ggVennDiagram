test_that("multiplication works", {
  x <- list(A=1:3,B=3:7,C=2:9)
  venn <- RVenn::Venn(x)
  expect_equal(discern_overlap(venn,1:3), RVenn::overlap(venn))
})
