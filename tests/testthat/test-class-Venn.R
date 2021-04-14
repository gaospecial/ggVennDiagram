test_that("discern_overlap(Venn) works", {
  sets <- list(A=1:5,B=3:9,C=c(1,3,5,7,9,11))
  sets <- lapply(sets, as.character)
  venn <- RVenn::Venn(sets = sets)

  expect_equal( discern_overlap(venn), c("3","5"))

})
