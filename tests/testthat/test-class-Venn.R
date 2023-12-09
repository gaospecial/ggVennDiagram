test_that("Venn methods work", {
  sets <- list(A=1:5,B=3:9,C=c(1,3,5,7,9,11))
  sets <- lapply(sets, as.character)
  venn <- Venn(sets = sets)

  expect_equal(discern_overlap(venn), c("3","5"))
  expect_equal(discern_overlap(venn,1:2), c("4"))
  expect_equal(overlap(venn), c("3","5"))
})
