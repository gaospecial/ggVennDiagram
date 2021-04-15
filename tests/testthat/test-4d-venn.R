test_that("plot 4d Venn", {
  genes <- paste("gene",1:1000,sep="")
  set.seed(20210408)
  x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))
  p <- ggVennDiagram(x)
  expect_is(p, "ggplot")
})
