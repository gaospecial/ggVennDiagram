test_that("upset plot works", {
  list = list(A = LETTERS[1:10],
              B = LETTERS[8:15],
              C = LETTERS[10:13])
  venn = Venn(list)
  data = process_upset_data(venn,
                            order.intersect.by = "size",
                            order.set.by = "name")
  expect_equal(data$top_data$name, c("A","B","C","A/B","A/C","B/C","A/B/C"))
  expect_equal(data$top_data$size, c(7L,2L,0L,2L,0L,3L,1L))
  expect_equal(data$top_data$item[[1]], LETTERS[1:7])
  p = plot_upset(venn, order.intersect.by = "name")
  expect_s3_class(p, c("upset_plot","aplot"))
})
