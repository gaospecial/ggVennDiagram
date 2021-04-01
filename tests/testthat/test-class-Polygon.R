test_that("multiplication works", {
  parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ell2poly <- VennDiagram::ell2poly
  ellipses <- lapply(parameters,function(x){
    do.call("ell2poly",as.list(c(x,1000))) %>%
      data.frame() %>%
      mutate(x=round(.data$x,6),y=round(.data$y,6))
  })
  polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))
  polygon <- Polygon(sets = polygons)

  overlap1 <- discern_overlap(polygon)
  overlap2 <- purrr::reduce(polygon@sets, function(x,y) sf::st_intersection(x,y))
  expect_equal(overlap1,overlap2)
})
