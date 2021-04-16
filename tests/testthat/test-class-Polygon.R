test_that("discern_overlap(Polygon) works", {
  parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    do.call(ellipse,as.list(c(x,100)))
  })
  polygons <- lapply(ellipses,function(x) sf::st_polygon(list(x)))
  polygon <- Polygon(sets = polygons)

  overlap1 <- discern_overlap(polygon)
  overlap2 <- purrr::reduce(polygon@sets, function(x,y) sf::st_intersection(x,y))
  expect_equal(overlap1,overlap2)
})
