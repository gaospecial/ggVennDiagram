test_that("multiple implements",{
  n.sides <- 100
  # ellipse
  parameters <- list(
    c(0.35, 0.47, 0.35, 0.20, 135),
    c(0.50, 0.57, 0.35, 0.15, 135),
    c(0.50, 0.57, 0.33, 0.15,  45),
    c(0.65, 0.47, 0.35, 0.20,  45)
  )

  library(sf)
  library(tidyverse)

  ellipses <- lapply(parameters, function(x) {
    do.call(ell2poly, as.list(c(x, n.sides))) %>%
      data.frame() %>%
      mutate(x = round(x, 6), y = round(y, 6))
  })
  polygons <-
    lapply(ellipses, function(x)
      st_polygon(list(as.matrix(x))))

  # regions
  A <-  st_difference(st_difference(st_difference(polygons[[1]], polygons[[2]]), polygons[[3]]), polygons[[4]])
  B <-  st_difference(st_difference(st_difference(polygons[[2]], polygons[[1]]), polygons[[3]]), polygons[[4]])
  C <-  st_difference(st_difference(st_difference(polygons[[3]], polygons[[1]]), polygons[[2]]), polygons[[4]])
  D <-  st_difference(st_difference(st_difference(polygons[[4]], polygons[[1]]), polygons[[3]]), polygons[[2]])
  AB <-  st_difference(st_intersection(polygons[[1]], polygons[[2]]),
                       st_union(polygons[[3]], polygons[[4]]))
  AC <-  st_difference(st_intersection(polygons[[1]], polygons[[3]]),
                       st_union(polygons[[2]], polygons[[4]]))
  AD <-  st_difference(st_intersection(polygons[[1]], polygons[[4]]),
                       st_union(polygons[[3]], polygons[[2]]))
  BC <-  st_difference(st_intersection(polygons[[3]], polygons[[2]]),
                       st_union(polygons[[1]], polygons[[4]]))
  BD <-  st_difference(st_intersection(polygons[[4]], polygons[[2]]),
                       st_union(polygons[[3]], polygons[[1]]))
  CD <-  st_difference(st_intersection(polygons[[3]], polygons[[4]]),
                       st_union(polygons[[1]], polygons[[2]]))
  ABC <-  st_difference(st_intersection(st_intersection(polygons[[1]], polygons[[2]]), polygons[[3]]), polygons[[4]])
  ABD <-  st_difference(st_intersection(st_intersection(polygons[[1]], polygons[[2]]), polygons[[4]]), polygons[[3]])
  ACD <-  st_difference(st_intersection(st_intersection(polygons[[1]], polygons[[4]]), polygons[[3]]), polygons[[2]])
  BCD <-  st_difference(st_intersection(st_intersection(polygons[[4]], polygons[[2]]), polygons[[3]]), polygons[[1]])
  ABCD <-  st_intersection(st_intersection(st_intersection(polygons[[1]], polygons[[2]]), polygons[[3]]), polygons[[4]])


  A1 <- st_multi_difference(polygons[[1]], polygons[[2]], polygons[[3]], polygons[[4]])
  B1 <- st_multi_difference(polygons[[2]], polygons[[1]], polygons[[3]], polygons[[4]])
  C1 <-  st_multi_difference(polygons[[3]], polygons[[1]], polygons[[2]], polygons[[4]])
  D1 <-  st_multi_difference(polygons[[4]], polygons[[1]], polygons[[3]], polygons[[2]])
  AB1 <-  st_difference(st_intersection(polygons[[1]], polygons[[2]]),
                        st_union(polygons[[3]], polygons[[4]]))
  AC1 <-  st_difference(st_intersection(polygons[[1]], polygons[[3]]),
                        st_union(polygons[[2]], polygons[[4]]))
  AD1 <-  st_difference(st_intersection(polygons[[1]], polygons[[4]]),
                        st_union(polygons[[3]], polygons[[2]]))
  BC1 <-  st_difference(st_intersection(polygons[[3]], polygons[[2]]),
                        st_union(polygons[[1]], polygons[[4]]))
  BD1 <-  st_difference(st_intersection(polygons[[4]], polygons[[2]]),
                        st_union(polygons[[3]], polygons[[1]]))
  CD1 <-  st_difference(st_intersection(polygons[[3]], polygons[[4]]),
                        st_union(polygons[[1]], polygons[[2]]))
  ABC1 <-  st_difference(st_multi_intersection(polygons[[1]], polygons[[2]], polygons[[3]]), polygons[[4]])
  ABD1 <-  st_difference(st_multi_intersection(polygons[[1]], polygons[[2]], polygons[[4]]), polygons[[3]])
  ACD1 <-  st_difference(st_multi_intersection(polygons[[1]], polygons[[4]], polygons[[3]]), polygons[[2]])
  BCD1 <-  st_difference(st_multi_intersection(polygons[[4]], polygons[[2]], polygons[[3]]), polygons[[1]])
  ABCD1 <-  st_multi_intersection(polygons[[1]], polygons[[2]], polygons[[3]], polygons[[4]])

  expect_identical(A,A1)
  expect_identical(B,B1)
  expect_identical(C,C1)
  expect_identical(D,D1)
  expect_identical(AB,AB1)
  expect_identical(AC,AC1)
  expect_identical(AD,AD1)
  expect_identical(BC,BC1)
  expect_identical(BD,BD1)
  expect_identical(CD,CD1)
  expect_identical(ABC,ABC1)
  expect_identical(ABD,ABD1)
  expect_identical(ACD,ACD1)
  expect_identical(BCD,BCD1)
  expect_identical(ABCD,ABCD1)
})
