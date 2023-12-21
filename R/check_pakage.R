check_package = function(package = "ggVennDiagram"){
  if (!requireNamespace(package, quietly = TRUE)){
    stop(paste("The", package, "package is not found in your library paths.",
               "  It is required to show intersections interactively.",
               "  Please run `install.packages('package')` and retry.",
               collapse = "\n"))
  }
}
