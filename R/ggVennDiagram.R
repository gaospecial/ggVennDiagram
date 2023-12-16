#' `ggVennDiagram`: an easy to use Venn diagram generator
#'
#' Venn diagram is frequently used in scientific studies of many fields.
#' This package incorporates state-of-art Venn plot tools and provides a set of
#' easy-to-use functions to plot Venn. By dealing with a user-provided list,
#' which contains the sets of Venn, `ggVennDiagram` returns a structured data that
#' can be used to plot Venn. The data contains three slots: 1) the edge of Venn sets;
#' 2) the separated regions of Venn sets; 3) the labels of Venn sets.
#' By help from the package `venn`, it is possible to draw Venn diagram up to 7 sets.
#'
#' @docType package
#' @name ggVennDiagram-package
NULL

#' shapes: shape data used to setup Venn plot
#'
#' a collection of geometric shapes, which defined the edge and label of sets in a Venn plot.
#' use `plot_shapes()` to see some of them.
#'
#' @format a tibble with 6 columns
#'
#'  * `nsets`: number of sets, from 1-7.
#'  * `type`: ellipse, circle or triangle
#'  - `shape_id`: to separate different shapes
#'  - `component`: each shape has two components, 'setEdge' and 'setLabel'
#'  - `id`: to separate edges/labels of a shape. For example, 4 sets shape will have ids of 1-4.
#'  - `xy`: coordinates
#'
#' @source
#' - `venn:::sets`
#' - `library(VennDiagram)`
#' - [Wiki](https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg)
#' @name vennplot-shapes
NULL

#' ggVennDiagram main parser
#'
#' @details
#' From version 1.4.4, `ggVennDiagram` will plot a upset plot when the number of sets
#' is more than 7. Besides, user can switch to a upset plot with `upset_plot()` function.
#' Please check the document of this function.
#'
#'
#' @param x list of items
#' @param category.names default is names(x)
#' @param show_intersect if TRUE the text can be visualized by `plotly`
#' @param set_color color of set labels ("black")
#' @param set_size size of set labels (NA)
#' @param label format of region labels, select one from c("count","percent","both","none")
#' @param label_geom layer of region labels, choose from c("label", "text")
#' @param label_alpha set 0 to remove the background of region labels
#' @param label_color color of region labels ("black")
#' @param label_size size of region labels (NA)
#' @param label_percent_digit number of digits when formatting percent label (0)
#' @param label_txtWidth width of text used in showing intersect members, will be ignored unless show_intersection is TRUE (40)
#' @param edge_lty line type of set edges ("solid")
#' @param edge_size line width of set edges (1)
#' @param ... useless
#'
#' @return A ggplot object
#' @export
#' @examples
#' library(ggVennDiagram)
#' x = list(A=1:5,B=2:7,C=3:6,D=4:9)
#' ggVennDiagram(x)  # 4d venn
#' ggVennDiagram(x[1:3])  # 3d venn
#' ggVennDiagram(x[1:2])  # 2d venn
ggVennDiagram = function(x,
                          category.names = names(x),
                          show_intersect = FALSE,
                          set_color = "black",
                          set_size = NA,
                          label = c("both","count","percent","none"),
                          label_alpha = 0.5,
                          label_geom = c("label","text"),
                          label_color = "black",
                          label_size = NA,
                          label_percent_digit = 0,
                          label_txtWidth = 40,
                          edge_lty = "solid",
                          edge_size = 1,
                          ...){
  if (!is.list(x)){
    stop(simpleError("ggVennDiagram() requires at least a list."))
  }
  names(x) = category.names
  dimension = length(x)
  venn = Venn(x)

  label = match.arg(label)
  label_geom = match.arg(label_geom)
  if (dimension <= 7){
    data = process_data(venn)
    plot_venn(data,
              show_intersect = show_intersect,
              set_color = set_color,
              set_size = set_size,
              label = label,
              label_alpha=label_alpha,
              label_geom = label_geom,
              label_color = label_color,
              label_size = label_size,
              label_percent_digit = label_percent_digit,
              label_txtWidth = label_txtWidth,
              edge_lty = edge_lty,
              edge_size = edge_size,
              ...)
  }
  else{
    warning("Only support 2-7 dimension Venn diagram. Will give a plain upset plot instead.")
    upset_plot(venn, nintersects = 30, order.intersect.by = "size", order.set.by = "name")
  }
}




#' plot codes
#'
#' @inheritParams ggVennDiagram
#' @param data plot data
#'
#' @import ggplot2
#' @export
#'
#' @return ggplot object, or plotly object if show_intersect is TRUE
plot_venn = function(data,
                     show_intersect = FALSE,
                     set_color = "black",
                     set_size = NA,
                     label = "both",
                     label_geom = "label",
                     label_alpha = 0.5,
                     label_color = "black",
                     label_size = NA,
                     label_percent_digit = 0,
                     label_txtWidth = 40,
                     edge_lty = "solid",
                     edge_size = 1,
                     ...){
  p = ggplot(mapping = aes(.data$X, .data$Y))
  setedge.params = list(data = get_shape_setedge(data),
                         mapping = aes(color = .data$id,
                                       group = .data$id),
                         linetype = edge_lty,
                         linewidth = edge_size,
                         # color = set_color,
                         show.legend = FALSE)
  setlabel.params = list(data = get_shape_setlabel(data),
                          mapping = aes(label = .data$name),
                          size = set_size,
                          color = set_color)
  region.params = list(data = get_shape_regionedge(data) |> dplyr::left_join(venn_region(data), by = "id"),
                        mapping = aes(fill = .data$count,
                                      group = .data$id))

  setedge.layer = do.call('geom_path', setedge.params)
  setlabel.layer = do.call('geom_text', setlabel.params)
  region.layer = do.call('geom_polygon', region.params)

  p = p + region.layer + setedge.layer + setlabel.layer + theme_void() + coord_equal()

  if (label == "none"){
    return(p)
  }

  # process data for plotting region labels
  region_label = get_shape_regionlabel(data)

  # use plotly to show intersect
  if (show_intersect){
    check_plotly()
    region_label = region_label |>
      dplyr::rowwise() |>
      dplyr::mutate(item = str_wrap(paste0(.data$item, collapse = " "),
                                                 width = label_txtWidth))
    p = p + geom_text(aes(label = .data$count, text = .data$item),
                      data = region_label) +
      theme(legend.position = "none")
    ax = list(
      showline = FALSE
    )
    p = plotly::ggplotly(p, tooltip = c("text")) |>
      plotly::layout(xaxis = ax, yaxis = ax)
    return(p)
  }

  # calculate labels, which are 'percent', 'count', or 'both'
  region_label = region_label |>
    dplyr::mutate(percent = paste(round(.data$count*100/sum(.data$count),
                                        digits = label_percent_digit),"%", sep=""),
                  both = paste(.data$count,paste0("(",.data$percent,")"),sep = "\n"))

  # if label != "none" & show_intersect == FALSE
  if (label_geom == "label"){
    p = p + geom_label(
      aes(label = .data[[label]]),
      data = region_label,
      alpha = label_alpha,
      color = label_color,
      size = label_size,
      lineheight = 0.85,
      label.size = NA
    )
    return(p)
  }

  if (label_geom == "text"){
    p = p + geom_text(
      aes(label = .data[[label]]),
      data = region_label,
      alpha = label_alpha,
      color = label_color,
      size = label_size,
      lineheight = 0.85
    )
    return(p)
  }


}


check_plotly = function(){
  if (!requireNamespace("plotly", quietly = TRUE)){
    stop(paste("The plotly package is not found in your library paths.",
               "  It is required to show intersections interactively.",
               "  Please run `install.packages('plotly')` and retry.",
               collapse = "\n"))
  }
}


# from yulab.utils::str_wrap
str_wrap = function (string, width = getOption("width")){
  result <- vapply(string, FUN = function(st) {
    words <- list()
    i <- 1
    while (nchar(st) > width) {
      if (length(grep(" ", st)) == 0)
        break
      y <- gregexpr(" ", st)[[1]]
      n <- nchar(st)
      y <- c(y, n)
      idx <- which(y < width)
      if (length(idx) == 0)
        idx <- 1
      words[[i]] <- substring(st, 1, y[idx[length(idx)]] -
                                1)
      st <- substring(st, y[idx[length(idx)]] + 1, n)
      i <- i + 1
    }
    words[[i]] <- st
    paste0(unlist(words), collapse = "\n")
  }, FUN.VALUE = character(1))
  names(result) <- NULL
  result
}
