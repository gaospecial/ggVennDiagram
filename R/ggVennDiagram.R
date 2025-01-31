

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
#' @param label_font font name of labels
#' @param label_bigMark Type of thousand separator
#' @param label_bigInterval Position of thousand separator
#' @param label_percent_digit number of digits when formatting percent label (0)
#' @param label_txtWidth width of text used in showing intersect members, will be ignored unless show_intersection is TRUE (40)
#' @param edge_lty line type of set edges ("solid")
#' @param edge_size line width of set edges (1)
#' @param force_upset if TRUE, will always produce Upset plot no matter how many sets have (FALSE)
#' @param shape_id specify a shape by id, run `plot_shapes()` to see available shapes (NULL)
#' @inheritParams upset-plot
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
                         label_font = "sans",
                         label_bigInterval = 3L,
                         label_bigMark = ",",
                         label_geom = c("label","text"),
                         label_color = "black",
                         label_size = NA,
                         label_percent_digit = 0,
                         label_txtWidth = 40,
                         edge_lty = "solid",
                         edge_size = 1,
                         force_upset = FALSE,
                         nintersects = 20,
                         order.intersect.by = c("size","name","none"),
                         order.set.by = c("size","name","none"),
                         relative_height = 3,
                         relative_width = 0.3,
                         shape_id = NULL,
                          ...){
  if (!is.list(x)){
    stop(simpleError("ggVennDiagram() requires at least a list."))
  }
  names(x) = category.names
  dimension = length(x)
  venn = Venn(x)

  label = match.arg(label)
  label_geom = match.arg(label_geom)
  if (dimension <= 7 & !force_upset){
    data = process_data(venn, shape_id = shape_id)
    plot_venn(data,
              show_intersect = show_intersect,
              set_color = set_color,
              set_size = set_size,
              label = label,
              label_alpha=label_alpha,
              label_font = label_font,
              label_geom = label_geom,
              label_color = label_color,
              label_size = label_size,
              label_bigMark = label_bigMark,
              label_bigInterval = label_bigInterval,
              label_percent_digit = label_percent_digit,
              label_txtWidth = label_txtWidth,
              edge_lty = edge_lty,
              edge_size = edge_size,
              ...)
  }
  else{
    if (!force_upset) warning("Only support 2-7 dimension Venn diagram. Will give a plain upset plot instead.")
    plot_upset(venn,
               nintersects = nintersects,
               order.intersect.by = order.intersect.by,
               order.set.by = order.set.by,
               relative_height = relative_height,
               relative_width = relative_width,
               ...)
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
                     label_font = "sans",
                     label_color = "black",
                     label_size = NA,
                     label_percent_digit = 0,
                     label_bigMark = ",",
                     label_bigInterval = 3L,
                     label_txtWidth = 40,
                     edge_lty = "solid",
                     edge_size = 1,
                     ...){
  setedge.params = list(data = get_shape_setedge(data, color = set_color,
                                                 linetype = edge_lty,
                                                 linewidth = as.numeric(edge_size)),
                         mapping = aes(color = I(.data$color),
                                       group = .data$id,
                                       linetype = I(.data$linetype),
                                       linewidth = I(.data$linewidth)),
                         show.legend = FALSE)
  setlabel.params = list(data = get_shape_setlabel(data, size = as.numeric(set_size), color = set_color),
                          mapping = aes(label = .data$name,
                                        size = I(.data$size),
                                        color = I(.data$color)
                                      ),
                         family = label_font,
                          show.legend = FALSE)
  region.params = list(data = get_shape_regionedge(data) |> dplyr::left_join(venn_region(data), by = "id"),
                        mapping = aes(fill = .data$count,
                                      group = .data$id))

  setedge.layer = do.call('geom_path', setedge.params)
  setlabel.layer = do.call('geom_text', setlabel.params)
  region.layer = do.call('geom_polygon', region.params)

  p = ggplot(mapping = aes(.data$X, .data$Y))
  p_nonlabel = p + region.layer + setedge.layer + setlabel.layer + theme_void() + coord_equal()

  if (label == "none"){
    return(p_nonlabel)
  }

  # process data for plotting region labels
  region_label = get_shape_regionlabel(data)

  # use plotly to show intersect
  if (show_intersect){
    check_package("plotly")
    region_label = region_label |>
      dplyr::rowwise() |>
      dplyr::mutate(item = yulab.utils::str_wrap(paste0(.data$item, collapse = " "),
                                                 width = label_txtWidth))
    p_plotly = p_nonlabel +
      geom_text(aes(label = .data$count, text = .data$item),
                    data = region_label) +
      theme(legend.position = "none")
    ax = list(
      showline = FALSE
    )
    p_plotly = plotly::ggplotly(p_plotly, tooltip = c("text")) |>
      plotly::layout(xaxis = ax, yaxis = ax)
    return(p_plotly)
  }

  # calculate labels, which are 'percent', 'count', or 'both'
  region_label = region_label |>
    dplyr::mutate(percent = paste(round(.data$count*100/sum(.data$count),
                                        digits = label_percent_digit),"%", sep=""),
                  both = paste(format(.data$count, big.mark = label_bigMark, big.interval = label_bigInterval),paste0("(",.data$percent,")"),sep = "\n"))

  # if label != "none" & show_intersect == FALSE
  if (label_geom == "label"){
    p_label = p_nonlabel + geom_label(
      aes(label = .data[[label]]),
      data = region_label,
      alpha = label_alpha,
      family = label_font,
      color = label_color,
      size = label_size,
      lineheight = 0.85,
      label.size = NA
    )
    return(p_label)
  }

  if (label_geom == "text"){
    p_label = p_nonlabel + geom_text(
      aes(label = .data[[label]]),
      data = region_label,
      alpha = label_alpha,
      family = label_font,
      color = label_color,
      size = label_size,
      lineheight = 0.85
    )
    return(p_label)
  }


}
