library(tidyr)
library(shiny)
library(ggVennDiagram)
# devtools::load_all(path = rprojroot::find_root("DESCRIPTION"))
library(ggplot2)
library(bslib)
library(colourpicker)
library(export)

# SHINY UI ------------------------------------------------------------------

ui = page_sidebar(
  theme = bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinyApp.css")
  ),
  title = "ggVennDiagram Shiny App",
  sidebar = sidebar(
    width = "30%",
    # Set number
    sliderInput(
      inputId = 'nsets',
      label = "Number of Sets: ",
      value = 3,
      min = 2,
      max = 8,
      step = 1
    ),

    p("Set name and members:"),

    # dynamic inputs
    uiOutput("text_inputs"),

    accordion(
      open = FALSE,
      accordion_panel(
        "Label Controls",
        numericInput("set_size", "size of set label", 5, min = 0, max = 10, step = 1),
        selectInput("label", "mode",c("both", "count", "percent", "none"), selected = "both"),
        selectInput("label_geom", 'geom', c("text", "label"), selected = "label"),
        numericInput("label_alpha", "alpha", 0.5, min = 0, max = 1, step = 0.1),
        colourInput("label_color", "color", value = "white"),
        numericInput("label_size", "size", 3),
        numericInput("label_percent_digit", "digit", 0, step = 1, min = 0, max = 3),
        numericInput("label_txtWidth", 'text width', 40, step = 1, min = 1, max = 100)
      ),
      accordion_panel(
        "Edge Controls",
        selectInput("edge_lty", "line type", c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
        numericInput("edge_size", 'size', 1, step = 1, min = 0, max = 10)
      ),
      accordion_panel(
        "Filling Controls",
        colourInput("low", "low value color", value = "white", showColour = "both", palette = "limited", closeOnClick = TRUE),
        colourInput("high", "high value color", value = "red",showColour = "both", palette = "limited", closeOnClick = TRUE),
      ),
      accordion_panel(
        "Upset Controls",
        numericInput("nintersects", "nintersects", 20, min = 1, max = 100, step = 1),
        selectInput("order.intersect.by", "order of intersect",c("size", "name", "none"), selected = "none"),
        selectInput("order.set.by", 'order of set', c("size", "name", "none"), selected = 'none'),
        numericInput("relative_height", 'relative height', 3, min = 2, max = 6, step = 0.1),
        numericInput('relative_width', 'relative width', 0.3, min = 0.1, max = 1, step = 0.1)
      ),
    ),

    fluidRow(
      # type of plot
      # column(4,
             checkboxInput(
               inputId = "force_upset",
               label = "Upset"
             )
             # ),
      # column(8,
      #   checkboxInput(
      #   "show_intersect",
      #   "Show Intersects"
      # ))
    ),

    # 画图按钮
    actionButton("plot_btn", "Plot Now!"),
  ),

  card(
    style = "overflow: visible;",
    uiOutput('plot_note'),

    # plot
    plotOutput("plot"),

    # download button
    conditionalPanel(
      condition = "output.plot",
      p("Download this plot in different formats:"),
      uiOutput("download_btns")

    )

  )
)




# SERVER SIDE FUNCTIONS ---------------------------------------------------


server = function(input, output, session) {
  # output format
  format = c("png","jpg","tiff","svg","pdf","pptx")
  output$download_btns = renderUI({
    dl_list = lapply(format, function(x){
      downloadButton(paste0("download_",x), toupper(x))
    })
    do.call(tagList, dl_list)
  })

  # 动态生成文本输入框的UI
  output$text_inputs = renderUI({
    # 生成 nsets 个文本输入框
    text_inputs = lapply(1:input$nsets, function(i) {
      div(
        class = "form-control my-2 p-2",
        div(
          class = "inline",
          textInput(paste0("setname_",i), NULL, paste("Set", i, sep = "_"), width = "60%"),
          colourInput(paste0("setcolor_",i), NULL, value = "black", showColour = "both", palette = "limited", closeOnClick = TRUE, width = "40%"),
        ),
        textAreaInput(paste0("set_", i),
                      label = "",
                      value = paste0(sample(letters, sample(3:10, 1)), collapse = ",")))
    })

    # 返回生成的文本输入框列表
    do.call(tagList, text_inputs)
  })

  # initialize plot note
  output$plot_note = renderUI({
    tagList(
      h2("Steps", class = "my-4"),
      markdown("1. Use the button or slider to specify the number of sets."),
      markdown("2. Specify set members using comma-sparated strings (accept separators are `,`, `;`, `\\t`, `\\n`, or `\\r`)."),
      markdown("3. Configure addtional parameters if you want."),
      markdown("4. Click the **<Plot Now!>** button."),
      markdown("5. Enjoy and download your publication-quality figures.")
    )
  })

  # 绘图的逻辑
  drawPlot <- function(){
    x = vector("list", length = input$nsets)
    category_names = vector("list", length = input$nsets)
    set_color = vector("list", length = input$nsets)
    for (i in 1:input$nsets) {
      x[[i]] = input[[paste0("set_", i)]] |> strsplit(split = "[,;\t\n\r]+") |> unlist()
      category_names[[i]] = input[[paste0("setname_",i)]]
      set_color[[i]] = input[[paste0("setcolor_", i)]]
    }
    set_color = unlist(set_color)
    p = ggVennDiagram(x,
                      category.names = category_names,
                      # show_intersect = input$show_intersect,
                      set_color = set_color,
                      set_size = input$set_size,
                      label = input$label,
                      label_alpha = input$label_alpha,
                      label_size = input$label_size,
                      label_percent_digit = input$label_percent_digit,
                      label_txtWidth = input$label_txtWidth,
                      edge_lty = input$edge_lty,
                      edge_size = input$edge_size,
                      force_upset = input$force_upset,
                      nintersects = input$nintersects,
                      order.intersect.by = input$order.intersect.by,
                      order.set.by = input$order.set.by,
                      relative_height = input$relative_height,
                      relative_width = input$relative_width)
    if (inherits(p, "upset_plot")){
      return(p)
    } else {
      return( p + scale_fill_gradient(low = input$low, high = input$high))
    }

  }

  # 监听画图按钮的点击事件
  observeEvent(input$plot_btn, {
    p = drawPlot()
    output$plot = renderPlot(p)
    output$plot_note = NULL
    session$userData$plot = p
  })

  download_filename = function(format){
    # 获取当前时间
    current_time <- Sys.time()

    # 将时间格式化为字符串，例如"2023-12-26_14-30-00"
    formatted_time <- format(current_time, format = "%Y-%m-%d_%H-%M-%S")

    # 生成文件名
    file_name <- paste0("ggVennDiagram_", formatted_time, ".", format)

    # 打印生成的文件名
    return(file_name)

  }


  # 下载按钮的回调函数
  lapply(format, function(x) {
    if (x == "pptx"){
      output[[paste0("download_", x)]] = downloadHandler(
        filename = download_filename(x),
        content = function(file){
          export::graph2ppt(file = file, x = session$userData$plot)
        }
      )
    } else {
      output[[paste0("download_", x)]] = downloadHandler(
        filename = download_filename(x),
        content = function(file){
          ggsave(file, session$userData$plot, device = x)
        }
      )
    }
  })

}


# RUN shinyApp() ----------------------------------------------------------

shinyApp(ui, server)
