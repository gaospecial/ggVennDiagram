library(shiny)
library(ggVennDiagram)
library(ggplot2)
library(shinyjs)

ui = fluidPage(
  titlePanel("Draw a Venn diagram or Upset plot"),
  sidebarLayout(
    sidebarPanel(
      # Set number
      sliderInput(
        inputId = 'nsets',
        label = "No. of Sets:",
        value = 4,
        min = 1,
        max = 10,
        step = 1
      ),

      # Increase or decrease set number
      actionButton(
        inputId = "decrease_btn",
        label = "decrease the no. of set",
        icon = icon("caret-down")
      ),
      actionButton(
        inputId = "increase_btn",
        label = "increase the no. of set",
        icon = icon("caret-up"),
        align = "right"
      ),

      hr(),

      # dynamic inputs
      uiOutput("text_inputs"),
      hr(),

      # type of plot
      checkboxInput(
        inputId = "force_upset",
        label = "Upset Plot"
      ),

      # 画图按钮
      actionButton("plot_btn", "Plot Now!"),
    ),

    mainPanel = mainPanel(
      uiOutput('plot_note'),

      # plot
      plotOutput("plot"),

      # download button
      conditionalPanel(
        condition = "output.plot",
        downloadButton("download_png", "Download as PNG"),
        downloadButton("download_pdf", "Download as PDF")
      )

    )
  )

)


server = function(input, output, session){
  # 监听按钮的点击事件，更新 slider 的值
  observeEvent(input$increase_btn, {
    updateSliderInput(session, "nsets", value = input$nsets + 1)
  })

  observeEvent(input$decrease_btn, {
    updateSliderInput(session, "nsets", value = input$nsets - 1)
  })

  # 动态生成文本输入框的UI
  output$text_inputs = renderUI({
    # 生成 nsets 个文本输入框
    text_inputs = lapply(1:input$nsets, function(i) {
      textAreaInput(paste0("set_", i),
                    label = paste0("Set_", i),
                    value = paste0(sample(letters, sample(3:10, 1)), collapse = ","))
    })

    # 返回生成的文本输入框列表
    do.call(tagList, text_inputs)
  })

  # initialize plot note
  output$plot_note = renderUI({
    tagList(
      markdown("### Steps"),
      markdown("1. Use the button or slider to specify the number of sets."),
      markdown("2. Specify set members using comma-sparated strings (just follow the examples)."),
      markdown("3. Click the **<Plot Now!>** button."),
      markdown("4. Enjoy and download your publication-quality figures.")
    )
  })

  # 监听画图按钮的点击事件
  observeEvent(input$plot_btn, {
    output$plot = renderPlot({
      x = vector("list", length = input$nsets)
      for (i in 1:input$nsets){
        x[[i]] = input[[paste0("set_", i)]] |> strsplit(split = ",") |> unlist()
      }
      (session$userData$plot = ggVennDiagram(x, force_upset = input$force_upset))
    })
    output$plot_note = renderUI(
      NULL
    )
  })


  # 下载按钮的回调函数
  output$download_png <- downloadHandler(
    filename = function() { "plot.png" },
    content = function(file) {
      ggsave(file, session$userData$plot, device = "png", width = 10, height = 6, units = "in", dpi = 300)
    }
  )

  output$download_pdf <- downloadHandler(
    filename = function() { "plot.pdf" },
    content = function(file) {
      ggsave(file, session$userData$plot, device = "pdf", width = 10, height = 6, units = "in")
    }
  )


}


shinyApp(ui, server)
