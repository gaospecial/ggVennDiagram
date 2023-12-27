library(shiny)
library(ggVennDiagram)
devtools::load_all(path = rprojroot::find_root("DESCRIPTION"))
library(ggplot2)
library(bslib)
library(htmltools)

# SMALL HTML ELEMENTS -------------------------------------------------------------


## Basic Elements of Sidebar ---------------------------------------------

### Venn plot primary control --------
venn_plot_primary_ctrl = accordion_panel(
  "Primary controls",
  # Set number
  sliderInput(
    inputId = 'nsets',
    label = "No. of Sets:",
    value = 4,
    min = 2,
    max = 10,
    step = 1
  ),

  hr(),

  # dynamic inputs
  uiOutput("text_inputs")
)

venn_plot_other_ctrl = accordion_panel(
  "Other controls",
  "Other controls go here"
)

# PAGE COMPONENTS ------

## Venn plot sidebar -----

venn_plot_card_sidebar = sidebar(
  width = "25em",
  accordion(
    venn_plot_primary_ctrl,
    venn_plot_other_ctrl
  ),
  actionButton("plot_btn", "Plot Now!")
)

# PAGE LAYOUT-----

## Navset card UI
ui_navset_card_tab = navset_card_tab(
  height = 700,
  full_screen = TRUE,
  title = "Draw a Venn diagram or Upset plot",

  # Venn plot card
  nav_panel(  # for Venn plot
    "Venn Diagram",
    card_title("A plotly plot"),
    layout_sidebar(
      sidebar = venn_plot_card_sidebar,

      # others
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
  ),

  # Upset plot card
  nav_panel(  # for Upset plot
    "Upset Plot",
    card_title("A leaflet plot"),
    markdown("leaflet_widget")
  ),

  # Help card
  nav_panel(  # for help
    shiny::icon("circle-info"),
    markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
  )
)


ui_page_sidebar = page_sidebar(
  title = "Penguins dashboard",
  sidebar = venn_plot_card_sidebar,
  navset_card_underline(
    title = "Histograms by species",
    nav_panel("Bill Length", plotOutput("bill_length")),
    nav_panel("Bill Depth", plotOutput("bill_depth")),
    nav_panel("Body Mass", plotOutput("body_mass"))
  )
)

# THEME -----
bs_theme = bs_theme(version = 5)
bs_theme = bs_theme |>
  bs_add_rules(xfun::read_utf8(xfun::magic_path("shinyApp.css")) |> paste(collapse = "\n"))

# NATIVE SHINY UI ------------------------------------------------------------------

ui = fluidPage(
  theme = bs_theme,
  titlePanel("ggVennDiagram Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # Set number
      sliderInput(
        inputId = 'nsets',
        label = "Number of Sets: ",
        value = 4,
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
        "Other Controls",
        selectInput("label-geom", 'label-geom', c("text", "label"))
      )),


      fluidRow(
        # type of plot
        column(4,
          checkboxInput(
            inputId = "force_upset",
            label = "Upset"
          )),
        column(8,
          checkboxInput(
          "show_intersect",
          "Show Intersects"
        ))
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



# SERVER SIDE FUNCTIONS ---------------------------------------------------


server = function(input, output, session) {
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
      div(
        class = "form-control my-2 p-2",
        fluidRow(
          column(textInput(paste0("setname_",i), NULL, paste("Set", i, sep = "_")), width = 10),
          column(colourInput(paste0("setcolor_",i), NULL, value = "black", showColour = "background"), width = 2),
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
      markdown("### Steps"),
      markdown("1. Use the button or slider to specify the number of sets."),
      markdown("2. Specify set members using comma-sparated strings (just follow the examples)."),
      markdown("3. Click the **<Plot Now!>** button."),
      markdown("4. Enjoy and download your publication-quality figures.")
    )
  })

  # 绘图的逻辑
  drawPlot <- function(){
    x = vector("list", length = input$nsets)
    category_names = vector("list", length = input$nsets)
    set_color = vector("list", length = input$nsets)
    for (i in 1:input$nsets) {
      x[[i]] = input[[paste0("set_", i)]] |> strsplit(split = ",") |> unlist()
      category_names[[i]] = input[[paste0("setname_",i)]]
      set_color[[i]] = input[[paste0("setcolor_", i)]]
    }
    return(ggVennDiagram(x,
                         category.names = category_names,
                         set_color = unlist(set_color),
                         force_upset = input$force_upset))
  }

  # 监听画图按钮的点击事件
  observeEvent(input$plot_btn, {
    p = drawPlot()
    output$plot = renderPlot(p)
    output$plot_note = NULL
    session$userData$plot = p
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


# RUN shinyApp() ----------------------------------------------------------

shinyApp(ui, server)
