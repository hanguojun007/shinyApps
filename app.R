library(shiny)

source("/root/shiny/shinyApps/DiffAnalysis/test.R")
source("/root/shiny/shinyApps/Heatmap/test.R")

ui <- fluidPage(
  navlistPanel(
    id = "Big Class",
    widths = c(2, 10),
    "分析类",
    tabPanel(
      "差异分析",
      "差异分析UI",
      test_ui()
    ),
    "画图类",
    tabPanel(
      "表达矩阵热图",
      "表达矩阵热图UI",
      test2_ui()
    )
  )
)

server <- function(input, output, session) {
  test_server(input, output, session)
  test2_server(input, output, session)
}

shinyApp(ui, server)
