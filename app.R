library(shiny)
library(tidyverse)

source("DiffAnalysis/DiffAnalysis.R")
source("ExprHeatmap/ExprHeatmap.R")

ui <- fluidPage(
  navlistPanel(
    id = "Big Class",
    widths = c(2, 10),
    "分析类",
    tabPanel(
      "差异分析",
      DiffAnalysis_ui("DiffAnalysis")
    ),
    "画图类",
    tabPanel(
      "表达矩阵热图",
      "表达矩阵热图UI",
    )
  )
)

server <- function(input, output, session) {
  DiffAnalysis_server("DiffAnalysis")
}

shinyApp(ui, server)
