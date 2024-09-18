options(shiny.maxRequestSize = 100 * 1024^2) # 最大允许上传文件大小100 MB



test_ui <- function() {
  fluidPage(
    selectInput("dataset", "Pick a dataset", ls("package:datasets")),
    tableOutput("preview"),
    downloadButton("download", "Download .tsv")
  )
}


test_server <- function(input, output, session) {
  data <- reactive({
    out <- get(input$dataset, "package:datasets")
    if (!is.data.frame(out)) {
      validate(paste0("'", input$dataset, "' is not a data frame"))
    }
    out
  })

  output$preview <- renderTable({
    head(data())
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
}
