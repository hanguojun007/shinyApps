options(shiny.maxRequestSize = 100 * 1024^2) # 最大允许上传文件大小100 MB
source("Utils/tool.R")


DiffAnalysis_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        UploadFile_ui(shiny::NS(id, "diff")),
        SmapleGroup_ui(shiny::NS(id, "diff")),
        CompareMethod_ui(shiny::NS(id, "diff")),
        width = 4
      ),
      mainPanel(
        DataTable_ui(shiny::NS(id, "diff")),
        textOutput(shiny::NS(id, "text")),
        width = 8
      )
    )
  )
}


DiffAnalysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- ReadFile_server("diff")
    selected <- DataTable_server("diff", data)

    output$text <- renderText({
      req(selected())
      selected()
    })

    sampleInfo <- SmapleGroup_server("diff", data)
    CompareMethod_server("diff", sampleInfo)
  })
}
