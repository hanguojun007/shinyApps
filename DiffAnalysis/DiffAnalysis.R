options(shiny.maxRequestSize = 100 * 1024^2) # 最大允许上传文件大小100 MB
source("Utils/tool.R")
source("DiffAnalysis/Test.R")


DiffAnalysis_ui <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        fluidPage(
          fluidRow(UploadFile_ui(shiny::NS(id, "diff"))),
          fluidRow(SmapleGroup_ui(shiny::NS(id, "diff"))),
          fluidRow(CompareMethod_ui(shiny::NS(id, "diff"))),
          fluidRow(
            actionButton(shiny::NS(id, "startAnalysis"), "开始差异分析"),
          )
        ),
        width = 4
      ),
      mainPanel(
        DataTable_ui(shiny::NS(id, "diff")),
        textOutput(shiny::NS(id, "text")),
        width = 8
      )
    ),
    textOutput(shiny::NS(id, "error_message")), # 展示错误信息
    CompareResult_ui(shiny::NS(id, "diff"))
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
    compareInfo <- CompareMethod_server("diff", sampleInfo)

    # 定义一个全局的 reactiveVal 来存储差异分析结果和错误信息
    diffResults <- shiny::reactiveVal(list())
    errorMessage <- shiny::reactiveVal(NULL) # 用于存储错误信息

    observeEvent(input$startAnalysis, {
      req(data(), sampleInfo(), compareInfo())

      # 捕获差异分析过程中的错误
      tryCatch(
        {
          # 执行差异分析
          newDiffResults <- Diff(data(), sampleInfo(), compareInfo())

          # 更新 diffResults 的值
          diffResults(newDiffResults)

          # 清空错误信息
          errorMessage("分析成功")
        },
        error = function(e) {
          # 捕获错误并更新错误信息
          errorMessage(paste("差异分析时出错:", e$message))
        }
      )
    })

    # 将错误信息渲染到页面中
    output$error_message <- renderText({
      req(errorMessage())
      errorMessage()
    })

    # 将 diffResults 传递给 CompareResult_server 模块
    CompareResult_server("diff", compareInfo, diffResults)
  })
}
