options(shiny.maxRequestSize = 100 * 1024^2) # 最大允许上传文件大小100 MB
source("Utils/tool.R")
source("DiffAnalysis/Test.R")


DiffAnalysis_ui <- function(id) {
  tagList(
    # Add custom CSS for modal customization
    tags$head(
      tags$style(HTML("
      /* 定义用于控制右侧弹出的 modal */
      #custom-modal .modal-dialog {
          position: fixed;
          margin: 0;
          width: 40%;
          height: 100%;
          right: 0;
          top: 0;
          bottom: 0;
          transition: transform 0.3s ease-out;
      }

      #custom-modal .modal-content {
          height: 100%;
          overflow-y: auto;
      }

      #custom-modal .modal-header, #custom-modal .modal-footer {
          padding: 10px;
      }

      /* 遮罩层样式 */
      .modal-backdrop {
          background-color: rgba(0,0,0,0.5);
      }

      /* 初始化时右侧显示外部 */
      #custom-modal.modal.fade .modal-dialog {
          transform: translateX(100%);
      }

      /* 显示时滑入 */
      #custom-modal.modal.fade.show .modal-dialog {
          transform: translateX(0);
      }
    "))
    ),
    fluidPage(
      fluidRow(actionButton(NS(id, "help"), "Help")), # Help button
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            fluidPage(
              fluidRow(UploadFile_ui(NS(id, "diff"), label = "上传文件", multiple = TRUE), style = "margin-bottom: 20px;"),
              fluidRow(SmapleGroup_ui(NS(id, "diff")), style = "margin-bottom: 20px;"),
              fluidRow(CompareMethod_ui(NS(id, "diff")), style = "margin-bottom: 20px;"),
              fluidRow(actionButton(NS(id, "startAnalysis"), "开始差异分析"), style = "margin-bottom: 20px;")
            ),
            width = 4
          ),
          mainPanel(
            DataTable_ui(NS(id, "diff")),
            width = 8
          )
        )
      ),
      fluidRow(textOutput(NS(id, "error_message"))), # 展示错误信息
      fluidRow(CompareResult_ui(NS(id, "diff")))
    )
  )
}


DiffAnalysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #--------------------------------#
    # 添加说明文档
    #--------------------------------#
    observeEvent(input$help, {
      showModal(
        tags$div(
          id = "custom-modal",
          modalDialog(
            tags$head(
              tags$style(HTML("
                /* 自定义 Markdown 表格样式 */
                table {
                  table-layout: auto; /* 自动调整列宽 */
                  border-collapse: collapse;
                  width: auto; /* 表格宽度由内容决定 */
                  max-width: 100%; /* 防止表格超过容器宽度 */
                }
                table, th, td {
                  border: 1px solid black;
                }
                th, td {
                  padding: 8px;
                  text-align: left;
                }
                th {
                  background-color: #f2f2f2;
                }
              "))
            ),
            includeMarkdown("DiffAnalysis/help.md"),
            easyClose = TRUE
          )
        )
      )
    })

    uploadFileInfo <- ReadFile_server("diff")

    #--------------------------------#
    # 处理表达矩阵数据
    #--------------------------------#
    data <- reactive({
      req(uploadFileInfo())
      exprMatrixPath <- uploadFileInfo()$datapath[uploadFileInfo()$name == "exprMatrix"]
      if (length(exprMatrixPath) > 0) {
        return(ReadFile(exprMatrixPath))
      } else {
        return(NULL)
      }
    })
    DataTable_server("diff", data, isSelect = FALSE)

    #--------------------------------#
    # 处理样本分组信息
    #--------------------------------#
    sampleInfo <- SmapleGroup_server("diff", data, uploadFileInfo)


    #--------------------------------#
    # 处理组别比较信息
    #--------------------------------#
    compareInfo <- CompareMethod_server("diff", sampleInfo, uploadFileInfo)

    #--------------------------------#
    # 进行差异分析
    #--------------------------------#
    diffResults <- reactiveVal(list()) # 定义一个全局的 reactiveVal 来存储差异分析结果和错误信息
    errorMessage <- reactiveVal(NULL) # 用于存储错误信息

    observeEvent(input$startAnalysis, {
      req(data(), sampleInfo(), compareInfo())

      # 捕获差异分析过程中的错误
      tryCatch(
        {
          newDiffResults <- Diff(data(), sampleInfo(), compareInfo()) # 执行差异分析
          diffResults(newDiffResults) # 更新 diffResults 的值
          errorMessage("分析成功") # 更新信息
        },
        error = function(e) {
          errorMessage(paste("差异分析时出错:", e$message)) # 捕获错误并更新错误信息
        }
      )
    })

    # 将错误信息渲染到页面中
    output$error_message <- renderText({
      req(errorMessage())
      errorMessage()
    })

    #--------------------------------#
    # 展示差异分析结果
    #--------------------------------#
    CompareResult_server("diff", compareInfo, diffResults)
  })
}
