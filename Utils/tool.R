#-----------------------------------#
# 读取文件，并生成表格展示，返回选择的行号
#-----------------------------------#
UploadFile_ui <- function(id, label = "FileUpload", placeholder = "Upload File", multiple = FALSE) {
  tagList(
    fileInput(NS(id, "FileUpload"), label = label, placeholder = placeholder, multiple = multiple, accept = c("tsv", "csv", "xlsx")),
  )
}

ReadFile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploadFileInfo <- reactive({
      req(input$FileUpload)

      input$FileUpload %>%
        dplyr::mutate(name = tools::file_path_sans_ext(name))
    })

    return(uploadFileInfo)
  })
}

ReadFile <- function(filePath) {
  if (tools::file_ext(filePath) == "xlsx") {
    readxl::read_excel(filePath)
  } else if (tools::file_ext(filePath) == "csv") {
    vroom::vroom(filePath, delim = ",")
  } else if (tools::file_ext(filePath) == "tsv") {
    vroom::vroom(filePath, delim = "\t")
  } else {
    vroom::vroom(filePath)
  }
}

#-----------------------------------#
# 下载文件
#-----------------------------------#
DownloadFile_ui <- function(id, label = "Download") {
  tagList(
    downloadButton(NS(id, "download"), label = label)
  )
}

DownloadFile_server <- function(id, fileName, fileData, fileFormat = "csv") {
  stopifnot(is.character(fileName))
  stopifnot(fileFormat %in% c("xlsx", "csv", "tsv"))
  stopifnot(is.reactive(fileData))
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {
        glue::glue("{fileName}.{fileFormat}")
      },
      content = function(file) {
        if (fileFormat == "xlsx") {
          if (is.list(fileData())) {
            # 创建新的 Excel 工作簿
            wb <- openxlsx::createWorkbook()

            for (compare in names(fileData())) {
              # 后续对compare进行处理，符合31字符
              sheetName <- compare
              openxlsx::addWorksheet(wb, sheetName) # 添加工作表
              openxlsx::writeData(wb, sheet = sheetName, fileData()[[sheetName]]) # 写入数据
            }

            # 保存工作簿到文件
            openxlsx::saveWorkbook(wb, file)
          } else {
            openxlsx::write.xlsx(fileData(), file)
          }
        } else if (fileFormat == "tsv") {
          vroom::vroom_write(fileData(), file, delim = "\t")
        } else {
          vroom::vroom_write(fileData(), file, delim = ",")
        }
      }
    )
  })
}


#-----------------------------------#
# table 展示，返回选择的行号
#-----------------------------------#
DataTable_ui <- function(id) {
  tagList(
    reactable::reactableOutput(NS(id, "table"), width = "auto", height = "auto")
  )
}

DataTable_server <- function(id, tableData, isSelect = TRUE) {
  stopifnot(is.reactive(tableData))
  moduleServer(id, function(input, output, session) {
    if (isSelect) {
      output$table <- reactable::renderReactable({
        reactable::reactable(tableData(), paginationType = "jump", selection = "multiple", onClick = "select", defaultSelected = c(1:nrow(tableData())))
      })

      selected <- reactive(getReactableState("table", "selected"))

      return(selected)
    } else {
      output$table <- reactable::renderReactable({
        reactable::reactable(tableData())
      })
    }
  })
}


#-----------------------------------#
# 根据读取的文件列名，对列进行分组，
# 并返回分组信息及文件
#-----------------------------------#
SmapleGroup_ui <- function(id) {
  tagList(
    fluidPage(
      fluidRow(
        actionButton(NS(id, "add_group"), "设置分组"),
        actionButton(NS(id, "group_reset"), "重设分组")
      ),
      fluidRow(tableOutput(NS(id, "group_result"))),
      fluidRow(DownloadFile_ui(NS(id, "download_group"), "下载分组信息"))
    )
  )
}

SmapleGroup_server <- function(id, data, uploadFileInfo) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(uploadFileInfo))
  moduleServer(id, function(input, output, session) {
    # 获取当前模块的命名空间
    ns <- session$ns

    # 初始化分组信息
    sampleInfo <- shiny::reactiveVal(data.frame(Sample = character(), Group = character(), stringsAsFactors = FALSE))
    # 更新 sampleInfo 的值
    observe({
      req(uploadFileInfo())
      sampleInfoPath <- uploadFileInfo()$datapath[uploadFileInfo()$name == "sampleInfo"]

      # 检查样本信息文件是否存在
      if (length(sampleInfoPath) > 0) {
        sampleInfo(ReadFile(sampleInfoPath)) # 更新 sampleInfo 的值
      }
    })

    # 可选择的列（动态更新）
    available_columns <- reactive({
      req(sampleInfo())
      setdiff(names(data()), sampleInfo()$Sample)
    })

    # 存储最后一次选择的列
    last_selected_columns <- reactiveVal(list())

    # 点击添加分组时弹出模态框
    observeEvent(input$add_group, {
      req(available_columns())

      # 定义模态框内容
      showModal(modalDialog(
        title = "设置分组",
        textOutput(ns("test_res_columns")),
        checkboxGroupInput(ns("res_columns"), "可进行分组的列", choices = available_columns()),
        textInput(ns("group_name"), "组名", ""), # 组名输入框
        actionButton(ns("set_group"), "Set Group"),
        actionButton(ns("cancel_group"), "Cancel"),
        tableOutput(ns("sample_group_preview")),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_group"), "确认")
        )
      ))
    })

    # 点击 Set Group 按钮时,更新分组信息
    observeEvent(input$set_group, {
      req(input$res_columns, input$group_name)

      selected_columns <- input$res_columns
      group_name <- input$group_name

      # 追加到 last_selected_columns
      current_last <- last_selected_columns()
      last_selected_columns(c(current_last, list(selected_columns)))

      # 更新分组
      new_group <- data.frame(Sample = selected_columns, Group = group_name, stringsAsFactors = FALSE)
      sampleInfo(rbind(sampleInfo(), new_group)) # 更新分组信息

      # 清空输入框
      updateTextInput(session, "group_name", value = "") # 清空组名输入框
      updateCheckboxGroupInput(session, "res_columns", choices = available_columns())
    })

    # 点击 Cancel 按钮时，更新分组信息
    observeEvent(input$cancel_group, {
      req(last_selected_columns())

      current_last <- last_selected_columns()
      if (length(current_last) > 0) {
        # 删除最后一个选择
        last_selection <- current_last[[length(current_last)]]
        last_selected_columns(current_last[-length(current_last)]) # 更新 last_selected_columns

        # 从 sampleInfo 中删除这些列
        current_sampleInfo <- sampleInfo()
        new_sampleInfo <- current_sampleInfo[!current_sampleInfo$Sample %in% last_selection, ]
        sampleInfo(new_sampleInfo) # 更新分组信息
      }

      # 清空输入框
      updateTextInput(session, "group_name", value = "") # 清空组名输入框
      updateCheckboxGroupInput(session, "res_columns", choices = available_columns())
    })

    # 点击 Confirm 按钮时，关闭模态框
    observeEvent(input$confirm_group, {
      removeModal() # 关闭模态框
    })

    # 在模态框中显示 reactable
    output$sample_group_preview <- renderTable({
      req(sampleInfo())
      sampleInfo() # 输出分组信息
    })

    # 再网页中显示 sampleInfo
    output$group_result <- renderTable({
      req(nrow(sampleInfo()) > 0)
      sampleInfo() # 输出分组信息
    })

    observeEvent(input$group_reset, {
      sampleInfo(data.frame(Sample = character(), Group = character(), stringsAsFactors = FALSE))
    })

    DownloadFile_server("download_group", "sampleInfo", sampleInfo)
    return(sampleInfo)
  })
}

#-----------------------------------#
# 根据分组信息，设置比较方法
#-----------------------------------#
CompareMethod_ui <- function(id) {
  tagList(
    fluidPage(
      fluidRow(
        actionButton(NS(id, "add_method"), "添加比较组信息"),
        actionButton(NS(id, "method_reset"), "重设比较组")
      ),
      fluidRow(tableOutput(NS(id, "method_result"))),
      fluidRow(DownloadFile_ui(NS(id, "download_method"), "下载比较信息"))
    )
  )
}

CompareMethod_server <- function(id, sampleInfo, uploadFileInfo) {
  stopifnot(is.reactive(sampleInfo))
  stopifnot(is.reactive(uploadFileInfo))
  moduleServer(id, function(input, output, session) {
    # 获取当前模块的命名空间
    ns <- session$ns

    # 初始化比较组信息
    compareInfo <- reactiveVal(data.frame(Compare = character(), Method = character(), isPair = logical(), stringsAsFactors = FALSE))
    # 更新 compareInfo 的值
    observe({
      req(uploadFileInfo())
      compareInfoPath <- uploadFileInfo()$datapath[uploadFileInfo()$name == "compareInfo"]

      # 检查样本信息文件是否存在
      if (length(compareInfoPath) > 0) {
        compareInfo(ReadFile(compareInfoPath)) # 更新 sampleInfo 的值
      }
    })

    # 可选择的组别（动态更新）
    available_group <- reactive({
      unique(sampleInfo()$Group)
    })
    # 点击添加分组时弹出模态框
    observeEvent(input$add_method, {
      req(available_group())

      # 定义模态框内容
      showModal(modalDialog(
        title = "设置比较组信息",
        fluidRow(
          # column(4, checkboxGroupInput(ns("res_groups"), "可进行比较的组别\n至少两个选项", choices = available_group())),
          column(4, selectInput(ns("res_groups"), "可进行比较的组别<br/>至少两个选项<br/>两个以上默认使用ANOVA,无配对信息", multiple = TRUE, choices = available_group())),
          column(4, selectInput(ns("compare_method"), "比较方法", choices = c("t.test", "wilcox.test", "aov"), selected = "t.test")),
          column(4, checkboxInput(ns("isPair"), "是否进行配对", value = FALSE))
        ),
        textOutput(ns("compare_info")),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_compare"), "确认")
        )
      ))
    })

    # 检查用户选择的组别，至少选择两组
    observeEvent(input$res_groups, {
      req(input$res_groups)

      if (length(input$res_groups) < 2) {
        output$compare_info <- renderText("请至少选择两组进行比较")
      } else {
        output$compare_info <- renderText(paste(input$res_groups, collapse = "_vs_"))
      }

      if (length(input$res_groups) > 2) {
        updateSelectInput(session, "compare_method", choices = "aov", selected = "aov")
        updateCheckboxInput(session, "isPair", value = FALSE)
      } else {
        updateSelectInput(session, "compare_method", choices = c("t.test", "wilcox.test", "aov"), selected = "t.test")
      }
    })

    # 点击确认时更新compareInfo，并关闭模态框
    observeEvent(input$confirm_compare, {
      req(input$res_groups)
      if (length(input$res_groups) >= 2) {
        # 更新compareInfo
        new_compare <- data.frame(
          Compare = paste(input$res_groups, collapse = "_vs_"),
          Method = input$compare_method,
          isPair = input$isPair,
          stringsAsFactors = FALSE
        )
        # 判断 new_compare 是否存在于 compareInfo 中
        if (nrow(compareInfo() %>%
          filter(
            Compare == new_compare$Compare,
            Method == new_compare$Method,
            isPair == new_compare$isPair
          )) == 0) {
          compareInfo(bind_rows(compareInfo(), new_compare))
        }
        # 关闭模态框
        removeModal()
      }
    })

    # 渲染比较组信息表格
    output$method_result <- renderTable({
      req(nrow(compareInfo()) > 0)
      compareInfo()
    })

    # 重置比较组信息
    observeEvent(input$method_reset, {
      compareInfo(data.frame(Compare = character(), Method = character(), isPair = logical(), stringsAsFactors = FALSE))
    })

    DownloadFile_server("download_method", "compareInfo", compareInfo)

    return(compareInfo)
  })
}
