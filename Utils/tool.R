#-----------------------------------#
# 读取文件，并生成表格展示，返回选择的行号
#-----------------------------------#
UploadFile_ui <- function(id) {
  shiny::tagList(
    fileInput(shiny::NS(id, "FileUpload"), label = "FileUpload", placeholder = "Upload File", accept = c("tsv", "csv", "xlsx")),
  )
}


ReadFile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$FileUpload)

      if (tools::file_ext(input$FileUpload$datapath) == "xlsx") {
        readxl::read_excel(input$FileUpload$datapath)
      } else {
        vroom::vroom(input$FileUpload$datapath)
      }
    })

    return(data)
  })
}

#-----------------------------------#
# table 展示，返回选择的行号
#-----------------------------------#
DataTable_ui <- function(id) {
  tagList(
    reactable::reactableOutput(shiny::NS(id, "table"), width = "auto", height = "auto")
  )
}

DataTable_server <- function(id, data) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    output$table <- reactable::renderReactable({
      reactable::reactable(data(), paginationType = "jump", selection = "multiple", onClick = "select", defaultSelected = c(1:nrow(data())))
    })

    selected <- reactive(getReactableState("table", "selected"))

    return(selected)
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
        actionButton(shiny::NS(id, "add_group"), "设置分组"),
        actionButton(shiny::NS(id, "group_reset"), "重设分组")
      ),
      tableOutput(shiny::NS(id, "group_result"))
    )
  )
}

SmapleGroup_server <- function(id, data) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    # 获取当前模块的命名空间
    ns <- session$ns

    # 初始化分组信息
    sampleInfo <- shiny::reactiveVal(data.frame(Sample = character(), Group = character(), stringsAsFactors = FALSE))

    # 可选择的列（动态更新）
    available_columns <- reactive({
      setdiff(names(data()), sampleInfo()$Sample)
    })

    # 存储最后一次选择的列
    last_selected_columns <- shiny::reactiveVal(list())

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

    # 点击 Confirm 按钮时，更新分组信息
    observeEvent(input$confirm_group, {
      removeModal() # 关闭模态框
      output$group_result <- renderTable({
        req(sampleInfo())
        sampleInfo() # 输出分组信息
      })
    })

    # 在模态框中显示 reactable
    output$sample_group_preview <- renderTable({
      req(sampleInfo())
      sampleInfo() # 输出分组信息
    })

    observeEvent(input$group_reset, {
      sampleInfo(data.frame(Sample = character(), Group = character(), stringsAsFactors = FALSE))
    })

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
        actionButton(shiny::NS(id, "add_method"), "添加比较组信息"),
        actionButton(shiny::NS(id, "method_reset"), "重设比较组")
      ),
      tableOutput(shiny::NS(id, "method_result"))
    )
  )
}

CompareMethod_server <- function(id, sampleInfo) {
  stopifnot(is.reactive(sampleInfo))
  moduleServer(id, function(input, output, session) {
    # 获取当前模块的命名空间
    ns <- session$ns

    # 初始化比较组信息
    compareInfo <- shiny::reactiveVal(data.frame(Compare = character(), Method = character(), isPair = logical(), stringsAsFactors = FALSE))

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
        compareInfo(rbind(compareInfo(), new_compare))

        # 关闭模态框
        removeModal()
      }
    })

    # 渲染比较组信息表格
    output$method_result <- renderTable({
      req(compareInfo())
      compareInfo()
    })

    # 重置比较组信息
    observeEvent(input$method_reset, {
      compareInfo(data.frame(Compare = character(), Method = character(), isPair = logical(), stringsAsFactors = FALSE))
    })

    return(compareInfo)
  })
}
