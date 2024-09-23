CompareResult_ui <- function(id) {
  tagList(
    # 动态选择比较组
    selectInput(shiny::NS(id, "compare_select"), "选择比较组", choices = NULL),

    # 显示选择的比较组差异分析结果
    reactable::reactableOutput(shiny::NS(id, "compare_result"), width = "auto", height = "auto")
  )
}

CompareResult_server <- function(id, compareInfo, diffResults) {
  stopifnot(is.reactive(compareInfo))
  stopifnot(is.reactive(diffResults))

  moduleServer(id, function(input, output, session) {
    # 动态更新比较组选择框的选项
    observe({
      req(compareInfo())
      updateSelectInput(session, "compare_select", choices = compareInfo()$Compare)
    })


    # 根据选择的比较组展示对应的差异分析结果
    output$compare_result <- reactable::renderReactable({
      req(input$compare_select, length(diffResults()) > 0)

      # 根据选择的比较组名提取对应的分析结果
      selected_group <- input$compare_select
      print(selected_group)
      print("____________")
      print(diffResults())
      result <- diffResults()[[selected_group]] # diffResults 存储不同组的结果
      print(result)

      if (!is.null(result)) {
        reactable::reactable(result, paginationType = "jump", selection = "multiple", onClick = "select", defaultSelected = c(1:nrow(result)))
      } else {
        data.frame(Notice = "没有对应的差异分析结果")
      }
    })
  })
}


Test <- function(groups, method, sampleInfo) {
  library(foreach)
  .pairedTtest <- function(x, y) {
    if (length(x) != length(y)) {
      stop("数据长度不一致，无法进行配对t检验")
    } else {
      if (sum(!is.na(x)) > 1 & sum(!is.na(y)) > 1) { # 保证每组至少有两个以上非NA的值，才进行t.test
        if (!sd(x, na.rm = T) == 0 | !sd(y, na.rm = T) == 0) { # 保证至少有一组数据方差不为0
          pvalue <- t.test(x, y, alternative = "two.side", var.equal = TRUE, paired = FALSE)$p.value
        } else {
          pvalue <- NA
        }
      } else {
        pvalue <- NA
      }
      return(pvalue)
    }
  }

  .unpairedTtest <- function(x, y) {
    if (sum(!is.na(x)) > 1 & sum(!is.na(y)) > 1) { # 保证每组至少有两个以上非NA的值，才进行t.test
      if (!sd(x, na.rm = T) == 0 | !sd(y, na.rm = T) == 0) { # 保证至少有一组数据方差不为0
        pvalue <- t.test(x, y, alternative = "two.side", var.equal = TRUE, paired = FALSE)$p.value
      } else {
        pvalue <- NA
      }
    } else {
      pvalue <- NA
    }
    return(pvalue)
  }

  # Warning message:
  # In wilcox.test.default(c(8, 4, 4), c(1, 6, 2)) : 无法精確計算带连结的p值
  # 上述警告是因为数据中存在相同的值，相同的值被称为结。可以使用参数exact=FALSE来解决。
  .pairedWilcoxTest <- function(x, y) {
    if (length(x) != length(y)) {
      stop("数据长度不一致，无法进行配对wilcox检验")
    } else {
      pvalue <- wilcox.test(x, y, paired = TRUE)$p.value
      return(pvalue)
    }
  }

  .unpairedWilcoxTest <- function(x, y) {
    pvalue <- wilcox.test(x, y)$p.value
    return(pvalue)
  }

  MeanRes <- foreach(g = groups, .combine = cbind) %do% {
    samples <- sampleInfo$Sample[sampleInfo$Group == g]
    colName <- glue::glue("{g} Mean")
    value <- mean(dplyr::c_across(dplyr::all_of(samples)), na.rm = TRUE)
    res <- tibble::tibble(!!rlang::enquo(colName) := value)
    return(res)
  }

  if (length(groups) == 2) {
    groupAName <- glue::glue("{groups[1]} Mean")
    groupBName <- glue::glue("{groups[2]} Mean")
    sampleA <- sampleInfo$Sample[sampleInfo$Group == groups[1]]
    sampleB <- sampleInfo$Sample[sampleInfo$Group == groups[2]]

    .test <- switch(method,
      "paired_t.test" = .pairedTtest,
      "unpaired_t.test" = .unpairedTtest,
      "paired_wilcox.test" = .pairedWilcoxTest,
      "unpaired_wilcox.test" = .unpairedWilcoxTest
    )

    MeanRes <- MeanRes |>
      dplyr::mutate(`Fold Change` = !!rlang::sym(groupAName) / !!rlang::sym(groupBName))
    pvalue <- .test(dplyr::c_across(dplyr::all_of(sampleA)), dplyr::c_across(dplyr::all_of(sampleB)))
    res <- cbind(MeanRes, tibble::tibble(pvalue = pvalue))
  } else if (length(groups) > 2) {
    samples <- sampleInfo$Sample[sampleInfo$Group %in% groups]
    tmpData <- sampleInfo |>
      dplyr::filter(Group %in% groups) |>
      dplyr::mutate(Group = factor(Group, levels = groups)) |>
      dplyr::left_join(tibble::tibble(Sample = samples, value = c_across(all_of(samples))), by = "Sample")
    # print(tmpData)
    #+++++++++++++++++++++++++++++++++++++#
    # 对数据中的NA进行检测，当至少有两组且每组
    # 至少有两个以上非NA的值时，才进行ANOVA检验
    #+++++++++++++++++++++++++++++++++++++#
    testTmp <- tmpData |>
      dplyr::group_by(Group) |>
      dplyr::summarise(n = sum(!is.na(value)))
    if (sum(testTmp$n > 1) > 1) {
      lamp.acv <- aov(value ~ Group, data = tmpData)
      # print(lamp.acv)
      saov <- summary(lamp.acv)
      pvalue <- saov[[1]]$`Pr(>F)`[1]
      # post <- TukeyHSD(lamp.acv)
    } else {
      pvalue <- NA
    }
    res <- cbind(MeanRes, tibble::tibble(pvalue = pvalue))
  }
  return(res)
}


Diff <- function(dat, sampleInfo, compareInfo) {
  dat <- dplyr::rename(dat, ID = 1)

  compareInfo <- compareInfo |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Groups = stringr::str_split(Compare, "_vs_"),
      Method = dplyr::if_else(isPair, paste0("paired_", Method), paste0("unpaired_", Method))
    )

  diffResultsTmp <- list()

  for (i in 1:nrow(compareInfo)) {
    # i <- 1
    groups <- compareInfo$Groups[[i]]
    compare <- compareInfo$Compare[[i]]
    method <- compareInfo$Method[[i]]
    samples <- sampleInfo$Sample[sampleInfo$Group %in% groups]

    diffRes <- dat |>
      dplyr::select(ID, dplyr::all_of(samples)) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        res = Test(groups = groups, method = method, sampleInfo = sampleInfo)
      ) |>
      tidyr::unnest(res)
    diffResultsTmp[[compare]] <- diffRes
  }

  return(diffResultsTmp)
}
