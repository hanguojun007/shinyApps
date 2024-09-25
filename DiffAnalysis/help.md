# 差异分析使用说明

## 输入

### 文件格式

* csv: 逗号分隔的文本文件。
* tsv: tab分隔的文本文件。
* xlsx: excel文件。

### 文件内容

#### <a href="https://github.com/hanguojun007/shinyApps/blob/main/DiffAnalysis/exprMatrix.csv" target="_blank">exprMatrix</a>


| gene    | Feature1     | Feature2     | Feature3     |
| ------- | ------------ | ------------ | ------------ |
| Fth1    | 1.262954285  | 0.781859185  | -1.045717652 |
| Gabarap | 0.414641434  | -1.130385778 | 0.775634319  |
| Ckb     | -1.539950042 | 0.576718782  | 1.557370376  |

* 文件名固定为exprMatrix。
* 列名不做要求，列顺序为：第一列是特征名列，其余列是样本列。

#### <a href="https://github.com/hanguojun007/shinyApps/blob/main/DiffAnalysis/sampleInfo.csv" target="_blank">sampleInfo</a>

| Sample   | Group |
| -------- | ----- |
| Feature1 | A     |
| Feature2 | A     |
| Feature3 | A     |
| Feature4 | B     |
| Feature5 | B     |
| Feature6 | B     |

* 文件名固定为sampleInfo
* 第一列为样本列，列名固定。
* 第二列为分组列，列名固定。

#### <a href="https://github.com/hanguojun007/shinyApps/blob/main/DiffAnalysis/compareInfo.csv" target="_blank">compareInfo</a>

| Compare | Method | isPair |
| ------- | ------ | ------ |
| B_vs_A  | t.test | FALSE  |

* 文件名固定为compareInfo
* 第一列是比较组别，列名固定：组别使用 `_vs_` 连接。
* 第二列是比较方法，列名固定：可填 `t.text`，`wilcox.test`，`aov`。
* 第三列是配对信息，列名固定：可填 `TRUE`，`FALSE。`

## 使用方法

### 上传三文件

假如你按上述要求配置了三个文件，只需：`上传文件` -->  `开始差异分析` --> `下载差异分析结果`。

### 仅上传exprMatrix

如果你只上传exprMatrix，则需要：点击 `设置分组` 添加分组信息，点击 `添加比较组` 添加比较组信息，然后 `开始差异分析` --> `下载差异分析结果`。
