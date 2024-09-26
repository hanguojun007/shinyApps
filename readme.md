# shinyApps

使用shiny搭建的用于进行一系列分析及绘图的工具。

## 使用它你需要准备：

* 本地安装R，参考：[R语言入门](https://mp.weixin.qq.com/s?__biz=MzkyMjc1NzEwMQ==&mid=2247483774&idx=1&sn=34641c5478731acda44f1f41bed75adb&chksm=c1ee33fcf699baea863cf6deb40b09214a4bd77fbbdd262ac3064b0e65d7e89062c46d780d67#rd)
* 必要的R包：[require.list
  ](https://github.com/hanguojun007/shinyApps/blob/main/require.list)

## 使用方法：

### runApp

下载文件夹到本地，使用shiny::runApp("path/shinyApps")

```R
library(shiny)
runApp("path/shinyApps")
```

### runGitHub

使用runGitHub直接运行

```
library(shiny)
runGitHub("hanguojun007/shinyApps")
```
