
<!-- README.md is generated from README.Rmd. Please edit that file -->

This is a package aim to visualize the biological pathway.

# PathwayIlluminator

<!-- badges: start -->
<!-- badges: end -->

The goal of `PathwayIlluminator` is to make computational analysis more
exciting! It provides convenience functions to make some directl results
analysis.

## Installation

You can install the development version of PathwayIlluminator like so:

``` r
# install.packages("devtools")
devtools::install_github("yudalang3/PathwayIlluminator")
```

You can also install it from the `gitee`.

``` r
# install.packages("remotes")
# install.packages("git2r")
# The two package above should be installed
remotes::install_git("https://gitee.com/yudalang3/PathwayI.git")
```

## Vesion informations:

Here we only record the major features.

- Version 0.0.0.1000 released the first draft of the package.
- Version 0.0.0.1003 add the affine transformer and simulate the
  simplest model of lipid bilayer.
- Version 0.0.0.1004 put forward another way to draw the phospholipid
  bilayer, while some flaws exists.
- Version 0.0.0.1005 implements lots of phospholipid bilayer models,
  from the simplest two lines and the most complex curves.
- Version 0.0.0.1006 implements a simple general signaling pathway.
- Version 0.0.0.1007 Progress has been made in stages, write the
  mandatory vignettes.
- Version 0.0.0.1008
  完成整个API调用的功能，用最简单的一个类，加上`+`操作符的重载来实现，对于用户来说，简便性大大加强
- Version 0.0.0.1009 新增pTitle与pScaleColor 的API调用函数
- Version 0.0.0.1010
  意识到以R这种脚本化形式调用绘图功能的局限性，改用JAVA开发GUI界面，初步实现基本的外观
- Version 0.0.0.1011
  完成GUI中的椭圆，矩形等的基本功能，包括使用拖拽方式来绘制，使用拖拽来移动图形元素等。
- Version 0.0.0.1012 新增图形元件，新增GUI拖拽手势
- Version 0.0.0.1013 GUI界面软件新增导入剪切板图像的功能
- Version 0.0.0.1014
  GUI界面软件新增导出R代码的功能，用户可以直接粘贴到R中配合R包直接使用
- Version 0.0.0.1015 GUI界面软件新增橡皮擦功能，也添加图标
- Version 0.0.0.1016
  GUI界面软件新增grid辅助线功能，也就是辅助定位的标尺，展示形式是井字的网格线
- Version 0.0.0.1017 GUI界面软件添加不同的颜色并且在右上角新增图例
- Version 0.0.0.1018
  GUI界面软件新增一种持久化保存的文件格式`pathIll`，用户可以保存自己绘制的通路骨架
- Version 0.0.0.1019 GUI界面软件实现对`pathIll`文件格式的保存和读取
- Version 0.0.0.1020 GUI界面软件完善导出R代码的功能，用户可以勾选auto
  copy的checkbox按钮，从而不必弹出图形界面
- Version 0.0.0.1021 GUI界面软件新增打开License面板的按钮
- Version 0.0.0.1022 GUI界面软件在左侧控制面板下方新增Tutorial
- Version 0.0.0.1023 GUI界面软件完成橡皮擦右键取消的功能
- Version 0.0.0.1024 GUI界面软件 Drawing
  Panel左上角新增当前画板的长度和宽度指示信息
- Version 0.0.0.1025 GUI界面软件对细胞膜和核膜线条加粗显示
- Version 0.0.0.1026 GUI界面软件实现对 free
  arrow之外所有图形元素的点击拖拽功能
- Version 0.0.0.1027 将JAVA
  GUI软件打入R包中的inst/java目录，随R一起分发。
- Version 0.0.0.1028
  编写PathwayIlluminator_licenseTerms.txt文件，该文件的说明在用户第一次启动的时候会提醒。
- Version 0.0.0.1029 完善draw PPAR通路绘制的教程文档
- Version 0.0.0.1030
  新增多个BioGraphicsNode模版，见`basic_bioGraphics_templates`数据。
- Version 0.0.0.1031 修复`nuclearEnvelop()`
  绘制函数中，shape参数为ellipse时的过大效应。
  当绘制一个单位正方形的是时候，单位是1；但是当绘制一个标准的单位圆的时候，单位是0.5
- Version 0.0.0.1032
  为了增加细胞膜磷脂双分子层绘制的美观性，我们将输入的width和height
  仅保留一位小数。
- Version 0.0.0.1033 修复 `do_bilateral_extension_alongCurve()`
  方法中计算的一个bug，并添加能够首尾相连形式计算的功能。
- Version 0.0.0.1034 修复调用
  dplyr::distanc方法的bug，以后要注意进行浮点数运算的问题
- Version 0.0.0.1035 开始构建ggplot2 style的API 调用接口
- Version 0.0.0.1036 完成使用`+`为基础的 ggplot2 style API调用方式
- Version 0.0.0.1037 略微调整DNA字符串的y坐标，使之和JAVA
  GUI版本效果一致
- Version 0.0.0.1038 修复JAVA GUI界面拖动“滑块现象”

## More information:

**Please keep eyes on me for the first news.**

<figure>
<img src="yudalang_subscription%20account.png"
alt="Wechat subscription" />
<figcaption aria-hidden="true">Wechat subscription</figcaption>
</figure>
