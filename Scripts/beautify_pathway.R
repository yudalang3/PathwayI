# C:\Users\yudal\Documents\BaiduSyncdisk\博士后工作开展\文献管理\具体文献\Wnt通路\我想要模仿的对象\mydata\fig2

width <- 644;
height <- 664;
df_node <- rbind(
  list('1', x = 86, y = 472, w = 48, h = 32, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('2', x = 59, y = 441, w = 64, h = 40, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('3', x = 104, y = 423, w = 52, h = 28, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('4', x = 219, y = 201, w = 52, h = 28, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('FZD_1', x = 148, y = 497, w = 96, h = 80, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('6', x = 129, y = 483, w = 16, h = 168, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('7', x = 150, y = 604, w = 32, h = 24, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('8', x = 174, y = 442, w = 72, h = 32, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('9', x = 124, y = 451, w = 52, h = 28, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('10', x = 65, y = 361, w = 70, h = 46, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('11', x = 55, y = 297, w = 70, h = 46, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('12', x = 151, y = 329, w = 70, h = 46, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('13', x = 258, y = 217, w = 52, h = 28, ratio = 0, tp=c(0.5, 0.5), 'roundRect')
)
colnames(df_node) <- c("label","x","y","width","height","inner_extension_ratio","text_position", "shape")
##############################
## call function here:
##############################
pathIllustrator <-  membrane(shape = 'roundRect', x = 27, y = 62, w = 519, h = 472) +
  nuclearEnvelop(shape = 'ellipse', x = 111, y = 99, w = 300, h = 198) +
  nuclearDNA(x = 172, y = 195,numOfRepeats = 1) +
  nodes(data = df_node)
pathIllustrator +
  pScaleCoord(oriWidth = width, oriHeight = height, destWidth = 5, destHeight = 5.16)
