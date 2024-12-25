width <- 467;
height <- 557;

## curvature = 0 for direct line, 1 and -1 for the rectangular style.
## arrowLen is the length of the end point triangle size.
df_freeIndicator <- rbind(
  c(284.0,250.0,309.0,266.0,-1 ,0.1),
  c(323.0,153.0,346.0,167.0,-1 ,0.1),
  c(191.0,156.0,216.0,167.0,-1 ,0.1)
)
colnames(df_freeIndicator) <- c('x1','y1','x2','y2','curvature','arrowLen')
df_node <- rbind(
  list('Axin', x = 131, y = 432, w = 184, h = 40, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('1', x = 279, y = 420, w = 64, h = 40, ratio = 0, tp=c(0.5, 0.5), 'ellipse' ),
  list('2', x = 109, y = 432, w = 56, h = 40, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('3', x = 160, y = 452, w = 64, h = 40, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('4', x = 221, y = 469, w = 28, h = 28, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('5', x = 259, y = 472, w = 28, h = 28, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('6', x = 260, y = 451, w = 28, h = 28, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('7', x = 177, y = 507, w = 70, h = 38, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('8', x = 77, y = 359, w = 110, h = 46, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('9', x = 169, y = 373, w = 110, h = 46, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('10', x = 168, y = 342, w = 110, h = 46, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('11', x = 255, y = 360, w = 110, h = 46, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('12', x = 218, y = 178, w = 114, h = 42, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('13', x = 86, y = 178, w = 114, h = 42, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('14', x = 141, y = 260, w = 114, h = 42, ratio = 0, tp=c(0.5, 0.5), 'ellipse'),
  list('16', x = 99, y = 481, w = 70, h = 38, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('17', x = 249, y = 468, w = 18, h = 74, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('18', x = 149, y = 230, w = 100, h = 36, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('19', x = 97, y = 148, w = 92, h = 36, ratio = 0, tp=c(0.5, 0.5), 'roundRect'),
  list('20', x = 230, y = 148, w = 92, h = 36, ratio = 0, tp=c(0.5, 0.5), 'roundRect')
)
colnames(df_node) <- c("label","x","y","width","height","inner_extension_ratio","text_position", "shape")
##############################
## call function here:
##############################
pathIllustrator <-  membrane(shape = 'roundRect', x = 30, y = 43, w = 388, h = 456) +
  nuclearEnvelop(shape = 'ellipse', x = 60, y = 68, w = 316, h = 250) +
  nuclearDNA(x = 124, y = 238,numOfRepeats = 1) +
  nuclearDNA(x = 89, y = 144,numOfRepeats = 1) +
  nuclearDNA(x = 226, y = 144,numOfRepeats = 1) +
  edges(free_indicator_df = df_freeIndicator) +
  nodes(data = df_node)
pathIllustrator +
  pScaleCoord(oriWidth = width, oriHeight = height, destWidth = 5, destHeight = 5.96)


