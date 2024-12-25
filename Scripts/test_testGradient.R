pdf("hi.pdf")

colours <- c("black", "white")
gradient <- linearGradient(colours,x1 = 0, y1 = 1)
grid.rect(width=.8, height=.4,
          gp=gpar(fill=gradient , col = NA))

dev.off()

pdf("evenodd.pdf")
grid.fill(circleGrob(r=c(.2, .4)), rule="evenodd",
          gp=gpar(fill=radialGradient()))
dev.off()

pdf("winding.pdf")
grid.fill(circleGrob(r=c(.2, .4)), rule="winding",
          gp=gpar(fill=radialGradient()))
dev.off()

pdf("test.gradient.pdf")

grid.roundrect(
  x = 1,
  y = 1,
  width = 0.8,
  height = 0.5,
  default.units = 'in',
  gp = gpar( col = 'black',
             fill = radialGradient(colours = c('white','#9DCFD5')))
)

grid.roundrect(
  x = 2,
  y = 1.2,
  width = 0.8,
  height = 0.5,
  default.units = 'in',
  gp = gpar( col = 'black',
             fill = radialGradient(colours = c('white','#9DCFD5')))
)
dev.off()




a <- circleGrob()

extract_xyCoordinate_points(a)
