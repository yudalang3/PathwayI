# Draw the dynamic curves -------------------------------------------------
# Warning: it still has the bugs: 20231025
w_and_h <- par('din')
centerLocation <- 0.5 * w_and_h
moveX <- centerLocation[1]; moveY <- centerLocation[2]


grid.newpage()
curGlob <- curveGrob(x1 = 1, y1 = 1, x2 = 3, y2 = moveY,default.units = 'in', ncp = 8)
grid.draw(curGlob)

objGrob <- grobCoords(curGlob, closed = F)

x <- objGrob[[1]][[1]]$x
y <- objGrob[[1]][[1]]$y

indexes <- seq.int(from = 1, to = length(x), by = 3)
# indexes <- 1:length(x)
painter <- create_lipidBilayer_drawer(draw_circle_head = F)
grid.newpage()
painter$draw_lipid_along_curve(x[indexes],y[indexes])
grid.lines(x[indexes],y[indexes],default.units = 'in')

grid.circle(x[indexes],y[indexes],default.units = 'in', r = unit(1,'pt'))

grid.newpage()

grid.lines(x = x, y = y, default.units = 'in', gp = gpar(col = 'purple'))
biLayer <- do_bilateral_extension_alongCurve(x =  x, y = y,d = 0.4)
grid.lines(x = biLayer$xPrime_1, y = biLayer$yPrime_1, default.units = 'in')
grid.lines(x = biLayer$xPrime_2, y = biLayer$yPrime_2, default.units = 'in', gp = gpar(col = 'green'))

#############################################################################
## Conclusion: The curveGrob function is hard to understand
## I should not consider it anymore
## would rather handle it by myself.
#############################################################################
#############################################################################

## circular points
produce_circle_lipid <- function(from = 0, to = 365){
  df_round <- produce_model_coordinate_points(from = from,to = to,radius = 2) + 2.5
  x <- df_round[1,] + 1
  y <- df_round[2,]
  # y <- x
  grid.newpage()
  painter <- create_lipidBilayer_drawer()
  painter$draw_circle_head <- T
  painter$current_lipid_model <- lipid_models[[1]];
  painter$draw_lipid_along_curve(lineX = x, lineY = y)
}
produce_circle_lipid()
# saveGIF({
#   for (i in 1:5) {
#     from <- i * 1 - 1 ;
#     to <- from + 365
#     produce_circle_lipid(from, to)
#   }
# }, movie.name = 'sin_lipidBilayer_circle.gif', interval = 0.2)

df_round <- produce_model_coordinate_points(from = 0,to = 364,by = 4,radius = 2) + 2.5
x <- df_round[1,] + 1
y <- df_round[2,]
# y <- x
grid.newpage()
painter <- create_lipidBilayer_drawer()
painter$draw_circle_head <- T
painter$current_lipid_model <- lipid_models[[1]];
painter$draw_lipid_along_curve(lineX = x, lineY = y)

### produce the animation

produce_picture <- function(a,t){
  w_and_h <- par('din')
  x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 40)
  # the simplest form of the line
  y <- sin(a * x + t) + 2
  # y <- x
  grid.newpage()
  painter <- create_lipidBilayer_drawer(draw_circle_head = T)
  painter$current_lipid_model <- lipid_models[[1]];
  # painter$draw_circle_head = T;
  painter$draw_lipid_along_curve(lineX = x, lineY = y)
}

# saveGIF({
#   for (i in 1:20) {
#     angle <- -10 * i * ONE_DEGREE_IN_RADIAN
#     produce_picture(a = 1, t = angle)
#   }
# }, movie.name = 'sin_lipidBilayer.gif', interval = 0.2)






