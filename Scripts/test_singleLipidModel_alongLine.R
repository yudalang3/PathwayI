w_and_h <- par('din')
x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 35)
# the simplest form of the line
# y <- x ^ (1 /3) + 1
y <- rep(2,length(x))
grid.newpage()
painter <- create_lipidBilayer_drawer(1)
painter$current_lipid_model <- lipid_models[[1]];
painter$draw_lipid_along_curve(lineX = x, lineY = y)

# Draw the dynamic curves -------------------------------------------------
w_and_h <- par('din')
centerLocation <- 0.5 * w_and_h
moveX <- centerLocation[1]; moveY <- centerLocation[2]


grid.newpage()
curGlob <- curveGrob(x1 = 0.09, y1 = 0.5, x2 = 3, y2 = moveY,default.units = 'in', ncp = 8)
grid.draw(curGlob)

objGrob <- grobCoords(curGlob, closed = F)

x <- objGrob[[1]][[1]]$x
y <- objGrob[[1]][[1]]$y

indexes <- seq.int(from = 1, to = length(x), by = 10)
painter <- create_lipidBilayer_drawer()
grid.newpage()
painter$draw_lipid_along_curve(x[indexes],y[indexes])
#############################################################################
## Conclusion: The curveGrob function is hard to understand
## I should not consider it anymore
## would rather handle it by myself.
#############################################################################
#############################################################################



# Use bezier function to get the curve ------------------------------------

w_and_h <- par('din')
centerLocation <- 0.5 * w_and_h
moveX <- centerLocation[1]; moveY <- centerLocation[2]


grid.newpage()

vertical_up_length <- 1;

leaf_point <- create_point(1, moveY);
right_point <- create_point(w_and_h[1] - 1, moveY)
top_left_point <- create_point(moveX *0.5, moveY + vertical_up_length)
top_right_point <- create_point(moveX * 1.5, moveY + vertical_up_length)

bGrob <- BezierGrob(
  x = c(leaf_point@x, top_left_point@x, top_right_point@x, right_point@x),
  y =c(leaf_point@y, top_left_point@y, top_right_point@y, right_point@y),
  default.units = 'in',
  stepFn = function(...)
    seq(0, 1, length.out = 25)
)
pts <- BezierPoints(bGrob)
x <- pts$x
y <- pts$y
painter <- create_lipidBilayer_drawer()
grid.newpage()
painter$draw_lipid_along_curve(x,y)

######


# Sin and cos -------------------------------------------------------------
w_and_h <- par('din')

x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 40)
# the simplest form of the line
y <- sin(x) + 2
# y <- x
grid.newpage()
painter <- create_lipidBilayer_drawer(3)
painter$draw_circle_head <- F
painter$draw_lipid_along_curve(lineX = x, lineY = y, lastDraw = F)

### curve with this
x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 35)
# the simplest form of the line
y <- x ^ (1 /3) + 1
# y <- x
grid.newpage()
painter <- Phospholipid_drawer$new()
painter$current_lipid_model <- lipid_models[[1]];
painter$draw_lipid_along_curve(lineX = x, lineY = y)



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

saveGIF({
  for (i in 1:5) {
    from <- i * 1 - 1 ;
    to <- from + 365
    produce_circle_lipid(from, to)
  }
}, movie.name = 'sin_lipidBilayer_circle.gif', interval = 0.2)


### produce the animation

produce_picture <- function(a,t){
  w_and_h <- par('din')
  x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 40)
  # the simplest form of the line
  y <- sin(a * x + t) + 2
  # y <- x
  grid.newpage()
  painter <- Phospholipid_drawer$new()
  painter$current_lipid_model <- lipid_models[[1]];
  # painter$draw_circle_head = T;
  painter$draw_lipid_along_curve(lineX = x, lineY = y, lastDraw = F)
}

saveGIF({
  for (i in 1:20) {
    angle <- -10 * i * ONE_DEGREE_IN_RADIAN
    produce_picture(a = 1, t = angle)
  }
}, movie.name = 'sin_lipidBilayer.gif', interval = 0.2)


#################################################################
w_and_h <- par('din')
x <- seq(from = 1, to = w_and_h[1] - 1, length.out = 35)
# the simplest form of the line
#
grid.newpage()
for (ii in 1:6) {
  y <- rep(ii,length(x))
  painter <- create_lipidBilayer_drawer(ii)
  painter$draw_lipid_along_curve(lineX = x, lineY = y - 0.5)
}

