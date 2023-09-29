mat <- rbind(c(1,0,1),c(2,1,0))
# we config a
transform_mat_tempelate <- matrix(c(1,0,0,0,1,0,0,0,1), byrow = T, ncol = 3)
transform_mat <- transform_mat_tempelate
transform_mat[1:2, 3] <- c(2,2)
# to do the transformation
after_mat <- do_affine_transformation(mat,transform_mat)

## you can visulize the results
library(grid)
grid.newpage()
grid.lines(x =mat[1,], y = mat[2,], default.units = 'in' )
after_mat <- do_affine_transformation(mat,transform_mat)
grid.lines(x =after_mat[1,], y = after_mat[2,], default.units = 'in' )
after_mat2 <- do_rotate_affine(after_mat[1:2,], theta = 1/10 * pi)
grid.lines(x =after_mat2[1,], y = after_mat2[2,], default.units = 'in' )

after_mat3 <- do_rotate_affine(after_mat[1:2,], theta = 1/4 * pi)
grid.lines(x =after_mat3[1,], y = after_mat3[2,], default.units = 'in' )



## draw the model with translation

w_and_h <- par('din')

centerLocation <- 0.5 * w_and_h

moveX <- centerLocation[1]; moveY <- centerLocation[2]

points <- simulate_lipid_bilayer_model()
circle_points <- do_translate_affine(points$circle_points, moveX = moveX, moveY = moveY)
left_line <- do_translate_affine(points$left_line, moveX = moveX, moveY = moveY)
right_line <- do_translate_affine(points$right_line, moveX = moveX, moveY = moveY)

grid.newpage()

grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
grid.lines(left_line[1,], left_line[2,], default.units = 'in')
grid.lines(right_line[1,], right_line[2,], default.units = 'in')

# draw the coordinate
coor_arrow <- arrow(ends = 'both',length = unit(0.1, "inches"))
coor_gpar <- gpar(lty = 'dashed')
grid.segments(
  moveX - 1,
  moveY,
  moveX + 1 ,
  moveY ,
  default.units = 'in',
  arrow = coor_arrow,
  gp = coor_gpar
)
grid.segments(moveX,
              moveY + 1,
              moveX ,
              moveY - 1,
              default.units = 'in' ,
              arrow = coor_arrow,
              gp = coor_gpar)
grid.text(
  "(0 , 0)",
  x = moveX,
  y = moveY,
  default.units = 'in',
  hjust = 0,
  vjust = 1
)

## draw the model with reflection

points <- simulate_lipid_bilayer_model()

circle_points <- do_reflection_xAxis(points$circle_points)
left_line <- do_reflection_xAxis(points$left_line)
right_line <- do_reflection_xAxis(points$right_line)


circle_points <- do_translate_affine(circle_points, moveX = 4, moveY = 4)
left_line <- do_translate_affine(left_line, moveX = 4, moveY = 4)
right_line <- do_translate_affine(right_line, moveX = 4, moveY = 4)


grid.newpage()

grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
grid.lines(left_line[1,], left_line[2,], default.units = 'in')
grid.lines(right_line[1,], right_line[2,], default.units = 'in')



## draw the model with rotation
points <- simulate_lipid_bilayer_model()

angle <- 45 * ONE_DEGREE_IN_RADIAN
circle_points <- do_rotate_affine(points$circle_points,theta = angle)
left_line <- do_rotate_affine(points$left_line,theta = angle)
right_line <- do_rotate_affine(points$right_line,theta = angle)


circle_points <- do_translate_affine(circle_points, moveX = 4, moveY = 4)
left_line <- do_translate_affine(left_line, moveX = 4, moveY = 4)
right_line <- do_translate_affine(right_line, moveX = 4, moveY = 4)


grid.newpage()

grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
grid.lines(left_line[1,], left_line[2,], default.units = 'in')
grid.lines(right_line[1,], right_line[2,], default.units = 'in')



### generate bilayers on the line


x <- seq(from = 1, to = 6, by = 0.6)
# the simplest form of the line
y <- x ^ (1 /3) + 1
# y <- x

n <- length(x)

grid.newpage()

grid.lines(x=x ,y = y , default.units = 'in',gp = gpar(col = 'blue'))
walk2(1:(n-1), 2:n, .f = function(i1, i2){
  x1 <- x[i1]; y1 <- y[i1]
  x2 <- x[i2]; y2 <- y[i2]

  cat(sprintf("%g %g %g %g ", x1,y1,x2,y2))

  coefficient <- -1
  theta <- coefficient * get_rotation_of_twoPoints(x1,y1,x2,y2)

  cat(sprintf("The rotation is %g\n", theta))

  points <- simulate_lipid_bilayer_model()

  circle_points <- do_rotate_affine(points$circle_points,theta = theta)
  left_line <- do_rotate_affine(points$left_line,theta = theta)
  right_line <- do_rotate_affine(points$right_line,theta = theta)


  circle_points <- do_translate_affine(circle_points, moveX = x1, moveY = y1)
  left_line <- do_translate_affine(left_line, moveX = x1, moveY = y1)
  right_line <- do_translate_affine(right_line, moveX = x1, moveY = y1)

  grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
  grid.lines(left_line[1,], left_line[2,], default.units = 'in')
  grid.lines(right_line[1,], right_line[2,], default.units = 'in')



  # step1: doing the x-axis reflection affine transform
  circle_points <- do_reflection_xAxis(points$circle_points)
  left_line <- do_reflection_xAxis(points$left_line)
  right_line <- do_reflection_xAxis(points$right_line)

  circle_points <- do_rotate_affine(circle_points,theta = theta)
  left_line <- do_rotate_affine(left_line,theta = theta)
  right_line <- do_rotate_affine(right_line,theta = theta)


  circle_points <- do_translate_affine(circle_points, moveX = x1, moveY = y1)
  left_line <- do_translate_affine(left_line, moveX = x1, moveY = y1)
  right_line <- do_translate_affine(right_line, moveX = x1, moveY = y1)

  grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
  grid.lines(left_line[1,], left_line[2,], default.units = 'in')
  grid.lines(right_line[1,], right_line[2,], default.units = 'in')
})


### re organized with object orientated programming
x <- seq(from = 1, to = 6, by = 0.6)
# the simplest form of the line
y <- x ^ (1 /3) + 1
# y <- x
grid.newpage()
painter <- Phospholipid_drawer$new()
painter$draw_lipid_along_curve(x = x, y = y)


