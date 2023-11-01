## yudalang: This file is used for testing the lipid bi-layer model in the earlier development process
## from now, 20231101, we do not need this, please see the singlelipidMOdel along line.


#' Simulate the lipid bilayer model.
#'
#' ##return a list
#' #####export
#'
#' ####examples
#' simulate_lipid_bilayer_model()
simulate_lipid_bilayer_model <- function(yAxisReflected = F) {
  from_angle <- -160
  circle_points <-
    produce_model_coordinate_points(
      from = from_angle,
      by = 1,
      to = from_angle + 360,
      radius = 0.3
    )
  x_horizontal_distance <- 0.1
  y_vertical_distance <- 0.4

  first_point <- circle_points[, 1]
  middle_point <-
    first_point + c(-x_horizontal_distance, -y_vertical_distance)

  last_point <- first_point
  last_point[2] <- middle_point[2] - y_vertical_distance

  left_line <- cbind(first_point, middle_point, last_point)
  right_line <- left_line
  right_line[1, ] <- right_line[1, ] + 2 * abs(first_point[1])

  # Adjust for the center
  xAdjust <- 0
  yAdjust <- abs(last_point[2]) * 1.01
  left_line[2,] <- left_line[2,] + yAdjust
  right_line[2,] <- right_line[2,] + yAdjust
  circle_points[2,] <- circle_points[2,] + yAdjust


  if (yAxisReflected) {
    left_line = do_reflection_xAxis(left_line)
    right_line = do_reflection_xAxis(right_line)
    circle_points = do_reflection_xAxis(circle_points)
  }

  ret <-
    list(
      left_line = left_line,
      right_line = right_line,
      circle_points = circle_points,
      head_diameter = 0.6
    )
  return(ret)
}

# Draw one model phospholipid  --------------------------------------------


## draw the model with translation

w_and_h <- par('din')

centerLocation <- 0.5 * w_and_h

moveX <- centerLocation[1]; moveY <- centerLocation[2]

points <- simulate_lipid_bilayer_model()
circle_points <- do_translate_affine(points$circle_points, moveX = moveX, moveY = moveY)
left_line <- do_translate_affine(points$left_line, moveX = moveX, moveY = moveY)
right_line <- do_translate_affine(points$right_line, moveX = moveX, moveY = moveY)

pdf(file = 'simplest_lipid_model.pdf',width = w_and_h[1], height = w_and_h[2])
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
  x = moveX + 0.01,
  y = moveY - 0.01,
  default.units = 'in',
  hjust = 0,
  vjust = 1
)
dev.off()





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

## draw the model with scaling

points <- simulate_lipid_bilayer_model()

scalingFactor <- 0.5
circle_points <- do_scale_affine(points$circle_points, scalingFactor,scalingFactor)
left_line <- do_scale_affine(points$left_line, scalingFactor,scalingFactor)
right_line <- do_scale_affine(points$right_line, scalingFactor,scalingFactor)


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
