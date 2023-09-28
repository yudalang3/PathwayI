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

points <- simulate_lipid_bilayer_model()
circle_points <- do_translate_affine(points$circle_points, moveX = 4, moveY = 4)
left_line <- do_translate_affine(points$left_line, moveX = 4, moveY = 4)
right_line <- do_translate_affine(points$right_line, moveX = 4, moveY = 4)

grid.newpage()

grid.lines(circle_points[1,],circle_points[2,], default.units = 'in')
grid.lines(left_line[1,], left_line[2,], default.units = 'in')
grid.lines(right_line[1,], right_line[2,], default.units = 'in')



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

