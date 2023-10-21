
# Test for low level affine_transformation_functions ----------------------

mat <- rbind(c(1, 0, 1), c(2, 1, 0))
# we config a
transform_mat_tempelate <-
  matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), byrow = T, ncol = 3)
transform_mat <- transform_mat_tempelate
transform_mat[1:2, 3] <- c(2, 2)
# to do the transformation
after_mat <- do_affine_transformation(mat, transform_mat)

## you can visulize the results
library(grid)
grid.newpage()
grid.lines(x = mat[1, ],
           y = mat[2, ],
           default.units = 'in')
after_mat <- do_affine_transformation(mat, transform_mat)
grid.lines(x = after_mat[1, ],
           y = after_mat[2, ],
           default.units = 'in')
after_mat2 <- do_rotate_affine(after_mat[1:2, ], theta = 1 / 10 * pi)
grid.lines(x = after_mat2[1, ],
           y = after_mat2[2, ],
           default.units = 'in')

after_mat3 <- do_rotate_affine(after_mat[1:2, ], theta = 1 / 4 * pi)
grid.lines(x = after_mat3[1, ],
           y = after_mat3[2, ],
           default.units = 'in')




