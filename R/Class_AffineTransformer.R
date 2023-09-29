
TRANSFORM_MAT_TEMPELATE <- matrix(c(1,0,0,0,1,0,0,0,1), byrow = T, ncol = 3)


produce_model_coordinate_points <- function(from =0, by = 5, to = 90, break_points = NULL, radius = 1 ){

  if (is.null(break_points)) {
    radians <- seq.int( from = from, by = by, to = to) * ONE_DEGREE_IN_RADIAN
  }else {
    radians <- break_points
  }

  cc <- exp((1i) * radians)
  matrixs <- rbind(Im(cc) * radius , Re(cc) * radius)

  return(matrixs)
}


#' The core process to do the affine transformation.
#'
#' @param mat the matrix that need to be transformed. The format is defined in the details.
#' @param transform_mat the transform matrix, always should be a 3 x 3 diminsion.
#' @description
#'
#' The algorithm is referenced from https://blog.csdn.net/u011681952/article/details/98942207
#'
#' Be caution of the location of matrix and transform matrix.
#'
#' @details
#' The input matrix is defined as a 2 * n dimension.
#'
#' For example, if you have 3 points, you should input a 2 x 3 matrix.
#'
#' The output matrix is a 3 * n dimension matrix.
#'
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#'
# simulate to get data
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' # we config a
#' transform_mat_tempelate <- matrix(c(1,0,0,0,1,0,0,0,1), byrow = T, ncol = 3)
#' transform_mat <- transform_mat_tempelate
#' transform_mat[1:2, 3] <- c(2,2)
#' # to do the transformation
#' after_mat <- do_affine_transformation(mat,transform_mat)
#'
#' ## you can visulize the results
#' library(grid)
#' grid.newpage()
#' grid.lines(x =mat[1,], y = mat[2,], default.units = 'in' )
#' after_mat <- do_affine_transformation(mat,transform_mat)
#' grid.lines(x =after_mat[1,], y = after_mat[2,], default.units = 'in' )
do_affine_transformation <- function(mat, transform_mat){
  ret <- transform_mat %*% rbind(mat,1)
  return(ret[1:2,])
}

#' The core process to do the translate affine transformation.
#'
#' @param mat the matrix that need to be transformed
#' @param moveX the x move distance
#' @param moveY the y move distance
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_translate_affine(mat, 1,1)
#'
do_translate_affine <- function(mat, moveX = 1, moveY = 1){
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1:2, 3] <- c(moveX,moveY)
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}



# Complete all transformation ---------------------------------------------
# The shear transformation is not have a convient function for it is not common to use.


#' The core process to do the scale affine transformation.
#'
#' @param mat the matrix that need to be transformed
#' @param scaleWidth scale in width
#' @param scaleHeight scale in height
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_scale_affine(mat, 1,1)
#'
do_scale_affine <- function(mat, scaleWidth = 1, scaleHeight = 1){
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1,1] <- scaleWidth
  transform_mat[2,2] <- scaleHeight
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The core process to do the rotate affine transformation.
#'
#' @param mat the matrix that need to be transformed
#' @param theta the rotate angle, in radian
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_rotate_affine(mat, 1,1)
#'
do_rotate_affine <- function(mat, theta = 0){

  s <- sin(theta)
  c <- cos(theta)

  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1, 1:2] <- c(c,s)
  transform_mat[2, 1:2] <- c(-s, c)
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The core process to do the reflection affine transformation.
#' The reflection is about the orgin (0 , 0)
#' @param mat the matrix that need to be transformed
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_reflection_origin(mat)
#'
do_reflection_origin <- function(mat){
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1,1] <- -1
  transform_mat[2,2] <- -1
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}


#' The core process to do the reflection affine transformation.
#' The reflection is about the x axis
#' @param mat the matrix that need to be transformed
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_reflection_origin(mat)
#'
do_reflection_xAxis <- function(mat){
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[2,2] <- -1
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The core process to do the reflection affine transformation.
#' The reflection is about the y axis
#' @param mat the matrix that need to be transformed
#'
#' @return the transformed matrix. the row number is 2, not three.
#' @export
#'
#' @examples
#' mat <- rbind(c(1,0,1),c(2,1,0))
#' do_reflection_origin(mat)
#'
do_reflection_yAxis <- function(mat){
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1,1] <- -1
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}




#' Simulate the lipid bilayer model.
#'
#' @return a list
#' @###export
#'
#' @examples
#' simulate_lipid_bilayer_model()
simulate_lipid_bilayer_model <- function(yAxisReflected = F) {
  from_angle <- -160
  circle_points <- produce_model_coordinate_points(from = from_angle, by = 1, to = from_angle +360, radius = 0.3)
  x_horizontal_distance <- 0.1
  y_vertical_distance <- 0.4

  first_point <- circle_points[,1]
  middle_point <- first_point + c(-x_horizontal_distance, - y_vertical_distance)

  last_point <- first_point
  last_point[2] <- middle_point[2] - y_vertical_distance

  left_line <- cbind(first_point, middle_point, last_point)
  right_line <- left_line
  right_line[1,] <- right_line[1,] + 2 * abs(first_point[1])

  # Adjust for the center
  xAdjust <- 0; yAdjust <- abs(last_point[2]) * 1.01
  left_line[2, ] <- left_line[2, ] + yAdjust
  right_line[2, ] <- right_line[2, ] + yAdjust
  circle_points[2, ] <- circle_points[2, ] + yAdjust


  if (yAxisReflected) {
    left_line = do_reflection_xAxis(left_line)
    right_line = do_reflection_xAxis(right_line)
    circle_points = do_reflection_xAxis(circle_points)
  }

  ret <- list(left_line = left_line, right_line = right_line,  circle_points = circle_points)
  return(ret)
}


