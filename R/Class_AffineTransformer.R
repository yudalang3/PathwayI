TRANSFORM_MAT_TEMPELATE <-
  matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), byrow = T, ncol = 3)


#' The core process to do the affine transformation.
#'
#' @param input_mat the matrix that need to be transformed. The format is defined in the details.
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
#' The output matrix is a 2 * n dimension matrix.
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
#'
do_affine_transformation <- function(input_mat, transform_mat) {
  ret <- transform_mat %*% rbind(input_mat, 1)
  return(ret[1:2, ])
}

#' First scale, next rotate and finally translate.
#' This is a high-level convenient function.
#'
#' @param mat the input matrix
#' @param theta angle in radian
#' @param moveX translate x
#' @param moveY translate y
#' @param scaleWidth scale of width
#' @param scaleHeight  scale of height
#'
#' @return the transformed matrix
#' @export
#'
#' @examples
#' do_scale_rotate_translate_affineTransfor( your requirement)
do_scale_rotate_translate_affineTransfor <-
  function(mat, scaleWidth = 1,scaleHeight = 1, theta, moveX, moveY) {
    ret <-
      get_translate_transform_mat(moveX, moveY) %*% get_rotate_transform_mat(theta) %*% get_scale_transform_mat(scaleWidth, scaleHeight)

    ret <- do_affine_transformation(input_mat = mat,transform_mat = ret)
    return(ret)
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
do_translate_affine <- function(mat, moveX = 1, moveY = 1) {
  transform_mat <- get_translate_transform_mat(moveX,moveY);
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The low-level function for translate transforming
#'
#' @param moveX the x move distance
#' @param moveY the y move distance
#'
#' @return the transforming matrix not the transformed matrix
#' @export
#'
#' @examples
#' get_translate_transform_mat(2,1)
get_translate_transform_mat <- function(moveX = 1, moveY = 1) {
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1:2, 3] <- c(moveX, moveY)
  return(transform_mat)
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
do_scale_affine <- function(mat,
                            scaleWidth = 1,
                            scaleHeight = 1) {
  transform_mat <- get_scale_transform_mat(scaleWidth,scaleHeight)
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The low-level function for scale transforming
#'
#' @param scaleWidth scale in width
#' @param scaleHeight scale in height
#'
#' @return the transforming matrix not the transformed matrix
#' @export
#'
#' @examples
#' get_scale_transform_mat(2,1)
get_scale_transform_mat <- function(scaleWidth = 1,
                                    scaleHeight = 1) {
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1, 1] <- scaleWidth
  transform_mat[2, 2] <- scaleHeight
  return(transform_mat)
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
do_rotate_affine <- function(mat, theta = 0) {
  transform_mat <- get_rotate_transform_mat(theta)
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}

#' The low-level function for rotate transforming
#'
#' @param theta the rotate angle, in radian
#'
#' @return the transforming matrix not the transformed matrix
#' @export
#'
#' @examples
#' get_scale_transform_mat(2,1)
get_rotate_transform_mat <- function(theta = 0) {
  s <- sin(theta)
  c <- cos(theta)

  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1, 1:2] <- c(c, s)
  transform_mat[2, 1:2] <- c(-s, c)
  return(transform_mat)
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
do_reflection_origin <- function(mat) {
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1, 1] <- -1
  transform_mat[2, 2] <- -1
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
do_reflection_xAxis <- function(mat) {
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[2, 2] <- -1
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
do_reflection_yAxis <- function(mat) {
  transform_mat <- TRANSFORM_MAT_TEMPELATE
  transform_mat[1, 1] <- -1
  ret <- do_affine_transformation(mat, transform_mat)
  return(ret)
}



#' Produce the model circle points.
#'
#' @description
#' It has many applications:
#' 1. produce poly regular shapes: like triangles, rectangular and so on.
#' 2. just drawing a circle, with points.
#'
#' @param from : from angle in degree
#' @param by : by angle in degree
#' @param to : to angle in degree
#' @param break_points the angles points in degree
#' @param radius the radius of circle regardless of the coordinates.
#'
#' @details
#' The return values formats are:
#' a 2 x n matrix, first row is x axis, second row is y axis.
#'
#' The 0 angle is 0 o'clock in the clock, angle of 90 is the 3 o'clock, angle of 180 is the 6 o'clock.
#'
#' @return matrix, see details
#' @export
#'
#' @examples
#' a <- produce_model_coordinate_points(from = 0,to = 160,by = 30,radius = 1)
#' xCenter <- 3; yCenter <- 3
#' xLocations <- a[1, ] + xCenter;
#' yLocations <- a[2, ] + yCenter;
#'
#' grid.newpage()
#' grid.lines(x = xLocations, y = yLocations, default.units = 'in')
produce_model_coordinate_points <-
  function(from = 0,
           by = 5,
           to = 90,
           break_points = NULL,
           radius = 1) {
    if (is.null(break_points)) {
      radians <-
        seq.int(from = from, by = by, to = to) * ONE_DEGREE_IN_RADIAN
    } else {
      radians <- break_points
    }

    cc <- exp((1i) * radians)
    matrixs <- rbind(Im(cc) * radius , Re(cc) * radius)

    return(matrixs)
  }
