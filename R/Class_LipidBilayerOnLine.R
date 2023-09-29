#' Get the rotation of the two points.
#'
#' @param x1
#' @param y1
#' @param x2
#' @param y2
#'
#' @return the rotation angle in radian
#' @export
#'
#' @examples
#' get_rotation_of_twoPoints(0,0,1,1)
get_rotation_of_twoPoints <- function(x1, y1, x2, y2) {
  horizontal_distance <- abs(x1 - x2)
  vertical_distance <- abs(y1 - y2)

  if (horizontal_distance == 0) {
    theta <- 90 * ONE_DEGREE_IN_RADIAN

  } else {
    theta <- atan2(vertical_distance, horizontal_distance)
  }

  return(theta)
}

#' @export
Phospholipid_drawer <-
  R6Class(
    classname = "Phospholipid_drawer",
    public = list(
      draw_lipid_along_curve = function(x, y) {
        grid.lines(
          x = x ,
          y = y ,
          default.units = 'in',
          gp = gpar(col = 'blue')
        )

        n <- length(x)

        walk2(
          1:(n - 1),
          2:n,
          .f = function(i1, i2) {
            x1 <- x[i1]
            y1 <- y[i1]
            x2 <- x[i2]
            y2 <- y[i2]

            # cat(sprintf("%g %g %g %g ", x1,y1,x2,y2))

            coefficient <- -1
            theta <-
              coefficient * get_rotation_of_twoPoints(x1, y1, x2, y2)

            # cat(sprintf("The rotation is %g\n", theta))

            points <- simulate_lipid_bilayer_model()
            self$draw_one_unit_lipid(points, theta, x1, y1)
            points <-
              simulate_lipid_bilayer_model(yAxisReflected = T)
            self$draw_one_unit_lipid(points,theta, x1, y1)
          }
        )
      },

      draw_one_unit_lipid = function(points, theta, moveX, moveY) {
        circle_points <-
          do_rotate_affine(points$circle_points, theta = theta)
        left_line <-
          do_rotate_affine(points$left_line, theta = theta)
        right_line <-
          do_rotate_affine(points$right_line, theta = theta)


        circle_points <-
          do_translate_affine(circle_points, moveX, moveY)
        left_line <-
          do_translate_affine(left_line, moveX, moveY)
        right_line <-
          do_translate_affine(right_line, moveX, moveY)

        grid.lines(circle_points[1,], circle_points[2,], default.units = 'in')
        grid.lines(left_line[1,], left_line[2,], default.units = 'in')
        grid.lines(right_line[1,], right_line[2,], default.units = 'in')



      }
    )
  )
