#' Get the rotation of the two points.
#'
#' @param x1 point1 x
#' @param y1 point1 y
#' @param x2 point2 x
#' @param y2 point2 y
#'
#' @description
#' Note the return angle is already between 0 <= angle <= 90.
#'
#' <pre>
#'               point2 A.(x2,y2)       |       A
#'                                      |
#'    point1 B.(x1,y1)            C.    |    D       B           C
#' </pre>
#' Angle ABC is the returned value for left picture; angle ABD is the returned value in right picture
#'
#'
#' @return the rotation angle in radian
#' @export
#'
#' @examples
#' get_rotation_of_twoPoints(0,0,1,1)
get_rotation_of_twoPoints <- function(x1, y1, x2, y2) {
  # x_distance <- x1 - x2
  #
  # if (x_distance > 0) {
  #   direction <- -1
  # } else {
  #   direction <- -1
  # }
  #
  # horizontal_distance <- abs(x_distance)
  # vertical_distance <- abs(y1 - y2)
  #
  # if (horizontal_distance == 0) {
  #   theta <- 90 * ONE_DEGREE_IN_RADIAN
  #
  # } else {
  #   theta <- atan2(vertical_distance, horizontal_distance)
  # }
  #
  # return(direction * theta)

  return(atan2( y1 - y2, x2 - x1))
}


#' The phospholipid model drawer, it can draw phospholipid along the curve.
#' @description
#' The draw of the phospholipid is repated, so it should has a drawer class.
#'
#'
Phospholipid_drawer <- R6Class(
  "Phospholipid_drawer",
  public = list(
    #' @field shape_scaling_factor The scaling parameter, bigger value for big lipid in your eyes.
    shape_scaling_factor = 1,

    #' @description
    #' Input the curve line on the x,y parameters, the lipid will correctly draw along the line.
    #'
    #' @param x x values
    #' @param y y values
    #' @param lastDraw should draw last model, default F
    #'
    #' @description
    #' It will handle the angle automatically. Note the denser the dots, the more they are drawn
    #'
    draw_lipid_along_curve = function(x, y, lastDraw = F) {

      #check
      len_ofX <- length(x)
      if (length(y) != len_ofX) {
        stop("The length of x and y should be equal.")
      }
      if (len_ofX < 2) {
        stop("The number should greater than 2.")
      }

      x_distance <- abs(x[2] - x[1])
      y_distance <- abs(y[2] - y[1])
      gap = sqrt(x_distance * x_distance + y_distance * y_distance)


      self$shape_scaling_factor <- gap / simulate_lipid_bilayer_model()$head_diameter


      grid.lines(
        x = x ,
        y = y ,
        default.units = 'in',
        gp = gpar(col = 'blue')
      )

      n <- length(x)

      # An iteration to draw lipid model one point
      # Default mode: the first point is not
      walk2(
        1:(n - 1),
        2:n,
        .f = function(i1, i2) {
          x1 <- x[i1]
          y1 <- y[i1]
          x2 <- x[i2]
          y2 <- y[i2]

          # cat(sprintf("%g %g %g %g ", x1,y1,x2,y2))

          theta <- get_rotation_of_twoPoints(x1, y1, x2, y2)

          # cat(sprintf("The rotation is %g\n", theta))

          points <-
            simulate_lipid_bilayer_model()
          self$draw_one_unit_lipid(points, theta, x1, y1)
          points <-
            simulate_lipid_bilayer_model(yAxisReflected = T)
          self$draw_one_unit_lipid(points, theta, x1, y1)
        }
      )

      if (lastDraw) {
        x1 <- x[n-1]
        y1 <- y[n-1]
        x2 <- x[n]
        y2 <- y[n]

        # cat(sprintf("%g %g %g %g ", x1,y1,x2,y2))

        theta <- get_rotation_of_twoPoints(x1, y1, x2, y2)

        # cat(sprintf("The rotation is %g\n", theta))

        points <-
          simulate_lipid_bilayer_model()
        self$draw_one_unit_lipid(points, theta, x2, y2)
        points <-
          simulate_lipid_bilayer_model(yAxisReflected = T)
        self$draw_one_unit_lipid(points, theta, x2, y2)
      }
    },

    #' Draw one unit lipid model
    #'
    #' @param points A list contains circle_points/left_line/right_line
    #' @param theta angle in radian
    #' @param moveX the moved x distance
    #' @param moveY the moved y distance
    #'
    #' @export
    #'
    #' @examples
    #' x <- seq(from = 1, to = 6, by = 0.6)
    #' # the simplest form of the line
    #' y <- x ^ (1 /3) + 1
    #' # y <- x
    #' grid.newpage()
    #' painter <- Phospholipid_drawer$new()
    #' painter$draw_lipid_along_curve(x = x, y = y)
    draw_one_unit_lipid = function(points, theta, moveX, moveY) {
      scaler <- self$shape_scaling_factor

      circle_points <-
        do_scale_rotate_translate_affineTransfor(points$circle_points, scaler, theta, moveX, moveY)
      left_line <-
        do_scale_rotate_translate_affineTransfor(points$left_line, scaler, theta, moveX, moveY)
      right_line <-
        do_scale_rotate_translate_affineTransfor(points$right_line, scaler, theta, moveX, moveY)


      # circle_points <-
      #   do_scale_affine(points$circle_points, scaler, scaler)
      # left_line <-
      #   do_scale_affine(points$left_line, scaler, scaler)
      # right_line <-
      #   do_scale_affine(points$right_line, scaler, scaler)
      #
      #
      # circle_points <-
      #   do_rotate_affine(circle_points, theta = theta)
      # left_line <-
      #   do_rotate_affine(left_line, theta = theta)
      # right_line <-
      #   do_rotate_affine(right_line, theta = theta)
      #
      #
      # circle_points <-
      #   do_translate_affine(circle_points, moveX, moveY)
      # left_line <-
      #   do_translate_affine(left_line, moveX, moveY)
      # right_line <-
      #   do_translate_affine(right_line, moveX, moveY)

      grid.lines(circle_points[1, ], circle_points[2, ], default.units = 'in')
      grid.lines(left_line[1, ], left_line[2, ], default.units = 'in')
      grid.lines(right_line[1, ], right_line[2, ], default.units = 'in')

    }
  )
)
