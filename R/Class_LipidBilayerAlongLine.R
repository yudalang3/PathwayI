# PhospholipidDrawer <- R6Class(
#   "PhospholipidDrawer",
#   public = list(
#     draw = function(my_model,
#                     scaleWidth,
#                     scaleHeight,
#                     theta,
#                     moveX,
#                     moveY) {
#       headPoints <- do_scale_rotate_translate_affineTransfor(
#         mat = my_model$headPoints,
#         scaleWidth =  1,
#         scaleHeight = 1,
#         theta = 45 * ONE_DEGREE_IN_RADIAN,
#         moveX = 2,
#         moveY = 2
#       )
#       leftLinePoints <-
#         do_scale_rotate_translate_affineTransfor(
#           my_model$leftLinePoints,
#           scaleWidth =  1,
#           scaleHeight = 1,
#           theta = 45 * ONE_DEGREE_IN_RADIAN,
#           moveX = 2,
#           moveY = 2
#         )
#       rightLinePoints <-
#         do_scale_rotate_translate_affineTransfor(
#           my_model$rightLinePoints,
#           scaleWidth =  1,
#           scaleHeight = 1,
#           theta = 45 * ONE_DEGREE_IN_RADIAN,
#           moveX = 2,
#           moveY = 2
#         )
#
#       draw_2xn_matrix_points(headPoints, leftLinePoints, rightLinePoints)
#     }
#   ))


#' The phospholipid model drawer, it can draw phospholipid along the curve.
#' @description
#' The draw of the phospholipid is repeated, so it should has a drawer class.
#'
PhospholipidBilayerDrawer <- R6Class(
  "PhospholipidBilayerDrawer",
  public = list(
    #' @field shape_scaling_factor The scaling parameter, bigger value for big lipid in your eyes.
    shape_scaling_factor = 1,
    #' @field current_lipid_model The model of the lipid
    current_lipid_model = NULL,
    #' @field lipid_head_par The graphics par
    lipid_head_par = gpar(fill = 'lightblue', col = 'grey30'),
    #' @field desired_hight The height in inch's
    desired_hight = 0.3,
    #' @field draw_circle_head most draw head as a circle in a curve
    draw_circle_head = F,

    #' @description
    #' Input the curve line on the x,y parameters, the lipid will correctly draw along the line.
    #'
    #' @param lineX x values
    #' @param lineY y values
    #' @param lastDraw should draw last model, default F
    #'
    #' @description
    #' It will handle the angle automatically. Note the denser the dots, the more they are drawn
    #'
    draw_lipid_along_curve = function(lineX, lineY, lastDraw = F) {
      checkCurvePoints(lineX, lineY)

      ## total height is 100 and the radius is 22
      circleCenter_toHeight_ratio <- 0.78

      scaleHeight <- self$desired_hight / 100
      d = self$desired_hight * circleCenter_toHeight_ratio + 0.01
      xyPrime <- do_bilateral_extension_alongCurve(lineX, lineY, d)
      ## layer 1
      x = xyPrime$xPrime_1
      y = xyPrime$yPrime_1

      len_ofX <- checkCurvePoints(x, y)

      ## format : for each row are:
      ## x_a_1, y_a_1, x_b_1, y_b_1
      df_layer1 <-
        rbind(x[1:(len_ofX - 1)], y[1:(len_ofX - 1)], x[2:len_ofX], y[2:len_ofX])

      half_distance_all_1 <-
        0.5 * distance(df_layer1[1, ], df_layer1[2, ], df_layer1[3, ], df_layer1[4, ])
      scaleWidth_1 <- half_distance_all_1 / 22 #The radius is 22
      ## layer 2
      x = xyPrime$xPrime_2
      y = xyPrime$yPrime_2
      df_layer2 <-
        rbind(x[1:(len_ofX - 1)], y[1:(len_ofX - 1)], x[2:len_ofX], y[2:len_ofX])

      half_distance_all_2 <-
        0.5 * distance(df_layer2[1, ], df_layer2[2, ], df_layer2[3, ], df_layer2[4, ])
      scaleWidth_2 <- half_distance_all_2 / 22 # the radius is 22

      degree_of_180_inRadian <- 180 * ONE_DEGREE_IN_RADIAN
      # An iteration to draw lipid model one point
      # Default mode: the first point is not
      walk(
        1:(len_ofX - 1),
        .f = function(i) {
          x1 <- df_layer1[1, i]
          y1 <- df_layer1[2, i]
          x2 <- df_layer1[3, i]
          y2 <- df_layer1[4, i]

          theta <- get_rotation_of_twoPoints(x1, y1, x2, y2)


          self$draw_one_unit_lipid(scaleWidth_1[i], scaleHeight, theta = theta, x1, y1)

          x1 <- df_layer2[1, i]
          y1 <- df_layer2[2, i]
          x2 <- df_layer2[3, i]
          y2 <- df_layer2[4, i]

          theta <- theta + degree_of_180_inRadian

          self$draw_one_unit_lipid(scaleWidth_2[i], scaleHeight, theta = theta, x1, y1)

        }
      )

      if (self$draw_circle_head) {
        grid.circle(
          x = df_layer1[1,],
          y = df_layer1[2,],
          r = half_distance_all_1,
          default.units = 'in',
          gp = self$lipid_head_par
        )
        grid.circle(
          x = df_layer2[1,],
          y = df_layer2[2,],
          r = half_distance_all_2,
          default.units = 'in',
          gp = self$lipid_head_par
        )
      }

      if (lastDraw) {
        x1 <- x[n - 1]
        y1 <- y[n - 1]
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
    #' @param theta angle in radian
    #' @param moveX the moved x distance
    #' @param moveY the moved y distance
    #' @param scaleWidth the width scale factor
    #' @param scaleHeight the height scale factor
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
    draw_one_unit_lipid = function(scaleWidth = 1,
                                   scaleHeight = 1,
                                   theta,
                                   moveX,
                                   moveY) {
      circle_points <-
        do_scale_rotate_translate_affineTransfor(self$current_lipid_model$headPoints,
                                                 scaleWidth,
                                                 scaleHeight,
                                                 theta,
                                                 moveX,
                                                 moveY)
      left_line <-
        do_scale_rotate_translate_affineTransfor(
          self$current_lipid_model$leftLinePoints,
          scaleWidth,
          scaleHeight,
          theta,
          moveX,
          moveY
        )
      right_line <-
        do_scale_rotate_translate_affineTransfor(
          self$current_lipid_model$rightLinePoints,
          scaleWidth,
          scaleHeight,
          theta,
          moveX,
          moveY
        )

      grid.lines(left_line[1, ], left_line[2, ], default.units = 'in')
      grid.lines(right_line[1, ], right_line[2, ], default.units = 'in')

      if (!self$draw_circle_head) {
        grid.polygon(
          circle_points[1,],
          circle_points[2,],
          default.units = 'in',
          gp = self$lipid_head_par
        )
      }

    }
  )
)


#' quick function to draw the instance.
#'
#' @param lipidModelIndex the index of model
#'
#' @return the drawer instance
#' @export
#'
#' @examples
#' create_lipidBilayer_drawer()
create_lipidBilayer_drawer <- function(lipidModelIndex = 1) {
  painter <- PhospholipidBilayerDrawer$new()
  painter$current_lipid_model <- lipid_models[[lipidModelIndex]]

  return(painter)
}
