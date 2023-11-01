#' The phospholipid model drawer, it can draw phospholipid along the curve.
#' @description
#' The draw of the phospholipid is repeated, so it should has a drawer class.
#'
#' @export
PhospholipidBilayerDrawer <- R6Class(
  "PhospholipidBilayerDrawer",
  public = list(
    #' @field current_lipid_model The model of the lipid
    current_lipid_model = NULL,
    #' @field lipid_head_par The graphics par of the header
    lipid_head_par = gpar(fill = 'lightblue', col = 'grey30'),
    #' @field lipid_line_par The graphics par of the line
    lipid_line_par = gpar(),
    #' @field desired_hight The height in inch's
    desired_hight = 0.2,
    #' @field percentage_of_off_track The percentage off track, higher the white line thicker, should >0 & <1.
    percentage_of_off_track = 0.05,
    #' @field draw_circle_head most draw head as a circle in a curve
    draw_circle_head = F,
    #' @field draw_only_upLayer draw only the up layer
    draw_only_upLayer = F,
    #' @field painting_parameters store for the parameters for painting. (for more complex functions it can be used)
    painting_parameters = list(),

    #' calculate lipid along curve, return the needed values for ploting.
    #'
    #' @param lineX the x axis points
    #' @param lineY the y axis points
    #' @param closedPolygon the input x,y line is a polygon
    #'
    #' @return the list of calculated values
    #' @export
    #'
    #' @examples
    #' calculate_lipid_along_curve(1:5,1:5)
    calculate_lipid_along_curve = function(lineX, lineY, closedPolygon = F) {
      checkCurvePoints(lineX, lineY)
      checkCurvePoints(lineX, lineY)

      ## total height is 100 and the radius is 22
      circleCenter_toHeight_ratio <- 0.78

      scaleHeight <- self$desired_hight / 100
      # d = self$desired_hight * circleCenter_toHeight_ratio + 0.01
      # The multiplication is the more universal solution
      d = self$desired_hight * circleCenter_toHeight_ratio * (1 + self$percentage_of_off_track)
      xyPrime <-
        do_bilateral_extension_alongCurve(lineX, lineY, d, closedPolygon = closedPolygon)

      ## layer 1
      if (closedPolygon) {
        x = c(xyPrime$xPrime_1, xyPrime$xPrime_1[1])
        y = c(xyPrime$yPrime_1, xyPrime$yPrime_1[1])
      } else {
        x = xyPrime$xPrime_1
        y = xyPrime$yPrime_1
      }


      len_ofX <- length(x)
      ## format : for each row are:
      ## x_a_1, y_a_1, x_b_1, y_b_1
      df_layer1 <-
        rbind(x[1:(len_ofX - 1)], y[1:(len_ofX - 1)], x[2:len_ofX], y[2:len_ofX])

      half_distance_all_1 <-
        0.5 * distance(df_layer1[1,], df_layer1[2,], df_layer1[3,], df_layer1[4,])

      scaleWidth_1 <- half_distance_all_1 / 22 #The radius is 22

      ## layer 2
      if (closedPolygon) {
        x = c(xyPrime$xPrime_2, xyPrime$xPrime_2[1])
        y = c(xyPrime$yPrime_2, xyPrime$yPrime_2[1])
      } else {
        x = xyPrime$xPrime_2
        y = xyPrime$yPrime_2
      }
      df_layer2 <-
        rbind(x[1:(len_ofX - 1)], y[1:(len_ofX - 1)], x[2:len_ofX], y[2:len_ofX])

      half_distance_all_2 <-
        0.5 * distance(df_layer2[1,], df_layer2[2,], df_layer2[3,], df_layer2[4,])
      scaleWidth_2 <- half_distance_all_2 / 22 # the radius is 22



      ## configure the draw length
      num_of_phospholipid_toDraw <- 1:(len_ofX - 1)

      theta_list <- map_dbl(
        num_of_phospholipid_toDraw,
        .f = function(i) {
          x1 <- df_layer1[1, i]
          y1 <- df_layer1[2, i]
          x2 <- df_layer1[3, i]
          y2 <- df_layer1[4, i]

          theta1 <- get_rotation_of_twoPoints(x1, y1, x2, y2)
          return(theta1)
        }
      )

      self$painting_parameters <-
        list(
          theta_list = theta_list,
          scaleWidth_1 = scaleWidth_1,
          scaleWidth_2 = scaleWidth_2,
          scaleHeight = scaleHeight,
          df_layer1 = df_layer1,
          df_layer2 = df_layer2,
          half_distance_all_1 = half_distance_all_1,
          half_distance_all_2 = half_distance_all_2
        )

      return(self$painting_parameters)
    },

    #' @description
    #' Input the curve line on the x,y parameters, the lipid will correctly draw along the line.
    #'
    #' @param lineX x values
    #' @param lineY y values
    #' @param closedPolygon the input x,y line is a polygon
    #'
    #' @description
    #' It will handle the angle automatically. Note the denser the dots, the more they are drawn
    #'
    draw_lipid_along_curve = function(lineX, lineY, closedPolygon = F) {
      # An iteration to draw lipid model one point
      # Default mode: the first point is not

      ret <-
        self$calculate_lipid_along_curve(lineX, lineY, closedPolygon)

      scaleHeight <- ret$scaleHeight
      thetaList <- ret$theta_list
      scaleWidth_1 <- ret$scaleWidth_1
      scaleWidth_2 <- ret$scaleWidth_2
      df_layer1 <- ret$df_layer1
      df_layer2 <- ret$df_layer2
      half_distance_all_1 <- ret$half_distance_all_1
      half_distance_all_2 <- ret$half_distance_all_2

      num_of_phospholipid_toDraw <- 1:ncol(df_layer1)
      walk(
        num_of_phospholipid_toDraw,
        .f = function(i) {
          x1 <- df_layer1[1, i]
          y1 <- df_layer1[2, i]
          x2 <- df_layer1[3, i]
          y2 <- df_layer1[4, i]

          theta1 <- thetaList[i]

          r = half_distance_all_1[i]
          self$draw_one_unit_lipid(scaleWidth_1[i], scaleHeight, theta = theta1, x1, y1, r)

          if (!self$draw_only_upLayer) {
            x1 <- df_layer2[1, i]
            y1 <- df_layer2[2, i]
            x2 <- df_layer2[3, i]
            y2 <- df_layer2[4, i]

            theta2 <- theta1 + degree_of_180_inRadian
            r = half_distance_all_2[i]
            self$draw_one_unit_lipid(scaleWidth_2[i], scaleHeight, theta = theta2, x1, y1, r)
          }
          return(theta1)
        }
      )
    },

    #' Draw one unit lipid model
    #'
    #' @param theta angle in radian
    #' @param moveX the moved x distance
    #' @param moveY the moved y distance
    #' @param scaleWidth the width scale factor
    #' @param scaleHeight the height scale factor
    #' @param r the unit for the header circle, if `draw_circle_head=T`, this value should be assigned
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
                                   moveY,
                                   r = NULL) {
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

      grid.lines(left_line[1,], left_line[2,], default.units = 'in',gp = self$lipid_line_par)
      grid.lines(right_line[1,], right_line[2,], default.units = 'in', gp = self$lipid_line_par)

      if (self$draw_circle_head) {
        if (is.null(r)) {
          stop('You need to provide the r, because the draw_circle_head=T')
        } else {
          grid.circle(
            x = moveX,
            y = moveY,
            r = r,
            default.units = 'in',
            gp = self$lipid_head_par
          )
        }
      } else {
        grid.polygon(
          circle_points[1, ],
          circle_points[2, ],
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
create_lipidBilayer_drawer <-
  function(lipidModelIndex = 1,
           draw_circle_head = F) {
    painter <- PhospholipidBilayerDrawer$new()
    painter$current_lipid_model <- lipid_models[[lipidModelIndex]]
    painter$draw_circle_head <- draw_circle_head

    return(painter)
  }
