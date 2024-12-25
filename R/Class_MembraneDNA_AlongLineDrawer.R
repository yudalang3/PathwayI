# PhospholipidBilayerDrawer -----------------------------------------------


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
    lipid_head_par = gpar(fill = NA, col = 'grey30'),
    # lipid_head_par = gpar(fill = 'lightblue', col = 'grey30'),
    #' @field lipid_line_par The graphics par of the line
    lipid_line_par = gpar(),
    #' @field desired_height The height in inch's
    desired_height = 0.2,
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

      # grid.lines(lineX,lineY,default.units = 'in', gp = gpar(col = 'orange'))

      ## total height is 100 and the radius is 22
      circleCenter_toHeight_ratio <- 0.78

      scaleHeight <- self$desired_height / 100
      # d = self$desired_height * circleCenter_toHeight_ratio + 0.01
      # The multiplication is the more universal solution
      d = self$desired_height * circleCenter_toHeight_ratio * (1 + self$percentage_of_off_track)
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

      # grid.lines(x,y,default.units = 'in', gp = gpar(col = 'green'))

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

      # grid.lines(x,y,default.units = 'in', gp = gpar(col = 'blue'))

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

      grid.lines(left_line[1,],
                 left_line[2,],
                 default.units = 'in',
                 gp = self$lipid_line_par)
      grid.lines(
        right_line[1,],
        right_line[2,],
        default.units = 'in',
        gp = self$lipid_line_par
      )

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


# RoundRectangularBiLayerDrawer -------------------------------------------

#' The round rectangule model drawer, it can draw rectangule along the curve.
#' @description
#' The draw of the rectangule is repeated, so it should has a drawer class.
#'
#' @export
RoundRectangularBiLayerDrawer <- R6Class(
  "RoundRectangularBiLayerDrawer",
  public = list(
    #' @field numOfIntervals the number of intervals, default is 10
    numOfIntervals = 10,
    #' @field percentageOfRectangle The percentage of the round rectangle, greater 0 and less than 1. Default is 0.7
    percentageOfRectangle = 0.7,
    #' @field roundRectGpar The graphics par of the round rectangle
    roundRectGpar = gpar(fill = NA),
    # roundRectGpar = gpar(fill = 'purple1' , col = NA),
    #' @field desired_height The height in inch's
    desired_height = 0.2,
    #' @field painting_parameters store for the parameters for painting. (for more complex functions it can be used)
    painting_parameters = list(),
    #' @field n_pointToSmoothOut see [soomthOut_twoSidesOf_quadrangle()]
    n_pointToSmoothOut = 2,
    #' @field n_step_bezier see [soomthOut_twoSidesOf_quadrangle()]
    n_step_bezier = 40,

    #' calculate lipid along curve, return the needed values for ploting.
    #'
    #' @param lineX the x axis points
    #' @param lineY the y axis points
    #'
    #' @return the list of calculated values
    #' @export
    #'
    #' @examples
    #' calculate_along_curve(1:5,1:5)
    calculate_along_curve = function(lineX, lineY) {
      lenOfInputData <- checkCurvePoints(lineX, lineY)

      count_of_intervals <- lenOfInputData %/% self$numOfIntervals
      count_of_drawingElements <-
        ceiling(count_of_intervals * self$percentageOfRectangle)

      start_vector_ofIntervals <-
        seq.int(from = 1, by = count_of_intervals, to = lenOfInputData)
      rectHeight <- self$desired_height
      xyPrime <-
        do_bilateral_extension_alongCurve(lineX, lineY, rectHeight, closedPolygon = F)

      listOf_df_forPainting <- map(
        .x = start_vector_ofIntervals,
        .f = function(s) {
          colIndexes <-
            seq.int(from = s,
                    by = 1,
                    length.out = count_of_drawingElements)
          ## Note: we reverse the points in the draw_along_curve() function
          df <-
            rbind(xyPrime$xPrime_1[colIndexes] ,
                  xyPrime$yPrime_1[colIndexes],
                  xyPrime$xPrime_2[colIndexes] ,
                  xyPrime$yPrime_2[colIndexes])
        }
      )

      self$painting_parameters <-
        list(lineX = lineX,
             lineY = lineY,
             listOf_df_forPainting = listOf_df_forPainting)
    },

    #' @description
    #' Input the curve line on the x,y parameters, the lipid will correctly draw along the line.
    #'
    #' @param lineX x values
    #' @param lineY y values
    #'
    #' @description
    #' It will handle the angle automatically. Note the denser the dots, the more they are drawn
    #'
    draw_along_curve = function(lineX, lineY) {
      self$calculate_along_curve(lineX, lineY)

      calculatedPars <- self$painting_parameters

      listOf_df_forPainting <- calculatedPars$listOf_df_forPainting

      draw_one_segment_round_rect <- function(df) {

        novelDF <- soomthOut_twoSidesOf_quadrangle(
          df = df,
          n_pointToSmoothOut = self$n_pointToSmoothOut,
          n_step_bezier = self$n_step_bezier
        )
        xx <- novelDF[1,]
        yy <- novelDF[2,]


        grid.polygon(
          x = xx ,
          y = yy,
          default.units = 'in',
          gp = self$roundRectGpar
        )
      }
      ## reverse the points here
      walk(
        listOf_df_forPainting,
        .f = draw_one_segment_round_rect
      )
    }

  )
)

#' quick function to create the instance.
#'
#'
#' @param numberOfIntervals intervals
#' @param percentageOfRectangle percentage of the round rectangle
#'
#' @return the drawer instance
#' @export
#'
#' @examples
#' create_roundRectBilayer_drawer()
create_roundRectBilayer_drawer <-
  function(numberOfIntervals = 10,
           percentageOfRectangle = 0.7) {
    painter <- RoundRectangularBiLayerDrawer$new()
    painter$numOfIntervals <- numberOfIntervals
    painter$percentageOfRectangle <- percentageOfRectangle

    return(painter)
  }


# DNA_DoubleHelixDrawer ---------------------------------------------------

#' The DNA double delix drawer, it can draw DNA along the curve.
#' @description
#' The draw of the delix is repeated, so it should has a drawer class.
#'
#' @export
DNA_DoubleHelixDrawer <- R6Class(
  "DNA_DoubleHelixDrawer",
  public = list(
    #' @field numOfRepeats the number of repeats, default is 8
    numOfRepeats = 8,
    #' @field listOfRibbonGpars The graphics par of the round rectangle
    listOfRibbonGpars = list(
      leftTop = gpar(fill = NA , col = 'black'),
      leftBottom = gpar(fill = NA , col = 'black'),
      rightTop = gpar(fill = NA , col = 'black'),
      rightBottom = gpar(fill = NA , col = 'black')
    ),
    # listOfRibbonGpars = list(
    #   leftTop = gpar(fill = '#0055D4' , col = NA),
    #   leftBottom = gpar(fill = '#AA0000' , col = NA),
    #   rightTop = gpar(fill = '#003380' , col = NA),
    #   rightBottom = gpar(fill = '#FF2A2A' , col = NA)
    # ),
    #' @field listOfHydrogenBondColors The colors of the hydrogen bonds, the number should be 1,2,4,8
    listOfHydrogenBondColors = c(NA, NA, NA, NA),
    # listOfHydrogenBondColors = c('#42210B', '#F7931E', '#9E005D', '#2E3192'),

    #' @field dna_template_instance The DNA template instance to paint.
    #' It includes the graphics points of the HydrogenBbond
    #' And the graphics par of the Ribbons
    dna_template_instance = NULL,

    #' @field desired_height The height in inch's
    desired_height = 0.2,
    #' @field painting_parameters store for the parameters for painting. (for more complex functions it can be used)
    painting_parameters = list(),

    #' @description
    #' Input the point of the location and it will draw the double helix
    #'
    #' @param x x location fro drawing, in inchs
    #' @param y y location fro drawing, in inchs
    #'
    #' @description
    #' It will handle the angle automatically. Note the denser the dots, the more they are drawn
    #'
    draw_me = function(x, y) {
      if (is.null(self$dna_template_instance)) {
        stop("you need to provide the points of the ribbons and hydrogen bounds.")
      }

      scaler <- self$desired_height / self$dna_template_instance$height
      ribbons <-
        map(
          self$dna_template_instance$ribbons,
          ~ do_scale_rotate_translate_affineTransfor(.x, scaler, scaler, 0, x, y)
        )
      hydrogenBbonds <-
        map(
          self$dna_template_instance$hydrogenBbonds,
          ~ do_scale_rotate_translate_affineTransfor(.x, scaler, scaler, 0, x, y)
        )


      for (i in 1:self$numOfRepeats) {
        one_step <- self$dna_template_instance$moveStep * scaler
        additional_x <- (i - 1) * one_step


        colors <- self$listOfHydrogenBondColors
        colors <- rep(colors, 16 / length(colors))

        if (is.na(colors[1])) {
          colo_of_board <- 'black'
        }else {
          colo_of_board <- NA
        }

        walk2(
          hydrogenBbonds,
          colors,
          ~ grid.polygon(
            x = .x[1, ] + additional_x,
            y = .x[2, ],
            default.units = 'in',
            gp = gpar(fill = .y , col = colo_of_board)

          )
        )

        walk2(
          ribbons,
          self$listOfRibbonGpars,
          ~ grid.polygon(
            x = .x[1,] + additional_x,
            y = .x[2,],
            default.units = 'in',
            gp = .y
          )
        )
      }


    }

  )
)

#' Quick function to create the DNA drawer instance.
#'
#' @param dnaType index of the DNA drawer type, from index 1
#' @param numOfRepeats the helix repeat, the greater the longer length
#' @param desired_height the height of the helix
#'
#' @return the drawer instance
#' @export
#'
#' @examples
#' create_DNA_drawer()
create_DNA_drawer <-
  function(dnaType = 1,
           numOfRepeats = 3,
           desired_height = 0.4) {
    dna_template_instance <- DNA_templates[[dnaType]]

    drawer <- DNA_DoubleHelixDrawer$new()
    drawer$numOfRepeats <- numOfRepeats
    drawer$desired_height <- desired_height
    drawer$dna_template_instance <- dna_template_instance


    return(drawer)
  }
