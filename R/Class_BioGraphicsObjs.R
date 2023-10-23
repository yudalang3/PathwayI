#' @export
BioGraphicNode <- R6Class(
  classname = "BioGraphicNode",
  inherit = GraphicNode,
  public = list(
    circle_radius = 50,

    xyPoints_shape = rbind(c(-0.5, 0.5, 0.5, -0.5), c(-0.5, -0.5, 0.5, 0.5)),
    gpar_shape = gpar(),
    gpar_text = gpar(),
    inner_extension_ratio = 0.15,

    draw_me = function(default_unit = 'in') {
      x = self$xAxis_or_radius
      y = self$yAxis_or_angle
      r = self$circle_radius

      scaler <- r / 1

      xyPoints <-
        do_scale_rotate_translate_affineTransfor(
          self$xyPoints_shape,
          scaleWidth = scaler,
          scaleHeight = scaler,
          theta = 0,
          moveX = x,
          moveY = y
        )
      polyGrob <- grid.polygon(
        x = xyPoints[1,],
        y = xyPoints[2,],
        default.units = default_unit,
        gp = self$gpar_shape
      )

      register_global_bioGraphics_nodes_list(self$label, list(grob = polyGrob, x =
                                                                x, y = y))

      if (self$inner_extension_ratio > 0) {
        scale_factor <- scaler * (1 - self$inner_extension_ratio)
        xyPoints <-
          do_scale_rotate_translate_affineTransfor(
            self$xyPoints_shape,
            scaleWidth = scale_factor,
            scaleHeight = scale_factor,
            theta = 0,
            moveX = x,
            moveY = y
          )
        grid.polygon(x = xyPoints[1,],
                     y = xyPoints[2,],
                     default.units = default_unit)
      }
      grid.text(
        x = x,
        y = y,
        label = self$label,
        default.units = default_unit,
        gp = self$gpar_text
      )
    }

  )

)


#' Create the round bio graphics node drawer.
#'
#' @param inner_extension_ratio try this argument yourself
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_round_node()
create_round_node <- function(inner_extension_ratio = 0) {
  a <- BioGraphicNode$new()

  a$xyPoints_shape <-
    produce_model_coordinate_points(
      from = 0,
      by = 2,
      to = 360,
      radius = 1
    )

  a$inner_extension_ratio <- inner_extension_ratio
  return(a)
}

#' Create the oval bio graphics node drawer.
#'
#' @param inner_extension_ratio try this argument yourself
#' @param scaleWidth scaler for width
#' @param scaleHeight scaler for height
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_oval_node()
create_oval_node <-
  function(scaleWidth = 1,
           scaleHeight = 1,
           inner_extension_ratio = 0) {
    a <- BioGraphicNode$new()


    xy <-
      produce_model_coordinate_points(
        from = 0,
        by = 2,
        to = 360,
        radius = 1
      )
    ## Users can adjust the height ratio
    xy <-
      do_scale_affine(xy, scaleWidth = scaleWidth, scaleHeight = scaleHeight)

    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }

#' Create the rectangular bio graphics node drawer.
#'
#' @param inner_extension_ratio try this argument yourself
#' @param scaleWidth scaler for width
#' @param scaleHeight scaler for height
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_rectangular_node()
create_rectangular_node <-
  function(scaleWidth = 1,
           scaleHeight = 1,
           inner_extension_ratio = 0) {
    a <- BioGraphicNode$new()

    xy <- a$xyPoints_shape
    ## Users can adjust the height ratio
    xy <-
      do_scale_affine(xy, scaleWidth = scaleWidth, scaleHeight = scaleHeight)
    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }

#' Create the free shape bio graphics node drawer.
#'
#' @param xyPoints_shape the 2xn matrix, where first line is x, next line is y
#' @param inner_extension_ratio try this argument yourself
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_freeShape_node(xyPoints_shape = rbind(c(1,2,3), c(2,3,4)))
create_freeShape_node <-
  function(inner_extension_ratio = 0, xyPoints_shape) {
    a <- BioGraphicNode$new()

    xy <- xyPoints_shape
    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }
