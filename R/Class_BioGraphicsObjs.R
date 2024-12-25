#' The BioGraphicNode model drawer
#' @description
#' `BioGraphicNode` is an models for user to draw any arbitrary node
#'
#' It can be organized by list in list structure. Some situations are the most suitable:
#' * The complex Membrane receptor protein.
#' * Some complex structure, like DC complex in WNT signaling pathway.
#'
#' @export
BioGraphicNode <- R6Class(
  classname = "BioGraphicNode",
  inherit = GraphicNode,
  public = list(
    #' @field circle_radius how big the node is.
    circle_radius = 1,
    #' @field rotation_angle_inRadian the rotation angle in radian, default is 0.
    rotation_angle_inRadian = 0,
    #' @field xyPoints_shape the x y points  of the drawing shape, default is a rectangle
    xyPoints_shape = rbind(c(-0.5, 0.5, 0.5, -0.5), c(-0.5, -0.5, 0.5, 0.5)),
    #' @field gpar_shape the gpar for the shape
    gpar_shape = gpar(),
    #' @field gpar_text the gpar for the text
    gpar_text = NULL,
    #' @field inner_extension_ratio the ratio of inner boarder, try this parameter by yourself.
    inner_extension_ratio = 0.15,
    #' @field text_position hjust and vjust for textGrob
    text_position = c(0.5, 0.5),
    #' @field if_draw_label Should draw the label
    if_draw_label = T,
    #' @field uniqueID Different from the label, this is used to distinguish the each node entity, each important node needs to assign the id.
    uniqueID = NA,

    #' initialize the function to get the default parameters.
    #' @description
    #' In order to get the default parameters for the class, we initialize properties here.
    #'
    initialize = function() {
      self$gpar_text = get_global_text_pars()
    },

    #' Draw if this node does not have children.
    #' @param default_unit the unit default is inch.
    draw_if_hasChildren = function(default_unit = 'in') {
      x = self$xAxis_or_radius
      y = self$yAxis_or_angle
      r = self$circle_radius

      ## The 1 means the default free shape is 1 'inch'
      scaler <- r / 1

      rangeList <- map(self$children, function(child) {
        xyPoints <-
          do_scale_rotate_translate_affineTransfor(
            child$xyPoints_shape,
            scaleWidth = scaler,
            scaleHeight = scaler,
            theta = self$rotation_angle_inRadian,
            moveX = x,
            moveY = y
          )
        polyGrob <- grid.polygon(
          x = xyPoints[1,],
          y = xyPoints[2,],
          default.units = default_unit,
          gp = child$gpar_shape
        )

        x_range <- range(xyPoints[1,])
        y_range <- range(xyPoints[2,])
        return(c(x_range, y_range))
      })

      # browser()
      xyPoints <-
        do_scale_rotate_translate_affineTransfor(
          self$xyPoints_shape,
          scaleWidth = scaler,
          scaleHeight = scaler,
          theta = self$rotation_angle_inRadian,
          moveX = x,
          moveY = y
        )
      # polygonGrob
      polyGrob <- polygonGrob(
        x = xyPoints[1,],
        y = xyPoints[2,],
        default.units = default_unit,
        gp = self$gpar_shape
      )
      register_global_bioGraphics_nodes_list(self$label, list(grob = polyGrob, x =
                                                                x, y = y))

      df_range <- do.call(rbind, rangeList)
      xMin <- min(df_range[, 1])
      yMin <- min(df_range[, 3])
      xMax <- max(df_range[, 2])
      yMax <- max(df_range[, 4])

      x <-
        xMin * (1 - self$text_position[1]) + xMax * self$text_position[1]
      y <-
        yMin * (1 - self$text_position[2]) + yMax * self$text_position[2]

      if (self$if_draw_label && !is.na(self$label)) {
        grid.text(
          x = x,
          y = y,
          label = self$label,
          default.units = default_unit,
          gp = self$gpar_text,
          hjust = 1 - self$text_position[1],
          vjust = 1 - self$text_position[2]
        )
      }
    },

    #' Draw if this node does have children.
    #' @param default_unit the unit default is inch.
    draw_if_noChildren = function(default_unit = 'in') {
      x = self$xAxis_or_radius
      y = self$yAxis_or_angle
      r = self$circle_radius

      ## The 1 means the default free shape is 1 'inch'
      scaler <- r / 1

      xyPoints <-
        do_scale_rotate_translate_affineTransfor(
          self$xyPoints_shape,
          scaleWidth = scaler,
          scaleHeight = scaler,
          theta = self$rotation_angle_inRadian,
          moveX = x,
          moveY = y
        )
      polyGrob <- grid.polygon(
        x = xyPoints[1,],
        y = xyPoints[2,],
        default.units = default_unit,
        gp = self$gpar_shape
      )

      x_range <- range(xyPoints[1,])
      y_range <- range(xyPoints[2,])


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

      if (self$if_draw_label && !is.na(self$label)) {
        x <-
          x_range[1] * (1 - self$text_position[1]) + x_range[2] * self$text_position[1]
        y <-
          y_range[1] * (1 - self$text_position[2]) + y_range[2] * self$text_position[2]
        grid.text(
          x = x,
          y = y,
          label = self$label,
          default.units = default_unit,
          gp = self$gpar_text,
          hjust = 1 - self$text_position[1],
          vjust = 1 - self$text_position[2],
          rot = -self$rotation_angle_inRadian / ONE_DEGREE_IN_RADIAN
        )
      }
    },

    #' Draw myself
    #'
    #' @param default_unit the coordinate unit, default is 'in'
    #'
    #' @examples
    #' draw_me()
    draw_me = function(default_unit = 'in') {
      num_of_children <- self$children |> length()
      if (num_of_children == 0) {
        self$draw_if_noChildren(default_unit = default_unit)
      } else {
        self$draw_if_hasChildren(default_unit = default_unit)
      }
    },

    #' The print method Override here
    #' @examples
    #' print()
    print = function() {
      super$print()
      cat('\n')

      num_of_children <- self$children |> length()
      if (num_of_children == 0) {
        cat("The number dim of the shape is: " , dim(self$xyPoints_shape) , '\n')
      }else{
        cat("The number of children are ", num_of_children, " ")
      }
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
      radius = 0.5
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
        radius = 0.5
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
#' @details
#' Note: this is the scaler of width height, also the final obtained height and width
#' Because the template's width and height both are the 1.
#'
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

    xy <-
      do_scale_affine(xy, scaleWidth = scaleWidth, scaleHeight = scaleHeight)
    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }

#' Create the round rectangular bio graphics node drawer.
#'
#' @param width the width with inchs
#' @param height the height with inchs
#' @param r_ratio the ratio of the round corner
#' @param inner_extension_ratio try this argument yourself
#'
#' @details
#' Why the parameter does not have the scale*prefix ? Because it is produced not through
#' a template.
#'
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_round_rectangular_node()
create_round_rectangular_node <-
  function(width = 1,
           height = 1,
           r_ratio = 0.2,
           inner_extension_ratio = 0) {
    grob <-
      roundrectGrob(
        x = 0,
        y = 0,
        width = width,
        height = height,
        default.units = 'in',
        r = unit(r_ratio, 'snpc')
      )

    xy <- extract_xyCoordinate_points(grob)
    a <- BioGraphicNode$new()
    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }

#' Create the free shape bio graphics node drawer.
#'
#' @param xyPoints_shape the 2xn matrix, where first line is x, next line is y
#' @param inner_extension_ratio try this argument yourself
#' @param scaleWidth the scaler of the width
#' @param scaleHeight the scaler of the height
#'
#' @details
#' Note: this is the scaler of width and height, not the final obtained height and width
#'
#' @return the instance
#' @export
#'
#' @examples
#' create_freeShape_node(xyPoints_shape = rbind(c(1,2,3), c(2,3,4)))
create_freeShape_node <-
  function(inner_extension_ratio = 0,
           xyPoints_shape,
           scaleWidth = 1,
           scaleHeight = 1) {
    a <- BioGraphicNode$new()

    xy <-
      do_scale_affine(xyPoints_shape, scaleWidth = scaleWidth, scaleHeight = scaleHeight)
    a$xyPoints_shape <- xy
    a$inner_extension_ratio <- inner_extension_ratio
    return(a)
  }

#' Create the object with the self contained simple bio graphics node.
#' This node is only has one shape x y coordinates.
#'
#'@description
#' use `basic_bioGraphics_templates |> names()` to see the supported name.
#'
#' @param name the name of the supported free shape.
#' @param expectWidth the user expected width, recommendation is in 'inch', default 1
#' @param expectHeight the user expected height recommendation is in 'inch'. set it as `NA` for scaling by one para. default is NA.
#'
#' @details
#' Note: this is NOT the scaler of width height, it is the final obtained height and width
#'
#' If you want to scale according to one length, set the `expectHeight` as NA, default is NA.
#'
#' @return A BioGraphicNode instance.
#' @export
#'
#' @examples
#' create_selfContained_simple_bioGraphicsNode('LRP_1')
create_selfContained_simple_bioGraphicsNode <-
  function(name,
           expectWidth = 1,
           expectHeight = NA_real_) {

    if (is.null(basic_bioGraphics_templates[[name]])) {
      stop(name , " is not exist in this package. Plase see the documentation.")
    }
    templ <- basic_bioGraphics_templates[[name]]
    tempHeight <- templ$height
    tempWidth <- templ$width

    points <- templ$points
    scaleWidth <- expectWidth / tempWidth
    if (is.na(expectHeight)) {
      scaleHeight <- scaleWidth
      expectHeight <- scaleHeight * tempHeight
    }else {
      scaleHeight <- expectHeight / tempHeight
    }

    if (is.list(points)) {
      children <- lapply(points, function(x) {
        xy <-
          do_scale_affine(x, scaleWidth = scaleWidth, scaleHeight = scaleHeight)
        create_freeShape_node(xyPoints_shape = xy)
      })

      parent <-
        create_rectangular_node(scaleWidth = expectWidth, scaleHeight = expectHeight)
      for (node in children) {
        parent$addChild(node)
      }
      ret <- parent
    } else {
      xy <-
        do_scale_affine(points, scaleWidth = scaleWidth, scaleHeight = scaleHeight)
      ret <- create_freeShape_node(xyPoints_shape = xy)
    }

    return(ret)
  }
