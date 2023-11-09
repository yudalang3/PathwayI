# Edge_attachedWithArrowDrawer ---------------------------------------------------

#' The edge drawer, it can draw edge with or without the arrow.
#' @description
#' In most cases, the edge needs to be draw-ed and with an arrow.
#' Now I model this process by create the class here.
#'
#' @export
Edge_attachedWithArrowDrawer <- R6Class(
  "Edge_attachedWithArrowDrawer",
  public = list(
    #' @field gpar_shape the gpar for the shape
    gpar_shape = gpar(fill = 'black', lwd = 2),
    #' @field gpar_text the gpar for the text
    gpar_text = NULL,
    #' @field arrow_instance the arrow instance.
    arrow_instance = NULL,
    #' @field text_position hjust and vjust for textGrob
    text_position = c(0.5, 0.5),
    #' @field xyPoints store the xy points for drawing
    xyPoints = list(),
    #' @field label the label of the edge
    label = NULL,
    #' @field curvature control the angle of the lines, -1 and 1 are recommended.
    curvature = 1,

    #' initialize the function to get the default parameters.
    #' @description
    #' In order to initialize the default class properties.
    #'
    initialize = function() {
      self$gpar_text = get_global_text_pars()
    },

    #' Draw the arrow according to the input points.
    #'
    #' @param x1 x of point1
    #' @param y1 y of point1
    #' @param x2 x of point2
    #' @param y2 y of point2
    #'
    #' @description
    #' Input the point of the location and it will draw the arrow.
    #'
    draw_me = function(x1, y1, x2, y2) {
      if (x1 == x2 || y1 == y2) {
        self$draw_grill_edge(x1, y1, x2, y2)
      } else {
        self$draw_rectangular_edge(x1, y1, x2, y2)
      }

      if (is.null(self$label)) {
        return(invisible())
      }

      xMin <- min(x1, x2)
      yMin <- min(y1, y2)
      xMax <- max(x1, x2)
      yMax <- max(y1, y2)
      x <-
        xMin * (1 - self$text_position[1]) + xMax * self$text_position[1]
      y <-
        yMin * (1 - self$text_position[2]) + yMax * self$text_position[2]
      grid.text(
        x = x,
        y = y,
        label = self$label,
        default.units = 'in',
        gp = self$gpar_text,
        hjust = 1 - self$text_position[1],
        vjust = 1 - self$text_position[2]
      )

      invisible()
    },

    #' Draw the edge either horizontal or vertical.
    #' @param x1 x of point1
    #' @param y1 y of point1
    #' @param x2 x of point2
    #' @param y2 y of point2
    draw_grill_edge = function(x1, y1, x2, y2) {
      grid.segments(
        x1,
        y1,
        x2,
        y2,
        default.units = 'in',
        arrow = self$arrow_instance,
        gp = self$gpar_shape,
      )
    },
    #' Draw the edge with diagonal line manner.
    #' @param x1 x of point1
    #' @param y1 y of point1
    #' @param x2 x of point2
    #' @param y2 y of point2
    draw_rectangular_edge = function(x1, y1, x2, y2) {
      grid.curve(
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2,
        default.units = 'in',
        arrow = self$arrow_instance,
        gp = self$gpar_shape,
        curvature = self$curvature
      )
    }

  )
)

#' Quick function to create the Edge drawer instance.
#'
#' @return the drawer instance
#' @export
#'
#' @examples
#' create_dege_drawer()
create_dege_drawer <-
  function(arrowLength = 0.1,
           curvature = 1) {
    drawer <- Edge_attachedWithArrowDrawer$new()

    if (arrowLength > 0) {
      drawer$arrow_instance <- arrow(
        length = unit(arrowLength, "inches"),
        ends = "last",
        type = "closed",
      )
    }
    drawer$curvature = curvature

    return(drawer)
  }
