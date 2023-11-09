#' Draw a sector.
#' @description
#' What? you do not know the sector? Try to google it.
#' In order not to get the odd results. The function set the default.units = "in".
#'
#' @param border_colo border color
#' @param xCenter the xCenter coordinate point
#' @param yCenter the yCenter coordinate point
#' @param startDegree start degree
#' @param endDegree end degree
#' @param circle_out_radius outter circle radius
#' @param circle_inner_radius inner circle radius
#' @param fill_colo filled color
#' @param default_unit coordinate unit
#' @param ...
#'
#' @return the polygon grob
#' @export
#'
#' @examples
#' draw_sector() # give result with default values
draw_sector <-
  function(xCenter = 0.5,
           yCenter = 0.5,
           startDegree = 0,
           endDegree = 30,
           circle_out_radius = 1,
           circle_inner_radius = 1.1,
           fill_colo = '#00FFB0',
           border_colo = NA,
           default_unit = 'in',
           ...) {


    num_of_points_arc_out <- 20
    num_of_points_arc_inner <- 20
    equal_spersed_degrees <-
      seq(from = startDegree,
          to = endDegree,
          length.out = num_of_points_arc_out)
    coor <-
      polar2cartesianCoor(
        radius = circle_out_radius,
        angle = equal_spersed_degrees,
        xCenter = xCenter,
        yCenter = yCenter
      )

    xx_out <- coor[1, ]
    yy_out <- coor[2, ]


    equal_spersed_degrees <-
      rev(seq(
        from = startDegree,
        to = endDegree,
        length.out = num_of_points_arc_inner
      ))
    coor <-
      polar2cartesianCoor(
        radius = circle_inner_radius,
        angle = equal_spersed_degrees,
        xCenter = xCenter,
        yCenter = yCenter
      )
    xx_inner <- coor[1, ]
    yy_inner <- coor[2, ]


    glob <-
      grid.polygon(
        x = c(xx_out, xx_inner),
        y = c(yy_out, yy_inner),
        gp = gpar(fill = fill_colo, col = border_colo),
        default.units = default_unit
      )

    invisible(glob)
  }


#一个单位圆分成500等分
one_break_length <- 2 * pi / 800

#' Draw the arc of a circle.
#'
#' @description
#' The mechanism is that you calculate the (x,y) location of the each points.
#' And than draw it.
#'
#' @param startDegree the degree where to start
#' @param endDegree the degree where to end
#' @param radius the radius of the circle
#' @param xCenter the xCenter coordinate point
#' @param yCenter the yCenter coordinate point
#' @param default_unit coordinate unit
#'
#' @export
#'
#' @examples
#' draw_arc(startDegree = 0, endDegree = 68,radius = 1, xCenter = 2,yCenter = 2)
draw_arc <- function(startDegree,
                     endDegree,
                     radius,
                     xCenter = 0.5,
                     yCenter = 0.5,
                     default_unit = 'in') {
  angle <- abs(startDegree - endDegree)
  radian <- angle * ONE_DEGREE_IN_RADIAN

  arc_length <- radian * radius


  num_of_segments = arc_length / one_break_length

  num_of_segments = floor(num_of_segments)
  num_of_segments = ifelse(num_of_segments < 2, 2, num_of_segments)

  intervals <-
    seq.int(from = startDegree,
            to = endDegree,
            length.out = num_of_segments)

  coor <-
    polar2cartesianCoor(radius = radius, angle = intervals, xCenter, yCenter)

  grid.lines(x = coor[1, ],
             y = coor[2, ],
             default.units = default_unit)
}


#' Draw_bezier from two points.
#'
#' @param x0 x0
#' @param y0 y0
#' @param x1 x1
#' @param y1 y1
#' @param default_unit coordinate unit
#' @param ... transfer to the grob function
#' @param vertical: otherwise horizontal
#'
#' @export
#'
#' @examples
#' draw_bezier_twoPoints()
draw_bezier_twoPoints <-
  function(x0,
           y0,
           x1,
           y1,
           default_unit = 'in',
           vertical = T,
           ...) {
    if (vertical) {
      x = c(x0, x0 , x1, x1)
      temp <- 0.5 * (y0 + y1)
      y = c(y0, temp, temp, y1)
    } else {
      temp <- 0.5 * (x0 + x1)
      x = c(x0, temp, temp, x1)
      y = c(y0, y0, y1, y1)
    }

    bGrob <- BezierGrob(
      x = x,
      y = y,
      default.units = default_unit,
      open = T,
      ...
    )
    grid.draw(bGrob)
  }

#'
#' Draw the bezier curve for the four points.
#'
#' @description
#'
#' The location of four points are:
#' <pre>
#' 4    1
#'   3    2
#'</pre>
#'
#' @param x: x axis are: (1,2,3,4)
#' @param y: the order same as y
#' @param default_unit: the coordinate unit
#' @param ...
#'
#' @export
#'
#' @examples
#' draw_bezier_fourPoints()
draw_bezier_fourPoints <-
  function(xx = c(0.2, 0.5, 0.3, 0) + 0.2,
           yy = c(1, 0, 0, 1) + 0.2,
           default_unit = 'in',
           ...) {
    y_interm <- 0.5 * (yy[1] + yy[2])
    ##right
    x = rep(xx[1:2], each = 2)
    y = c(yy[1], y_interm, y_interm, yy[2])
    bGrob <- BezierGrob(
      x = x,
      y = y,
      default.units = default_unit,
      open = T,
      ...
    )
    pts <- BezierPoints(bGrob)
    x_ploygon <- pts$x
    y_ploygon <- pts$y

    ## bottom
    x_ploygon <- c(x_ploygon, xx[3])
    y_ploygon <- c(y_ploygon, yy[3])

    ## left
    x = rep(xx[3:4], each = 2)
    y = c(yy[3], y_interm, y_interm, yy[4])
    bGrob <- BezierGrob(
      x = x,
      y = y,
      default.units = default_unit,
      open = T,
      ...
    )
    pts <- BezierPoints(bGrob)
    x_ploygon <- c(x_ploygon, pts$x)
    y_ploygon <- c(y_ploygon, pts$y)

    grid.polygon(x = x_ploygon,
                 y = y_ploygon,
                 default.units = default_unit,
                 ...)

    # grid.circle(xx,yy, r = unit(2,"mm") ,default.units = default_unit)
    # grid.circle(xx[3],yy[3], r = unit(3,"mm") ,default.units = default_unit)
    ## top is not needed.
  }

#' Draw the 2xn matrix in points
#'
#' @param default.units default is in
#' @param ... the 2xn matrix list
#'
#' @return null
#' @export
#'
#' @examples
#' draw_2xn_matrix_points(rbind(1:5,rep(3,5)))
draw_2xn_matrix_points <- function(... , default.units = 'in') {
  all_objs <- list(...)
  walk(all_objs, function(x) {
    if(dim(x)[1] > 2){
      message("Your input points data, may need to transpose. The nrow is greater than 2.")
    }
    grid.lines(x[1,], x[2,], default.units = default.units)
  })
}


#' draw_lineArrow_accordingTo_grobAndPoints
#'
#' @param node1 grob 1
#' @param node2 grob 2
#' @param x1 x1
#' @param y1 y1
#' @param x2 x2
#' @param y2 y2
#' @param arrow the arrow instance
#' @param ... arguments pass to the grid.lines() function
#'
#' @export
#' @importFrom gridGeometry trimGrob
#'
#' @examples
#' node1 <- get_global_bioGraphics_nodes_list()[['ligand']]
#' node2 <- get_global_bioGraphics_nodes_list()[['receptor']]
#' ligand$xAxis_or_radius, receptor$xAxis_or_radius
#' ligand$yAxis_or_angle, receptor$yAxis_or_angle
#' draw_lineArrow_accordingTo_grobAndPoints(node1,node2,x1,y1,x2,y2)
draw_lineArrow_accordingTo_grobAndPoints <-
  function(node1, node2, x1, y1, x2, y2, arrow = NULL, ...) {
    line <- linesGrob(x = c(x1, x2),
                      y = c(y1, y2),
                      default.units = 'in')


    p <- polyclipGrob(line,
                      gList(node1, node2),
                      op = "minus",
                      gp = gpar(lwd = 2))

    p <- trimGrob(p, from = 0.1, to = 0.9)
    # grid.newpage()
    # grid.draw(node1)
    # grid.draw(node2)
    # grid.draw(line)
    #
    # grid.draw(p)
    temp <- grobCoords(p, closed = F)
    a <- temp[[1]][[1]]
    if (is.null(arrow)) {
      arrow <- arrow()
    }
    grid.lines(a$x, a$y, arrow = arrow, default.units = 'in', ...)

  }
