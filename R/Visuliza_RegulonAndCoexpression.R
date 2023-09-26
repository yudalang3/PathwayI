#' @export
BioGraphicNode <- R6Class(
  classname = "BioGraphicNode",
  inherit = GraphicNode,
  public = list(
    circle_radius = 50,
    # may be a ellipse/oval
    shape_aspect_ratio = 0,
    draw_me = function(default_unit = 'in',
                       gp_shape = gpar(),
                       gp_text = gpar()) {
      x = self$xAxis_or_radius
      y = self$yAxis_or_angle
      r = self$circle_radius


      # grid.rect(x -r ,y,width = 2*r,height = 15,just = 0, gp = gpar(fill = 'black'),default.units = default_unit)
      # grid.circle(x=x,y=y,default.units = default_unit, r = r, gp = gpar(fill = NA))

      xLeft <- x - r
      xRight <- x + r
      x <- c(xLeft, xLeft, xRight, xRight, xRight, xLeft)
      y <- c(y, y + r, y + r, y, y - r, y - r)
      grid.Bezier(
        x = x ,
        y = y,
        open = F,
        default.units = default_unit,
        gp = gp_shape
      )

      grid.text(
        x = self$xAxis_or_radius,
        y = self$yAxis_or_angle,
        label = self$label,
        default.units = default_unit,
        gp = gp_text
      )
    }
  )

)


tan_to_sin_cos <- function(tan_value) {
  hypotenuse <- sqrt(1 + tan_value * tan_value)

  result <-
    list(sin = tan_value / hypotenuse , cos = 1 / hypotenuse)

  return(result)
}


getBottomXYList_fromBezier <- function(ol, default_unit) {
  x = c(
    ol$x_coeNodes_leftMost,
    ol$x_coeNodes_leftMost ,
    ol$x_coeNodes_rightMost,
    ol$x_coeNodes_rightMost
  )

  y = c(
    ol$y_coeNodes_leftMost,
    ol$y_coeNodes_leftMost - ol$circle_unit_r_value * 2.2 ,
    ol$y_coeNodes_leftMost - ol$circle_unit_r_value * 2.2 ,
    ol$y_coeNodes_rightMost
  )

  x <- rev(x)
  y <- rev(y)
  bGrob <- BezierGrob(
    x = x,
    y = y,
    default.units = default_unit,
    open = T
  )

  pts <- BezierPoints(bGrob)
  return(pts)
}

getBottomXYList_fromMultipleCurves <-
  function(ol,coexpresNodes_belongsTo_targets,
           half_unit_coexpression_node = 0.5,
           default_unit = 'in') {
    x_ploygon <- vector()
    y_ploygon <- vector()

    length_of_co <- length(coexpresNodes_belongsTo_targets)
    xx1 <- map_dbl(rev(coexpresNodes_belongsTo_targets), ~ .x$xAxis_or_radius + half_unit_coexpression_node)
    xx1[1] <- ol$x_coeNodes_rightMost
    xx2 <- map_dbl(rev(coexpresNodes_belongsTo_targets), ~ .x$xAxis_or_radius - half_unit_coexpression_node)
    xx2[length_of_co] <- ol$x_coeNodes_leftMost


    yy1 <- map_dbl(rev(coexpresNodes_belongsTo_targets), ~ .x$yAxis_or_angle - 0.5 * ol$circle_unit_r_value )
    temp_y <- ol$y_coeNodes_rightMost
    yy1[1] <- temp_y
    yy2 <- map_dbl(rev(coexpresNodes_belongsTo_targets), ~ .x$yAxis_or_angle - 0.5 * ol$circle_unit_r_value )
    yy2[length_of_co] <-  temp_y

    curvatures <- rep(-0.5, length_of_co)
    curvatures[1] <- -0.7
    curvatures[length_of_co] <- -0.7

    pwalk(list(xx1,xx2,yy1,yy2,curvatures), function(x1,x2,y1,y2,curvature) {
      a <- curveGrob(
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2,
        default.units = default_unit,
        square = F,
        curvature = curvature,
        ncp = 8
      )

      b <- grobCoords(a, closed = F)

      x_ploygon <<- c(x_ploygon, b[[1]][[1]]$x)
      y_ploygon <<- c(y_ploygon, b[[1]][[1]]$y)

      # grid.segments(
      #   x0 = x$xAxis_or_radius,
      #   y0 = x$yAxis_or_angle,
      #   x1 = x$xAxis_or_radius + 2 * radius_circule_coeNode,
      #   y1 = x$yAxis_or_angle,
      #   default.units = default_unit,
      # )
    })

    # grid.lines(x_ploygon,y_ploygon,default.units = default_unit, gp = gpar(col = 'red') )
    # grid.polygon(x_ploygon,y_ploygon,default.units = default_unit, gp = gpar(fill = 'green') )
    return(list(x = x_ploygon, y =y_ploygon))
  }

#' Outbound painter
#'
#' @param ol : outlinePoints
#' @param default_unit
#'
#' @export
#'
#' @examples
#' outbound_painter()
outbound_painter <- function(ol = list(
  x_TF_top = 2,
  y_TF_top = 2.5,
  x_TF_left = 0.5,
  y_TF_left = 2,
  x_TF_right = 2.5,
  y_TF_right = 2,
  x_coeNodes_leftMost = 0,
  y_coeNodes_leftMost = 0.5,
  x_coeNodes_1_afMost = 0.5,
  y_coeNodes_1_afMost = 0,
  x_coeNodes_last_afMost = 2.5,
  y_coeNodes_last_afMost = 0,
  x_coeNodes_rightMost = 3,
  y_coeNodes_rightMost = 0.5,
  circle_unit_r_value = 0.3
),
coexpresNodes_belongsTo_targets,
default_unit = 'in',
buttomStyleBezier = T,
half_unit_coexpression_node = 0.5) {
  # draw the top line
  x = c(ol$x_TF_left, ol$x_TF_left , ol$x_TF_right, ol$x_TF_right)

  y = c(ol$y_TF_left, ol$y_TF_top  , ol$y_TF_top , ol$y_TF_right)


  bGrob <- BezierGrob(
    x = x,
    y = y,
    default.units = default_unit,
    open = T
  )
  pts <- BezierPoints(bGrob)

  x_ploygon <- pts$x
  y_ploygon <- pts$y

  #draw right
  x = c(ol$x_TF_right,
        ol$x_TF_right ,
        ol$x_coeNodes_rightMost,
        ol$x_coeNodes_rightMost)

  temp <- 0.5 * (ol$y_TF_right + ol$y_coeNodes_rightMost)
  y = c(ol$y_TF_left,
        temp,
        temp,
        ol$y_coeNodes_rightMost)

  bGrob <- BezierGrob(
    x = x,
    y = y,
    default.units = default_unit,
    open = T
  )
  pts <- BezierPoints(bGrob)

  x_ploygon <- c(x_ploygon, pts$x)
  y_ploygon <- c(y_ploygon, pts$y)

  #draw the bottom line
  if (buttomStyleBezier) {
    pts <- getBottomXYList_fromBezier(ol, default_unit)
  } else {

    pts <-
      getBottomXYList_fromMultipleCurves(ol,coexpresNodes_belongsTo_targets,
                                         half_unit_coexpression_node,
                                         default_unit)
  }

  x_ploygon <- c(x_ploygon, pts$x)
  y_ploygon <- c(y_ploygon, pts$y)

  # draw left
  x = c(ol$x_TF_left,
        ol$x_TF_left ,
        ol$x_coeNodes_leftMost,
        ol$x_coeNodes_leftMost)

  temp <- 0.5 * (ol$y_TF_left + ol$y_coeNodes_leftMost)
  y = c(ol$y_TF_left,
        temp,
        temp,
        ol$y_coeNodes_leftMost)

  x <- rev(x)
  y <- rev(y)
  bGrob <- BezierGrob(
    x = x,
    y = y,
    default.units = default_unit,
    open = T
  )

  pts <- BezierPoints(bGrob)

  x_ploygon <- c(x_ploygon, pts$x)
  y_ploygon <- c(y_ploygon, pts$y)

  grid.polygon(
    x = x_ploygon,
    y = y_ploygon,
    default.units = default_unit,
    gp = gpar(
      fill = '#F6D328',
      alpha = 1,
      col = 'black',
      lwd = 1
    )
  )

}


#' Display the TF, its targets and co expressed genes.
#'
#' @param data a list contains three elements, the name is fixed, the values are the char vector.
#' @param buttomStyleBezier whether draw the buttom outbound with the bezier lines.
#'
#' @description
#' The input data list needs three elements, the name is fiexed: TF, targets and coexpression genes.
#'
#' For example, the default value, we could generate the data with the following codes:
#'
#' <pre>
#' data = list(
#'     TF = "TF1",
#'     targets = paste0("g", c(4, 1, 9, 8, 3, 2, 6, 7, 5)),
#'     coexpression = c(paste0("g", 1:7), paste0("k", 1:3))
#' )
#' </pre>
#'
#' @details
#'
#' 1. The string vector corresponding to coexprerssion is the gene that has a co-expression relationship with the TF, which may or may not belong to the target gene of the TF.
#'
#' 2. The string vector given after targets is all the target genes of the TF.
#'
#' 3. Note that in the content of coexpression, the strings that belong to targets are placed in the first part, rather than being shuffled.
#'
#' @export
#'
#' @examples
#' # example1
#' display_TF_targets_coexpress()
#' display_TF_targets_coexpress(buttomStyleBezier = T)
#' # example2
#' set_global_pars('fontsize', 7)
#' data = list(
#'   TF = "TCF/LEF",
#'   targets = c('Axin2','TCF7','ST7','BZRAP1', 'PDK2', 'PIK3C2A', 'CLK1' , 'SERPINB1','SPEN','NUCB2','JMJD6','DGKD','MLLT10'),
#'   coexpression = c('SPEN','NUCB2','JMJD6','DGKD','MLLT10','AXIN2','PIK3C2A','SERPINB1','TP53','CXCL1','INS')
#' )
#' display_TF_targets_coexpress(data = data,buttomStyleBezier = T)
#' # example 3
#' set_global_pars('fontsize', 12)
#' data = list(
#'   TF = "TF1",
#'   targets = paste0("g", 1:10),
#'   coexpression = c(paste0("g", 1:4), paste0("k", 1:4))
#' )
#' display_TF_targets_coexpress(data = data)
display_TF_targets_coexpress <- function(data = list(
  TF = "TF1",
  targets = paste0("g", c(4, 1, 9, 8, 3, 2, 6, 7, 5)),
  coexpression = c(paste0("g", 1:7), paste0("k", 1:3))
), buttomStyleBezier = F) {

  default_unit = 'in'
  last_index_of_coexpresion_belongTo_targes <-
    length(intersect(data$coexpression, data$targets))

  dim_of_device <- par("din")
  width <- dim_of_device[1]
  height <- dim_of_device[2]

  blank_area <-
    calculate_blankArea( width = width, height = height)
  available_width <- width - blank_area$l - blank_area$r
  available_height <- height - blank_area$b - blank_area$t

  # generate nodes for coexpression and targets
  node_TF <- BioGraphicNode$new(1)
  node_TF$label <- data$TF

  createNodeFunc <- function(x) {
    node <- BioGraphicNode$new()
    node$label <- x
    return(node)
  }

  coexpressionNodes <- lapply(X = data$coexpression,
                              FUN = createNodeFunc)
  coexpressionNodes <-
    setNames(coexpressionNodes, nm = data$coexpression)
  targetsNodes <- lapply(X = data$targets,
                         FUN = createNodeFunc)
  targetsNodes <- setNames(targetsNodes, data$targets)


  # start to calculate the locations
  num_coexpression_nodes <- length(data$coexpression)
  num_target_nodes <- length(data$targets)

  yAxis_TF <- 0.9 * height
  yAxis_coexpression_nodes <- 0.5 * height
  yAxis_targets <- 0.1 * height

  # calculate: 0.5 is for radius,
  # 0.9 is the ratio of paint the circle
  half_unit_coexpression_node <-
    available_width / num_coexpression_nodes * 0.5
  radius_circule_coeNode = 0.9 * half_unit_coexpression_node

  half_unit_target_node <- available_width / num_target_nodes * 0.5
  radius_circule_tarNode = 0.9 * half_unit_target_node


  node_TF$xAxis_or_radius <- 0.3 * available_width + blank_area$l
  node_TF$yAxis_or_angle <- yAxis_TF
  node_TF$circle_radius <- 0.07 * available_width


  xs_for_circle <-
    available_width * seq_along(coexpressionNodes) / num_coexpression_nodes - half_unit_coexpression_node + blank_area$l
  pwalk(list(coexpressionNodes, xs_for_circle), function(x, t) {
    x$xAxis_or_radius <- t
    x$yAxis_or_angle <- yAxis_coexpression_nodes
    x$circle_radius <- radius_circule_coeNode
  })

  xs_for_circle <-
    available_width * seq_along(targetsNodes) / num_target_nodes - half_unit_target_node + blank_area$l
  pwalk(list(targetsNodes, xs_for_circle), function(x, t) {
    x$xAxis_or_radius <- t
    x$yAxis_or_angle <- yAxis_targets
    x$circle_radius <- radius_circule_tarNode
  })

  # visulize
  grid.newpage()

  # draw coexpression genes belong to the targets
  coexpresNodes_belongsTo_targets <-
    coexpressionNodes[1:last_index_of_coexpresion_belongTo_targes]
  x_coNodes_belTo_tars <-
    map_dbl(coexpresNodes_belongsTo_targets, function(x) {
      x$xAxis_or_radius
    })
  y_coNodes_belTo_tars <-
    map_dbl(coexpresNodes_belongsTo_targets, function(x) {
      x$yAxis_or_angle
    })
  firstCoexprNode_belTo_tars <- coexpresNodes_belongsTo_targets[[1]]

  lastCoexprNode_belongsTo_targets <-
    coexpresNodes_belongsTo_targets[[last_index_of_coexpresion_belongTo_targes]]


  x_TF_top <- node_TF$xAxis_or_radius


  y_TF_top <- node_TF$yAxis_or_angle + node_TF$circle_radius * 1.4

  x_TF_left <-
    node_TF$xAxis_or_radius - node_TF$circle_radius * 1.2

  y_TF_left <- node_TF$yAxis_or_angle

  x_TF_right <-
    node_TF$xAxis_or_radius + node_TF$circle_radius * 1.2

  y_TF_right <- node_TF$yAxis_or_angle


  x_coeNodes_leftMost <-
    firstCoexprNode_belTo_tars$xAxis_or_radius - half_unit_coexpression_node

  y_coeNodes_leftMost <- firstCoexprNode_belTo_tars$yAxis_or_angle

  x_coeNodes_1_afMost <- firstCoexprNode_belTo_tars$xAxis_or_radius
  y_coeNodes_1_afMost <-
    firstCoexprNode_belTo_tars$yAxis_or_angle - radius_circule_coeNode * 1.4

  x_coeNodes_last_afMost <-
    lastCoexprNode_belongsTo_targets$xAxis_or_radius
  y_coeNodes_last_afMost <-
    lastCoexprNode_belongsTo_targets$yAxis_or_angle - radius_circule_coeNode * 1.4
  if (last_index_of_coexpresion_belongTo_targes == num_coexpression_nodes) {
    x_coeNodes_rightMost <-
      firstCoexprNode_belTo_tars$xAxis_or_radius + half_unit_coexpression_node
  } else {
    x_coeNodes_rightMost <-
      0.5 * (
        lastCoexprNode_belongsTo_targets$xAxis_or_radius + coexpressionNodes[[last_index_of_coexpresion_belongTo_targes + 1]]$xAxis_or_radius
      )
  }

  y_coeNodes_rightMost <-
    lastCoexprNode_belongsTo_targets$yAxis_or_angle

  outlinePoints <- list(
    x_TF_top = x_TF_top,
    y_TF_top = y_TF_top,
    x_TF_left = x_TF_left,
    y_TF_left = y_TF_left,
    x_TF_right = x_TF_right,
    y_TF_right = y_TF_right,
    x_coeNodes_leftMost = x_coeNodes_leftMost,
    y_coeNodes_leftMost = y_coeNodes_leftMost,
    x_coeNodes_1_afMost = x_coeNodes_1_afMost,
    y_coeNodes_1_afMost = y_coeNodes_1_afMost,
    x_coeNodes_last_afMost = x_coeNodes_last_afMost,
    y_coeNodes_last_afMost = y_coeNodes_last_afMost,
    x_coeNodes_rightMost = x_coeNodes_rightMost,
    y_coeNodes_rightMost = y_coeNodes_rightMost,
    circle_unit_r_value = radius_circule_coeNode
  )

  outbound_painter(
    ol = outlinePoints,
    coexpresNodes_belongsTo_targets = coexpresNodes_belongsTo_targets,
    default_unit = default_unit,
    buttomStyleBezier = buttomStyleBezier,
    half_unit_coexpression_node
  )

  # draw the connecting lines
  for (i in 1:last_index_of_coexpresion_belongTo_targes) {
    node <- coexpresNodes_belongsTo_targets[[i]]
    corespondingNode <- targetsNodes[[node$label]]

    draw_bezier_fourPoints(
      xx = c(
        node$xAxis_or_radius + node$circle_radius,
        corespondingNode$xAxis_or_radius + corespondingNode$circle_radius,
        corespondingNode$xAxis_or_radius - corespondingNode$circle_radius,
        node$xAxis_or_radius - node$circle_radius
      ),
      yy = c(
        node$yAxis_or_angle,
        corespondingNode$yAxis_or_angle,
        corespondingNode$yAxis_or_angle,
        node$yAxis_or_angle
      ),
      gp = gpar(fill = "lightblue", alpha = 0.2)
    )
  }


  # draw the coexpression genes
  default_gpar_for_genes <-
    gpar(fill = '#DDDDDD',
         lwd = 1,
         fontsize = global_pars[['fontsize']])
  gpar_for_coexp_butNot_targ <-
    gpar(
      col = '#828282',
      fill = '#DDDDDD',
      lwd = 1,
      fontsize = global_pars[['fontsize']]
    )
  # draw the coexpression genes
  draw_coexpressed_genes <- function(x, index) {
    x_tf <- node_TF$xAxis_or_radius
    y_tf <- node_TF$yAxis_or_angle


    x_curr = x$xAxis_or_radius
    y_curr = x$yAxis_or_angle

    abs_of_xAxis <- abs(x_curr - x_tf)
    circle_enlarged_unit_r <- 1 * radius_circule_coeNode
    if (abs_of_xAxis == 0) {
      x_curr <- x_curr
      y_curr <- y_curr + circle_enlarged_unit_r
    } else {
      tan_theta <- (y_tf - y_curr) / abs_of_xAxis

      ret <- tan_to_sin_cos(tan_theta)
      if (x_curr > x_tf) {
        x_curr <-
          x_curr - circle_enlarged_unit_r * ret$cos
      } else {
        x_curr <-
          x_curr + circle_enlarged_unit_r * ret$cos
      }
      y_curr <-
        y_curr + circle_enlarged_unit_r * ret$sin
    }

    if (index <= last_index_of_coexpresion_belongTo_targes) {
      grid.segments(
        x0 = node_TF$xAxis_or_radius,
        y0 = node_TF$yAxis_or_angle,
        x1 = x_curr,
        y1 = y_curr,
        arrow = arrow(type = "closed", length = unit(0.1, "inches")),
        gp = gpar(
          lty = "solid",
          lwd = 2,
          fill = "black"
        )
        ,
        default.units = default_unit
      )

      x$draw_me(gp_shape = default_gpar_for_genes, gp_text = default_gpar_for_genes)
    } else {
      grid.segments(
        x0 = node_TF$xAxis_or_radius,
        y0 = node_TF$yAxis_or_angle,
        x1 = x_curr,
        y1 = y_curr,
        gp = gpar(
          lty = "longdash",
          lwd = 2,
          col = "gray50"
        )
        ,
        default.units = default_unit
      )

      x$draw_me(gp_shape = gpar_for_coexp_butNot_targ, gp_text = gpar_for_coexp_butNot_targ)
    }

  }
  pwalk(list(coexpressionNodes, 1:length(coexpressionNodes)), draw_coexpressed_genes)

  # TF draw
  node_TF$draw_me(gp_shape = default_gpar_for_genes, gp_text = default_gpar_for_genes)

  # target nodes
  walk(targetsNodes, function(x) {
    x$draw_me(gp_shape = default_gpar_for_genes, gp_text = default_gpar_for_genes)
  })

  # grid.newpage()


}
