
#' Generate the grid for assistant.
#'
#' Most of the time, we need to use the coordinates to locate the complex.
#'
#' @param interval the gap, in inch unit
#'
#' @export
#'
#' @examples
#' show_grid_forAssistance(0.5)
show_grid_forAssistance <- function(interval = 1) {
  deviceDim <- par('din')
  width_int <- ceiling(deviceDim[1])
  hight_int <- ceiling(deviceDim[2])

  w_seq <- seq.int(from = 0, to = width_int, by = interval)
  h_seq <- seq.int(from = 0, to = hight_int, by = interval)

  lineGpar <- gpar(col = 'grey', lty = 'dashed')
  walk(w_seq, function(x) {
    grid.segments(
      x0 = x,
      y0 = 0,
      x1 = x,
      y1 = hight_int,
      default.units = 'in',
      gp = lineGpar
    )
  })

  grid.text(label = w_seq, x = w_seq , y = 0.06, default.units = 'in', gp = lineGpar,vjust = 0, hjust = 0)

  walk(h_seq, function(y) {
    grid.segments(
      x0 = 0,
      y0 = y,
      x1 = width_int,
      y1 = y,
      default.units = 'in',
      gp = lineGpar
    )
  })

  grid.text(label = h_seq[-1], x = 0 , y = h_seq[-1] , default.units = 'in', gp = lineGpar,vjust = 0, hjust = 0)
}

#' Display the supported phospholipid models.
#'
#' @export
#'
#' @examples
#' show_phospholipid_models()
show_phospholipid_models <- function() {
  # w_and_h <- par('din')
  grid.newpage()
  for (i in 1:6) {
    painter <- create_lipidBilayer_drawer(lipidModelIndex = i)
    scaler <- 1 / 100
    painter$draw_one_unit_lipid(
      scaleWidth = scaler,
      scaleHeight = scaler,
      theta = 0,
      moveX = 0.5 * i,
      moveY = 2
    )

    grid.text(label = i , x = 0.5 *i , y = 1, default.units = 'in')
  }
}


#' A snapshot of the bioGraphics nodes.
#'
#' @description
#' Also, we support the free shape nodel.
#'
#' @examples
#' show_bioGraphicNodes_models()
show_bioGraphicNodes_models <- function() {

  radius <- 0.5

  grid.newpage()
  a <- BioGraphicNode$new();
  a$xAxis_or_radius <- 1;
  a$yAxis_or_angle <- 2;
  a$circle_radius <- radius;
  a$label <- "most sinplest one"
  a$draw_me()

  a <- create_round_node();
  a$xAxis_or_radius <- 2.5;
  a$yAxis_or_angle <- 2;
  a$circle_radius <- radius;
  a$label <- "ellipse one"
  a$draw_me()

  a <- create_oval_node(0.6,1);
  a$xAxis_or_radius <- 3.9;
  a$yAxis_or_angle <- 2;
  a$circle_radius <- radius;
  a$label <- "ellipse one"
  a$gpar_shape <- gpar(fill = 'orange')
  a$draw_me()
}


#' Display a basic signalling pathway model.
#'
#' @export
#'
#' @examples
#' show_pathway_models()
show_pathway_models <- function(){
  clear_global_bioGraphics_nodes_list()

  grid.newpage()
  set_global_pars('fontsize', 9)
  display_TF_targets_coexpress(buttomStyleBezier = T,
                               paintArea = create_rectangle(1.8, 1, 3, 3))

  painter <- create_lipidBilayer_drawer()
  painter$draw_circle_head <- T
  painter$current_lipid_model <- lipid_models[[1]]

  painter$desired_height <- 0.2

  r <- 3
  df_round <-
    produce_model_coordinate_points(
      from = 0,
      to = 365,
      by = 2,
      radius = r - painter$desired_height * 1.2
    )
  x <- df_round[1, ] + r + 0.2
  y <- df_round[2, ] + r + 0.2
  painter$draw_lipid_along_curve(lineX = x, lineY = y)


  receptor <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_receptor']]$points,
                          inner_extension_ratio = 0.15)
  receptor$xAxis_or_radius <- x[1]
  receptor$yAxis_or_angle <- y[1]
  receptor$circle_radius <- 1
  receptor$gpar_shape <- gpar(fill = 'white')
  receptor$label <- "receptor"
  receptor$draw_me()

  ligand <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_ligand']]$points,
                          inner_extension_ratio = 0.2)
  ligand$xAxis_or_radius <- x[1]
  ligand$yAxis_or_angle <- y[1] + 1
  ligand$circle_radius <- 1
  ligand$label <- "ligand"
  ligand$draw_me()

  kinase <-
    create_oval_node(scaleHeight = 0.6, inner_extension_ratio = 0.15)
  kinase$xAxis_or_radius <- x[1]
  kinase$yAxis_or_angle <- y[1] - 1.2
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()


  arrow <- arrow(length = unit(0.1, 'in'))
  node1 <- get_global_bioGraphics_nodes_list()[['ligand']]$grob
  node2 <- get_global_bioGraphics_nodes_list()[['receptor']]$grob
  x1 <- ligand$xAxis_or_radius;x2 <-  receptor$xAxis_or_radius
  y1 <- ligand$yAxis_or_angle;y2 <-  receptor$yAxis_or_angle
  draw_lineArrow_accordingTo_grobAndPoints(node1,node2,x1,y1,x2,y2,arrow = arrow)


  node1 <- get_global_bioGraphics_nodes_list()[['receptor']]$grob
  node2 <- get_global_bioGraphics_nodes_list()[['kinase']]$grob
  x1 <- receptor$xAxis_or_radius;x2 <-  kinase$xAxis_or_radius
  y1 <- receptor$yAxis_or_angle;y2 <-  kinase$yAxis_or_angle
  draw_lineArrow_accordingTo_grobAndPoints(node1,node2,x1,y1,x2,y2,arrow = arrow)

  node1 <- get_global_bioGraphics_nodes_list()[['kinase']]
  node2 <- get_global_bioGraphics_nodes_list()[['TF1']]
  node1_grob <- node1$grob
  node2_grob <- node2$grob
  x1 <- kinase$xAxis_or_radius;x2 <-  node2$x
  y1 <- kinase$yAxis_or_angle;y2 <-  node2$y
  draw_lineArrow_accordingTo_grobAndPoints(node1_grob,node2_grob,x1,y1,x2,y2,arrow = arrow)

}

#' Display a basic surface model.
#'
#' @param painter the lipid bilayer drawer
#' @param yHigh the high y location
#' @param yLow the low y location
#' @param xStart the start location of x
#' @param xEnd the end location of y
#'
#' @export
#'
#' @examples
#' show_surface_models()
show_surface_models <- function(painter = NULL, yHigh = 3.6, yLow = 3, xStart = 1,xEnd = 4){

  if (is.null(painter)) {
    painter <- create_lipidBilayer_drawer(draw_circle_head = T)
  }

  yStartLocations <- seq.int(from = yHigh, to = yLow, by = -0.05)
  xPaintingValue <- seq.int(from = xStart, to = xEnd, by = 0.12)

  grid.newpage()

  last_index <- length(yStartLocations)

  iwalk(yStartLocations, function(yStartLocation, i) {
    num_of_phospholipid <- length(xPaintingValue)
    yy <- rep(yStartLocation, num_of_phospholipid)

    xPaintingValue <- xPaintingValue + i * 0.1

    if (i == last_index) {
      painter$draw_lipid_along_curve(lineX = xPaintingValue, lineY = yy)
    } else {
      ret <-
        painter$calculate_lipid_along_curve(lineX = xPaintingValue[1:num_of_phospholipid], lineY = yy[1:num_of_phospholipid])

      scaleHeight <- ret$scaleHeight
      thetaList <- ret$theta_list
      scaleWidth_1 <- ret$scaleWidth_1
      scaleWidth_2 <- ret$scaleWidth_2
      df_layer1 <- ret$df_layer1
      df_layer2 <- ret$df_layer2
      half_distance_all_1 <- ret$half_distance_all_1
      half_distance_all_2 <- ret$half_distance_all_2

      num_of_phospholipid_toDraw <- 2:ncol(df_layer1)
      grid.circle(
        x = df_layer1[1, num_of_phospholipid_toDraw],
        y = df_layer1[2, num_of_phospholipid_toDraw],
        r = half_distance_all_1,
        default.units = 'in',
        gp = painter$lipid_head_par
      )

      painter$draw_one_unit_lipid(
        scaleWidth = scaleWidth_1[1],
        scaleHeight = scaleHeight,
        theta = thetaList[1],
        moveX = df_layer1[1, 1],
        moveY = df_layer1[2, 1],
        r = half_distance_all_1[1]
      )

      theta2 <- thetaList[1] + degree_of_180_inRadian

      painter$draw_one_unit_lipid(
        scaleWidth = scaleWidth_2[1],
        scaleHeight = scaleHeight,
        theta = theta2,
        moveX = df_layer2[1, 1],
        moveY = df_layer2[2, 1],
        r = half_distance_all_1[2]
      )
    }

  })


}

#' Display the round rectangle bi-layer phospholipid model
#'
#' @export
#'
#' @examples
#' show_roundRect_biLayer()
show_roundRect_biLayer <- function(){
  painter <- create_lipidBilayer_drawer(draw_circle_head = F)
  painter$lipid_head_par <- gpar(fill = 'lightblue', col = 'blue4')
  painter$draw_only_upLayer <- F

  coords <- create_round_rectangle_xyCoords(x = 1, y = 1, w = 5, h = 4, r = 1, space = 0.1,angle_for_roundCorner = 6)

  grid.newpage()

  painter$draw_lipid_along_curve(lineX = coords[1, ],  lineY = coords[2, ],closedPolygon = T)
}

