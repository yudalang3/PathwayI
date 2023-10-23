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

  painter$desired_hight <- 0.2

  r <- 3
  df_round <-
    produce_model_coordinate_points(
      from = 0,
      to = 365,
      by = 2,
      radius = r - painter$desired_hight * 1.2
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

show_surface_models <- function(){

  painter <- create_lipidBilayer_drawer(draw_circle_head = T)
  painter$draw_only_upLayer <- T
  painter$desired_hight <- 0.4

  grid.newpage()

  for (i in seq.int(from = 3, to = 2, by = -0.06)) {
    x <- seq.int(from = i - 1, to = i + 3, by = 0.2)
    y <- rep(i,length(x))
    painter$draw_lipid_along_curve(lineX = x, lineY = y)
  }

  ligand <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_ligand']]$points,
                          inner_extension_ratio = 0.2)
  ligand$xAxis_or_radius <- x[1]
  ligand$yAxis_or_angle <- y[1] + 0.5
  ligand$circle_radius <- 1
  ligand$label <- "ligand"
  ligand$draw_me()

  receptor <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_receptor']]$points,
                          inner_extension_ratio = 0.15)
  receptor$xAxis_or_radius <- x[1]
  receptor$yAxis_or_angle <- y[1]
  receptor$circle_radius <- 1
  receptor$gpar_shape <- gpar(fill = 'white')
  receptor$label <- "receptor"
  receptor$draw_me()


  kinase <-
    create_oval_node(scaleHeight = 0.6, inner_extension_ratio = 0.15)
  kinase$xAxis_or_radius <- x[1] - 0.3
  kinase$yAxis_or_angle <- y[1] - 1.75
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}
