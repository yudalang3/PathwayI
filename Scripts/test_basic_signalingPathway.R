

painter <- create_lipidBilayer_drawer(draw_circle_head = T)

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

keyframe1 <- function(){
  grid.newpage()
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
}
keyframe2 <- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

  ligand <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_ligand']]$points,
                          inner_extension_ratio = 0.2)
  ligand$xAxis_or_radius <- x[1]
  ligand$yAxis_or_angle <- y[1] + 1
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
}

keyframe3 <- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

  ligand <-
    create_freeShape_node(xyPoints_shape = basic_bioGraphics_templates[['basic_ligand']]$points,
                          inner_extension_ratio = 0.2)
  ligand$xAxis_or_radius <- x[1]
  ligand$yAxis_or_angle <- y[1] + 0.7
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
}


keyframe4 <- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
}

keyframe5 <- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1] + 0.5
  kinase$yAxis_or_angle <- y[1] - 1.2
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe6<- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1] + 0.3
  kinase$yAxis_or_angle <- y[1] - 0.9
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe7<- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1] + 0.1
  kinase$yAxis_or_angle <- y[1] - 0.6
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe8<- function(){
  grid.newpage()
  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1]
  kinase$yAxis_or_angle <- y[1] - 1.2
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe9<- function(){
  clear_global_bioGraphics_nodes_list()

  set_global_pars('fontsize', 9)
  display_TF_targets_coexpress(buttomStyleBezier = T,
                               paintArea = create_rectangle(1.8, 1, 3, 3))

  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1]
  kinase$yAxis_or_angle <- y[1] - 1.2
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe10<- function(){
  clear_global_bioGraphics_nodes_list()

  set_global_pars('fontsize', 9)
  display_TF_targets_coexpress(buttomStyleBezier = T,
                               paintArea = create_rectangle(1.8, 1, 3, 3))

  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1]
  kinase$yAxis_or_angle <- y[1] - 1.6
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe11<- function(){
  clear_global_bioGraphics_nodes_list()

  set_global_pars('fontsize', 9)
  display_TF_targets_coexpress(buttomStyleBezier = T,
                               paintArea = create_rectangle(1.8, 1, 3, 3))

  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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
  kinase$xAxis_or_radius <- x[1] - 0.2
  kinase$yAxis_or_angle <- y[1] - 1.6
  kinase$circle_radius <- 0.5
  kinase$label <- "kinase"
  kinase$draw_me()
}

keyframe12<- function(){
  clear_global_bioGraphics_nodes_list()

  set_global_pars('fontsize', 9)
  display_TF_targets_coexpress(buttomStyleBezier = T,
                               paintArea = create_rectangle(1.8, 1, 3, 3))

  painter$draw_lipid_along_curve(lineX = x, lineY = y)

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

exps <- paste0('keyframe', 1:12)

animation::saveGIF(expr = {
  walk(exps, function(x) {
    do.call(what = x, args = list())
  })
}, interval = 0.4, ani.dev = 'png', antialias = "cleartype",ani.width = 1150, ani.height = 1400,ani.res = 180)
