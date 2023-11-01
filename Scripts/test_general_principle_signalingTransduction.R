# pdf(file = 'picture.pdf',family = 'GB1')



painter <- create_lipidBilayer_drawer(draw_circle_head = T)
painter$lipid_head_par <- gpar(fill = 'lightblue', col = 'blue4')

yStartLocations <- seq.int(from = 3.6, to = 3, by = -0.05)
xPaintingValue <- seq.int(from = 0.5, to = 5, by = 0.12)

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

  lipid_along_line <<- create_point(x = xPaintingValue, y = yy)
})

receptor1 <-
  create_oval_node(scaleHeight = 0.6, inner_extension_ratio = 0.15)
receptor1$xAxis_or_radius <- lipid_along_line@x[6]
receptor1$yAxis_or_angle <- lipid_along_line@y[6]
receptor1$circle_radius <- 0.5
receptor1$gpar_shape <- gpar(fill = 'white')
receptor1$label <- "receptor"
receptor1$text_position <- c(0.5,0)
receptor1$draw_me()


receptor2 <-
  create_selfContained_simple_bioGraphicsNode(name = 'bCatenin')
receptor2$xAxis_or_radius <- lipid_along_line@x[15]
receptor2$yAxis_or_angle <- lipid_along_line@y[15]
receptor2$circle_radius <- 1
receptor2$gpar_shape <- gpar(fill = 'white')
receptor2$label <- "b-Catenin"
receptor2$text_position <- c(0.5,0)
receptor2$draw_me()


receptor3 <-
  create_selfContained_simple_bioGraphicsNode(name = 'LRP_2')
receptor3$xAxis_or_radius <- lipid_along_line@x[22]
receptor3$yAxis_or_angle <- lipid_along_line@y[22]
receptor3$circle_radius <- 0.15
receptor3$gpar_shape <- gpar(fill = 'white')
receptor3$label <- "LRP"
receptor3$text_position <- c(0.5,0)
receptor3$draw_me()


receptor4 <-
  create_selfContained_simple_bioGraphicsNode(name = 'FZD_1')
receptor4$xAxis_or_radius <- lipid_along_line@x[30]
receptor4$yAxis_or_angle <- lipid_along_line@y[30]
receptor4$circle_radius <- 1
receptor4$gpar_shape <- gpar(fill = 'white')
receptor4$label <- "FZD"
receptor4$text_position <- c(0.5,0)
receptor4$draw_me()


# dev.off()
