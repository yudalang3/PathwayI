# to learn the color mapper, this is also a very very basic fundemental functions.
# When we use the color mapper, it will be needed to mapping the color

# Rectangular heatmap -----------------------------------------------------
ret <- process_createNodes(2,numOfChildren = 3)


set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.4, t = 0.1, b = 0.1))

rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(ret)
rectLayoutDesigner$plotTree(ret)

displayTheTree(ret$rootNode)


data <- matrix(rnorm(9 * 3), ncol = 3)
rownames(data) <- as.character(1:9)
mapper <- continuousMapper(colors = colorRampPalette(c("blue", "red"))( 50 ), range = range(data))

mapped_colors <- generate_colors(data, mapper)
rownames(mapped_colors) <- rownames(data)

root_node_drawer <- NULL
inner_node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  grid.lines(x = c(x_parent, x_parent, x_node), y = c( y_parent, y_node, y_node),default.units = 'in')
}

leafHeight <- global_pars$leafIntervalLength
leaf_drawer <- function(parent, node) {
  inner_node_drawer(parent, node)

  # draw the leaf label
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  textGrob <- grid.text(
    label =  paste0('ID ',node$id),
    just = 'left',
    x= x_node + 0.1,
    y = y_node,
    default.units = 'in',
    gp = gpar(fontsize = 12)
  )

  colo <- mapped_colors[paste0(node$id),]
  xx <- unit(x_node + 0.35 * 1:length(colo), 'in') + grobWidth(textGrob)
  grid.rect(
    x = xx,
    y = y_node,
    width = 0.35,
    height = leafHeight,
    gp = gpar(fill = colo, col = NA),
    hjust = 0,
    vjust = 0.5,
    default.units = 'in'
  )

}

treeCustomizedDrawer(tree = ret,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = inner_node_drawer,
                     leaf_drawer = leaf_drawer)


# Circular heatmap --------------------------------------------------------
# intuitiveTree <- process_createNodes(2,numOfChildren = 3)
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc)
intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

## convert the colors
color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="RdYlBu")))(100)
mapper <- continuousMapper(colors = color, range = range(USArrests))

mapped_colors <- generate_colors(as.matrix(USArrests), mapper)
rownames(mapped_colors) <- rownames(USArrests)

## define the designer

circLayoutDesigner <- CircularLayoutDesigner$new();

intuitiveTree$innerRadius <- 0.5;
intuitiveTree$outterRadius <- 2
# get the right degree not easy
intuitiveTree$startDegree <- 30
intuitiveTree$extendDegree <- 270

circLayoutDesigner$calculate(intuitiveTree)
# circLayoutDesigner$plotTree(intuitiveTree)

half_leaf_degree <- get_global_pars()$leafIntervalLength * 0.5

xCenter <- circLayoutDesigner$xCenter;
yCenter <- circLayoutDesigner$yCenter;
# pretty circular layout
node_drawer <- function(parent, node) {
  radius_parent <- parent$xAxis_or_radius
  angle_parent <- parent$yAxis_or_angle

  radius_node <- node$xAxis_or_radius
  angle_node <- node$yAxis_or_angle

  parent_coor <-
    polar2cartesianCoor(radius_parent, angle_parent, xCenter, yCenter)

  node_coor <-
    polar2cartesianCoor(radius_node, angle_node, xCenter, yCenter)

  radius_x1 <- radius_parent
  angle_y1 <- angle_node
  point_coor <-
    polar2cartesianCoor(radius_x1, angle_y1, xCenter, yCenter)

  grid.segments(
    x0 = point_coor[1],
    y0 = point_coor[2],
    x1 = node_coor[1],
    y1 = node_coor[2],
    default.units = 'in'
  )

  draw_arc(angle_node, angle_parent, radius_parent, xCenter, yCenter,default_unit = 'in')
}

leaf_drawer <- function(parent, node) {
  node_drawer(parent,node)
  radius_parent <- parent$xAxis_or_radius
  angle_parent <- parent$yAxis_or_angle

  radius_node <- node$xAxis_or_radius
  angle_node <- node$yAxis_or_angle

  # parent_coor <-
  #   polar2cartesianCoor(radius_parent, angle_parent, xCenter, yCenter)

  node_coor <-
    polar2cartesianCoor(radius_node, angle_node, xCenter, yCenter)

  colo <- mapped_colors[node$label, ]

  for (i in 1:length(colo)) {
    inner_circle_radius <- 0.1 *i
    draw_sector(
      xCenter = xCenter,
      yCenter = yCenter,
      startDegree = angle_node - half_leaf_degree,
      endDegree = angle_node + half_leaf_degree,
      circle_inner_radius  = radius_node + inner_circle_radius,
      circle_out_radius = radius_node + inner_circle_radius + 0.1,
      fill_colo = colo[i]
    )
  }


}

treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = NULL,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = leaf_drawer)

root_node_drawer  <-
  function(rootNode) {
    radius_node <- rootNode$xAxis_or_radius
    angle_node <- rootNode$yAxis_or_angle

    node_coor <-
      polar2cartesianCoor(radius_node, angle_node, xCenter, yCenter)
    grid.circle(
      x = node_coor[1],
      y = node_coor[2],
      r = unit(3,'pt'),
      gp = gpar(fill = 'black', col = NA),
      default.units = 'in'
    )

    grid.segments(
      x0 = xCenter,
      y0 = yCenter,
      x1 = node_coor[1],
      y1 = node_coor[2],
      default.units = 'in'
    )
  }

treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = leaf_drawer)




# ComplexHeatmap partial mimic -----------------------------------------------------
ret <- process_createNodes(2,numOfChildren = 3)


set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.6, t = 0.1, b = 0.1))

rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(ret)


data <- matrix(rnorm(9 * 3), ncol = 3)
rownames(data) <- as.character(1:9)
mapper <- continuousMapper(colors = colorRampPalette(c("navy", "white", "firebrick3"))( 50 ), range = range(data))
mapper2 <- continuousMapper(colors = c("blue", "red"), range = range(data))

mapped_colors <- generate_colors(data, mapper)
rownames(mapped_colors) <- rownames(data)

mapped_colors2 <- generate_colors(data, mapper2)
rownames(mapped_colors2) <- rownames(data)

root_node_drawer <- NULL
inner_node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  grid.lines(x = c(x_parent, x_parent, x_node), y = c( y_parent, y_node, y_node),default.units = 'in')
}

leafHeight <- global_pars$leafIntervalLength
leaf_drawer <- function(parent, node) {
  inner_node_drawer(parent, node)

  # draw the leaf label
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  textGrob <- grid.text(
    label =  paste0('ID ',node$id),
    just = 'left',
    x= x_node + 0.1,
    y = y_node,
    default.units = 'in',
    gp = gpar(fontsize = 12)
  )

  colo <- mapped_colors[paste0(node$id),]
  xx <- unit(x_node + 0.5 * 1:length(colo), 'in') + grobWidth(textGrob)
  grid.rect(
    x = xx,
    y = y_node,
    width = 0.35,
    height = leafHeight,
    gp = gpar(fill = colo, col = NA),
    hjust = 0,
    vjust = 0.5,
    default.units = 'in'
  )

 colo <- mapped_colors2[paste0(node$id),]
  xx <- unit(x_node + 0.5 * length(colo) + 0.2 + 0.35 * 1:length(colo), 'in') + grobWidth(textGrob)
  grid.rect(
    x = xx,
    y = y_node,
    width = 0.35,
    height = leafHeight,
    gp = gpar(fill = colo, col = NA),
    hjust = 0,
    vjust = 0.5,
    default.units = 'in'
  )

}

treeCustomizedDrawer(tree = ret,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = inner_node_drawer,
                     leaf_drawer = leaf_drawer)

# 最后再添加个图例

grid.text('Type', x = 0.95, y = 0.85)
grid.rect(x = 0.95 , y = 0.8, width = 0.05,height = 0.05, gp = gpar(col =NA, fill = 'blue'))
grid.rect(x = 0.95 , y = 0.75, width = 0.05,height = 0.05, gp = gpar(col =NA, fill = 'red'))
