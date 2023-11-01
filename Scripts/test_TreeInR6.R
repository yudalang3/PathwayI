# Create tree manually ----------------------------------------------------

#
# # 压力测试
#
# system.time({
#   root <- process_createNodes(14)
# })
# root <- process_createNodes(16)
# lobstr::obj_size(root)
# displayTheTree(root$rootNode)
#

# customized tree drawer
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc)
intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer = function(node) {

  grid.circle(
    x = node$xAxis_or_radius,
    y = node$yAxis_or_angle,
    r = unit(4, 'pt'),
    gp = gpar(fill = 'black', col = NA),
    default.units = 'in'
  )
}
inner_node_drawer = function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle
  grid.circle(
    x = x_node,
    y = y_node,
    r = unit(2, 'pt'),
    gp = gpar(fill = 'black', col = NA),
    default.units = 'in'
  )
  x1 <- x_parent
  y1 <- y_node
  # grid.segments(x_parent,y_parent,x1 = x_parent, y1 = y_parent, default.units = 'in')
  grid.segments(
    x0 = x_parent,
    y0 = y_parent,
    x1 = x_node,
    y1 = y_node,
    default.units = 'in'
  )
  # draw_bezier_twoPoints(
  #   x0 = x_parent,
  #   y0 = y_parent,
  #   x1 = x_node,
  #   y1 = y_node,
  #   default_unit = 'npc',
  #   vertical = F
  # )
}
leaf_drawer = function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle
  grid.circle(
    x = x_node,
    y = y_node,
    r = unit(2, 'pt'),
    gp = gpar(fill = 'black', col = NA),
    default.units = 'in'
  )
  grid.segments(
    x0 = x_parent,
    y0 = y_parent,
    x1 = x_node,
    y1 = y_node,
    default.units = 'in'
  )
}
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = inner_node_drawer,
                     leaf_drawer = leaf_drawer)



## the simplest tree drawer -------------------------
# First, generate the tree
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc)
intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

# calculate the information for the rectangular layout
rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer <- NULL
node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle
  grid.segments(
    x0 = x_parent,
    y0 = y_parent,
    x1 = x_node,
    y1 = y_node,
    default.units = 'in'
  )
}
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = node_drawer)

# calculate the information for the circular layout
intuitiveTree$innerRadius <- 1
circLayoutDesigner <- CircularLayoutDesigner$new();
circLayoutDesigner$calculate(intuitiveTree)


xCenter <- circLayoutDesigner$xCenter;
yCenter <- circLayoutDesigner$yCenter;
node_drawer <- function(parent, node) {
  radius_parent <- parent$xAxis_or_radius
  angle_parent <- parent$yAxis_or_angle

  radius_node <- node$xAxis_or_radius
  angle_node <- node$yAxis_or_angle

  parent_coor <-
    polar2cartesianCoor(radius_parent, angle_parent, xCenter, yCenter)

  node_coor <-
    polar2cartesianCoor(radius_node, angle_node, xCenter, yCenter)

  grid.segments(
    x0 = parent_coor[1],
    y0 = parent_coor[2],
    x1 = node_coor[1],
    y1 = node_coor[2],
    default.units = 'in'
  )

}
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = node_drawer)

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
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = node_drawer)


## use bezier curve the draw the rectangular tree
# First, generate the tree
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc)
intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

# calculate the information for the rectangular layout
rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer <- NULL
node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  draw_bezier_twoPoints(
    x0 = x_parent,
    y0 = y_parent,
    x1 = x_node,
    y1 = y_node,
    default_unit = 'in',
    vertical = F
  )
}
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = node_drawer)


## use bezier curve the draw the rectangular tree
# First, generate the tree
intuitiveTree <- process_createNodes(2,numOfChildren = 6)

# calculate the information for the rectangular layout
rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer <- NULL
node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  draw_bezier_twoPoints(
    x0 = x_parent,
    y0 = y_parent,
    x1 = x_node,
    y1 = y_node,
    default_unit = 'in',
    vertical = F
  )
}
treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = node_drawer,
                     leaf_drawer = node_drawer)



## Dispaly the leaf name --------------------------------
# First, generate the tree
set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.2, t = 0.1, b = 0.1))
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc)
intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

# calculate the information for the rectangular layout
rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer <- NULL
inner_node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  grid.lines(x = c(x_parent, x_parent, x_node), y = c( y_parent, y_node, y_node),default.units = 'in')
}

leaf_drawer <- function(parent, node) {
  inner_node_drawer(parent, node)

  # draw the leaf label
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  grid.text(
    label =  node$label,
    just = 'left',
    x= x_node + 0.1,
    y = y_node,
    default.units = 'in',
    gp = gpar(fontsize = 8)
  )
}

treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = inner_node_drawer,
                     leaf_drawer = leaf_drawer)



### Any ggtree picture can be drawed  --------------------------------
# First, generate the tree
set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.2, t = 0.1, b = 0.1))
hc <- hclust(dist(USArrests), "ave")
dendrogram <- as.dendrogram(hc, hang = 0.1)

intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)

# calculate the information for the rectangular layout
rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(intuitiveTree)

root_node_drawer <- NULL
inner_node_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  grid.lines(x = c(x_parent, x_parent, x_node), y = c( y_parent, y_node, y_node),default.units = 'in')
}

colorMapper <- function(x){
  if (startsWith(x = x, prefix = 'M')) {
    return('wheat2')
  }else if(startsWith(x = x, prefix = 'N')){
    return('yellowgreen')
  }else {
    return('purple')
  }
}
leaf_drawer <- function(parent, node) {
  x_parent <- parent$xAxis_or_radius
  y_parent <- parent$yAxis_or_angle

  # draw the leaf label
  x_node <- node$xAxis_or_radius
  y_node <- node$yAxis_or_angle

  ## draw the leaf line color

  color <- colorMapper(node$label)

  grid.text(
    label =  node$label,
    just = 'left',
    x= unit(1 - global_pars$blank_area_ratio$r, 'npc') + unit(0.1,'in'),
    y = y_node,
    default.units = 'in',
    gp = gpar(fontsize = 8, col = color)
  )
  # Fill in the missing lines
  grid.segments(
    x0 = x_node,
    y0 = y_node,
    x1 = unit(1 - global_pars$blank_area_ratio$r, 'npc'),
    y1 = y_node,
    default.units = 'in',
    gp = gpar(lty = 'dashed',col = color)
  )

  grid.lines(x = c(x_parent, x_parent, x_node), y = c( y_parent, y_node, y_node),default.units = 'in',gp = gpar(col = color))
}

treeCustomizedDrawer(tree = intuitiveTree,
                     root_node_drawer = root_node_drawer,
                     inner_node_drawer = inner_node_drawer,
                     leaf_drawer = leaf_drawer)


