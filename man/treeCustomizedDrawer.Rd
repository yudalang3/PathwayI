% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_TreeDrawer.R
\name{treeCustomizedDrawer}
\alias{treeCustomizedDrawer}
\title{Draw the tree with customized ways.}
\usage{
treeCustomizedDrawer(tree, root_node_drawer, inner_node_drawer, leaf_drawer)
}
\arguments{
\item{tree}{A GraphicTree instance.}

\item{root_node_drawer}{a function parameter is node, tell the R how to draw the root node.}

\item{inner_node_drawer}{a function parameter is parent and node, tell the R how to draw the inner node.}

\item{leaf_drawer}{a function parameter is parent and node, tell the R how to draw the leaf.}
}
\description{
Note: see the examples for how to config the treeDrawer parameters.
}
\examples{
## a list that has three elements, the name should be precisely root_node_drawer, inner_node_drawer, leaf_drawer

root_node_drawer = function(node) {
  grid.circle(
    x = rootNode$xAxis_or_radius,
    y = rootNode$yAxis_or_angle,
    r = 0.02,
    gp = gpar(fill = 'cyan', col = NA)
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
    r = unit(4, 'pt'),
    gp = gpar(fill = 'black', col = NA)
  )
  x1 <- x_parent
  y1 <- y_node
  grid.segments(x_parent,y_parent,x1 = x_parent, y1 = y_parent, default.units = 'in')
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
    r = unit(4, 'pt'),
    gp = gpar(fill = 'black', col = NA)
  )
  grid.segments(x_parent,y_parent,x1 = x_parent, y1 = y_parent, default.units = 'in')
}

treeCustomizedDrawer(tree, root_node_drawer,inner_node_drawer,leaf_drawer)
}
