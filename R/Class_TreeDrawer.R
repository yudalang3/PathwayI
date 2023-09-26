

#' Draw the tree with customized ways.
#'
#' @description
#' Note: see the examples for how to config the treeDrawer parameters.
#'
#'
#' @param tree A GraphicTree instance.
#' @param root_node_drawer a function parameter is node, tell the R how to draw the root node.
#' @param inner_node_drawer a function parameter is parent and node, tell the R how to draw the inner node.
#' @param leaf_drawer a function parameter is parent and node, tell the R how to draw the leaf.
#'
#' @export
#'
#'
#' @examples
#' ## a list that has three elements, the name should be precisely root_node_drawer, inner_node_drawer, leaf_drawer
#'
#' root_node_drawer = function(node) {
#'   grid.circle(
#'     x = rootNode$xAxis_or_radius,
#'     y = rootNode$yAxis_or_angle,
#'     r = 0.02,
#'     gp = gpar(fill = 'cyan', col = NA)
#'   )
#' }
#' inner_node_drawer = function(parent, node) {
#'   x_parent <- parent$xAxis_or_radius
#'   y_parent <- parent$yAxis_or_angle
#'   x_node <- node$xAxis_or_radius
#'   y_node <- node$yAxis_or_angle
#'   grid.circle(
#'     x = x_node,
#'     y = y_node,
#'     r = unit(4, 'pt'),
#'     gp = gpar(fill = 'black', col = NA)
#'   )
#'   x1 <- x_parent
#'   y1 <- y_node
#'   grid.segments(x_parent,y_parent,x1 = x_parent, y1 = y_parent, default.units = 'in')
#'   # draw_bezier_twoPoints(
#'   #   x0 = x_parent,
#'   #   y0 = y_parent,
#'   #   x1 = x_node,
#'   #   y1 = y_node,
#'   #   default_unit = 'npc',
#'   #   vertical = F
#'   # )
#' }
#' leaf_drawer = function(parent, node) {
#'   x_parent <- parent$xAxis_or_radius
#'   y_parent <- parent$yAxis_or_angle
#'   x_node <- node$xAxis_or_radius
#'   y_node <- node$yAxis_or_angle
#'   grid.circle(
#'     x = x_node,
#'     y = y_node,
#'     r = unit(4, 'pt'),
#'     gp = gpar(fill = 'black', col = NA)
#'   )
#'   grid.segments(x_parent,y_parent,x1 = x_parent, y1 = y_parent, default.units = 'in')
#' }
#'
#' treeCustomizedDrawer(tree, root_node_drawer,inner_node_drawer,leaf_drawer)
treeCustomizedDrawer <-
  function(tree,
           root_node_drawer,
           inner_node_drawer,
           leaf_drawer) {

    if (!inherits(tree , what = 'GraphicTree')) {
      stop("You should input a class of GraphicTree")
    }


    grid.newpage()
    if (!is.null(root_node_drawer)) {
      root_node_drawer(tree$rootNode)
    }

    if (is.null(inner_node_drawer)) {
      inner_node_drawer <- function(x, y) {
      }
    }
    if (is.null(leaf_drawer)) {
      leaf_drawer <- function(x, y) {

      }
    }

    iterate_visulize_tree = function(node) {
      if (node$isLeaf()) {
        # nothing to do
      } else {
        for (child in node$children) {
          if (child$isLeaf()) {
            leaf_drawer(node, child)

          } else {
            inner_node_drawer(node, child)

          }
          iterate_visulize_tree(child)
        }
      }
      invisible()
    }

    iterate_visulize_tree(tree$rootNode)

    invisible()
  }
