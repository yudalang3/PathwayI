# LayoutDesigner ----------------------------------------------------------
# This is the layout designer. The main process it to compute the the node locations.
# But it also has default plot/draw process. So the full name should be Tree layout designer and defualt drawer.
#
# If you want to visualize in different ways, please see the `Class_TreeDrawer.R` file and invoke the [[treeCustomizedDrawer()]] function
#
# The subclass will implement the concrete visualize algorithms.
LayoutDesigner <-
  R6Class(
    classname = 'LayoutDesigner',
    public = list(
      # calculate the position or angle/ radius for the tree nodes.
      calculate = function(tree) {
        if (is.null(tree)) {
          stop("Where are you tree?")
        }
        if (!inherits(tree , what = 'GraphicTree')) {
          stop("You should input a class of GraphicTree")
        }

        dim_of_device <- par("din")
        width <- dim_of_device[1]
        height <- dim_of_device[2]

        blank_area <-
          calculate_blankArea(width = width, height = height)
        available_width <- width - blank_area$l - blank_area$r
        available_height <- height - blank_area$b - blank_area$t

        blank_space <-
          list(
            blank_area = blank_area,
            available_width = available_width,
            available_height = available_height
          )

        set_global_pars('wh_blank_space', blank_space)

        return(blank_space)
      },
      # input the tree
      plotTree = function(tree) {
        if (!inherits(tree , what = 'GraphicTree')) {
          stop("You should input a class of GraphicTree")
        }
        grid.newpage()
        self$configurate_graphic_parameters()


        self$draw_root_node_process(tree$rootNode)
        self$iterate_visulize_tree(tree$rootNode)
      },
      # An iterate method for visulize the tree
      iterate_visulize_tree = function(node) {
        if (node$isLeaf()) {

        } else {
          for (child in node$children) {
            self$draw_each_node_common_process(node, child)
            self$iterate_visulize_tree(child)
          }
        }
        invisible()
      }
    )
  )


# CircularLayoutDesigner --------------------------------------------------

#' @export
CircularLayoutDesigner <-
  R6Class(
    classname = 'CircularLayoutDesigner',
    inherit = LayoutDesigner,
    public = list(
      xCenter = 0.5,
      yCenter = 0.5,
      calculate =
        function(tree) {
          panelInfo <- super$calculate(tree)

          blank_area <- panelInfo$blank_area
          available_width <- panelInfo$available_width
          available_height <- panelInfo$available_height

          startDegree <- tree$startDegree
          extendDegree <- tree$extendDegree


          innerRadi <- tree$innerRadius

          # 外部也可以直接设定这个值
          if (tree$outterRadius < 0) {
            outterRadi <-
              tree$outterRadius <- min(available_width, available_height) * 0.5
          } else {
            outterRadi <- tree$outterRadius
          }
          direction <- tree$direction

          rootNode <- tree$rootNode
          longestDepth <- tree$longestDepth
          numOfLeaves <- tree$numOfLeaves

          self$xCenter <-
            tree$xCenter <- available_width * 0.5 + blank_area$l

          self$yCenter <-
            tree$yCenter <- available_height * 0.5 + blank_area$b


          if (extendDegree == 360) {
            numOfIntervals <- numOfLeaves
          } else if (extendDegree > 360) {
            stop("Sorry, the circle at most 360 degree.")
          } else {
            numOfIntervals <- numOfLeaves - 1
          }

          intervalLength <- extendDegree / numOfIntervals
          oneUnitDepth <- (outterRadi - innerRadi) / longestDepth
          leafNumberIndex <- 0

          set_global_pars('leafIntervalLength', intervalLength)
          set_global_pars('oneUnitDepth', oneUnitDepth)
          set_global_pars('xCenter', self$xCenter)
          set_global_pars('yCenter', self$yCenter)

          calcul <- function(node, cumulatedDepth) {
            depth <- cumulatedDepth + node$branchLength

            node$xAxis_or_radius = depth * oneUnitDepth + innerRadi

            if (node$isLeaf()) {
              node$yAxis_or_angle = leafNumberIndex * intervalLength + startDegree

              # 直接用<<-赋值符号
              leafNumberIndex <<- leafNumberIndex + 1
            } else {
              first_yAxis <- 0
              last_yAxis <- 0
              is_first_time <- T


              for (child in node$children) {
                calcul(child, depth)
                if (is_first_time) {
                  first_yAxis <- child$yAxis_or_angle
                  is_first_time = F

                }
                last_yAxis <- child$yAxis_or_angle
              }

              node$yAxis_or_angle = 0.5 * (first_yAxis + last_yAxis)
            }

            invisible()
          }
          calcul(rootNode, cumulatedDepth = 0)
        },
      configurate_graphic_parameters = function(x) {
        # pushViewport(viewport(
        #   x = 0.5,
        #   y = 0.5,
        #   width = 0.9,
        #   height = 0.9,
        #   default.units = 'snpc'
        # ))
        #

        # do not need anymore
      },
      draw_root_node_process =
        function(rootNode) {
          radius_node <- rootNode$xAxis_or_radius
          angle_node <- rootNode$yAxis_or_angle

          node_coor <-
            polar2cartesianCoor(radius_node, angle_node, self$xCenter, self$yCenter)
          grid.circle(
            x = node_coor[1],
            y = node_coor[2],
            r = unit(3, 'pt'),
            gp = gpar(fill = 'black', col = NA),
            default.units = 'in'
          )

          grid.segments(
            x0 = self$xCenter,
            y0 = self$yCenter,
            x1 = node_coor[1],
            y1 = node_coor[2],
            default.units = 'in'
          )
        },
      draw_each_node_common_process =
        function(parent,
                 node) {
          radius_parent <- parent$xAxis_or_radius
          angle_parent <- parent$yAxis_or_angle

          radius_node <- node$xAxis_or_radius
          angle_node <- node$yAxis_or_angle

          xCenter <- self$xCenter

          yCenter <- self$yCenter


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

          draw_arc(angle_node,
                   angle_parent,
                   radius_parent,
                   xCenter,
                   yCenter,
                   default_unit = 'in')
        }

    )
  )


# RectangularLayoutDesigner -----------------------------------------------
#' @export
RectangularLayoutDesigner <-
  R6::R6Class(
    classname = 'RectangularLayoutDesigner',
    inherit = LayoutDesigner,
    public = list(
      calculate = function(tree) {
        panelInfo <- super$calculate(tree)

        blank_area <- panelInfo$blank_area
        # panelInfo$available_width
        # panelInfo$available_height

        rootNode <- tree$rootNode
        longestDepth <- tree$longestDepth
        numOfLeaves <- tree$numOfLeaves

        numOfIntervals <- numOfLeaves - 1
        intervalLength <-
          panelInfo$available_height / numOfIntervals
        oneUnitDepth <- panelInfo$available_width / longestDepth

        leafNumberIndex <- 0

        set_global_pars('leafIntervalLength', intervalLength)
        set_global_pars('oneUnitDepth', oneUnitDepth)

        calcul <- function(node, cumulatedDepth) {
          depth <- cumulatedDepth + node$branchLength

          node$xAxis_or_radius = depth * oneUnitDepth + blank_area$l

          if (node$isLeaf()) {
            # node$yAxis = storage_env$leafNumberIndex * intervalLength;
            # storage_env$leafNumberIndex <- storage_env$leafNumberIndex + 1
            # cat("The leaf number is ", leafNumberIndex, "\n")
            node$yAxis_or_angle = leafNumberIndex * intervalLength + blank_area$b

            # 直接用<<-赋值符号
            leafNumberIndex <<- leafNumberIndex + 1
          } else {
            first_yAxis <- 0
            last_yAxis <- 0
            is_first_time <- T

            for (child in node$children) {
              calcul(child, depth)
              if (is_first_time) {
                first_yAxis <- child$yAxis_or_angle
                is_first_time = F
              }
              last_yAxis <- child$yAxis_or_angle
            }
            node$yAxis_or_angle = 0.5 * (first_yAxis + last_yAxis)
          }

          invisible()
        }

        calcul(rootNode, cumulatedDepth = 0)

      },
      draw_root_node_process = function(rootNode) {
        grid.circle(
          x = rootNode$xAxis_or_radius,
          y = rootNode$yAxis_or_angle,
          r = unit(4, 'pt'),
          gp = gpar(fill = 'black', col = NA),
          default.units = 'in'
        )
      },
      configurate_graphic_parameters = function(x) {
        # pushViewport(plotViewport(c(1, 1, 1, 1)))
        # # do not need
      },
      draw_each_node_common_process = function(parent, node) {
        x_parent <- parent$xAxis_or_radius
        y_parent <- parent$yAxis_or_angle
        x_node <- node$xAxis_or_radius

        y_node <- node$yAxis_or_angle

        grid.circle(
          x = x_node,
          y = y_node,
          r = unit(4, 'pt'),
          gp = gpar(fill = 'black', col = NA),
          default.units = 'in'
        )

        x1 <- x_parent
        y1 <- y_node

        grid.lines(
          x = c(x_parent, x1, x_node),
          y = c(y_parent, y1, y_node),
          default.units = 'in'
        )
        # grid.lines(x = c(x_parent, x_node), y = c(y_parent, y_node))
        # draw_bezier_twoPoints(
        #   x0 = x_parent,
        #   y0 = y_parent,
        #   x1 = x_node,
        #   y1 = y_node,
        #   default_unit = 'in',
        #   vertical = F
        # )
      }
    )
  )
