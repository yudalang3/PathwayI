###Statement#######################################################################
### This file contains the Application Programming interface (API)
### There are long-term a problem to deal with the graphics applications
### How to deal with the explosive parameters or bomb parameters.
### Generally speaking, we could handle in three ways:
### 1. Directly put it in the function as the parameters
### 2. like `par()` , providing a global function to set the parameters
### 3. The `ggplot2` style, overwrite the `+` operator to combine the parameters.
###
### Now, We choose the `ggplot2` style.

#' The parameter preparation of the painting process.
#'
#' @description
#' Inspired by the `ggplot2` package, We organized the parameters with the '+' symbols.
#'
#' @export
pDescriptor <- R6Class(
  "pDescriptor",
  public = list(
    #' @field membrane the cell membrane parameters.
    membrane = NULL,
    #' @field nuclearEnvelop the nuclear envelop parameters.
    nuclearEnvelop = NULL,
    #' @field nuclearDNA the nuclear DNA parameters.
    nuclearDNA = NULL,
    #' @field nodes the nodes parameters.
    nodes = NULL,
    #' @field edges the edges parameters.
    edges = NULL,
    #' @field name the name parameter. The title is stored here.
    name = NA_character_,
    #' @field gpar_title the gpar of the title.
    gpar_title = NULL,
    #' @field scaleCoordinate the coordinate re-scale.
    #'
    #' pp$scaleCoordinate <- c(realWidth, realHeight);
    scaleCoordinate = c(1,1),

    #' @description
    #' Workhouse function here.
    #'
    #' @export
    #'
    #' @examples
    #' print()
    #' # Note: this function may not need to be directly invoked.
    print = function() {

      if (is.null(self$membrane) && is.null(self$nuclearEnvelop) && is.null(self$nuclearDNA) && is.null(self$edges)) {
        if (is.null(self$nodes)) {
          stop("You at least need to input one name.")
        }
      }
      grid.newpage()

      if (!is.null(self$membrane)) {
        walk(self$membrane, function(x) self$draw_membrane(x))
      }

      if (!is.null(self$nuclearEnvelop)) {
        walk(self$nuclearEnvelop, function(x) self$draw_nuclearEnvelop(x))
      }

      if (!is.null(self$nuclearDNA)) {
        walk(self$nuclearDNA, function(x) self$draw_nuclearDNA(x))
      }

      if (!is.null(self$nodes)) {
        self$draw_nodes();
      }

      if (!is.null(self$edges)) {
        self$draw_edges();
      }

      if (!is.null(self$name) && !is.na(self$name)) {
        self$draw_title();
      }

    },

    #' @description
    #'
    #' Draw the membrane of the figure
    #'
    #' We separate the drawing process, this is the membrane drawing process
    #'
    #' @param membrane the list of the parameters
    #'
    draw_membrane = function(membrane) {

      # function(shape = c('ellipse','roundRect'),
      #          x = 0.5,
      #          y = 0.5,
      #          w = 2,
      #          h = 1.5,
      #          xyLocation = NULL,
      #          phos_height = 0.15,
      #          phos_dist = 0.1) {
      scaler <- self$scaleCoordinate


      x <- scaler[1] * membrane$x;
      y <- scaler[2] * membrane$y;
      w <- scaler[1] * membrane$w;
      h <- scaler[2] * membrane$h;
      ##phos_height <- scaler[2] * membrane$phos_height
      ##Not this
      phos_height <- membrane$phos_height

      drawer <-
        create_lipidBilayer_drawer(lipidModelIndex = 1, draw_circle_head = F)
      drawer$desired_height <- phos_height

      if ('points' == membrane$shape) {
        drawer$draw_lipid_along_curve(lineX = x ,
                                      lineY = y,
                                      closedPolygon = F)
      }else if ("ellipse" == membrane$shape) {
        ## Note: the circle unit the radius is 0.5
        df_lipidsPoints <-
          produce_model_coordinate_points(
            from = 0 ,
            to = 358,
            by = 2,
            radius = 0.5
          ) |> do_scale_affine(scaleHeight = h, scaleWidth = w) |>
          do_translate_affine(moveX = x + w * 0.5,
                              moveY = y + h * 0.5)
        drawer$draw_lipid_along_curve(lineX = df_lipidsPoints[1,] ,
                                      lineY = df_lipidsPoints[2,],
                                      closedPolygon = T)
      }else {
        # Use 20% of the shortest edge as the circle radius
        shortestEdge <- min(w,h)
        # r <- 0.1 * shortestEdge

        if (h < 0.2) {
          cat("Draw the membrane with horizontal line.\n")
          lineX <- seq.int(from = x , to = x + w, by = 0.1)
          lineY <- rep(y, length(lineX))
          drawer$draw_lipid_along_curve(lineX = lineX ,
                                        lineY = lineY,
                                        closedPolygon = F)
          return()
        }

        ## The simplest way:
        if (shortestEdge < 2) {
          cat("Sorry, in order to get better visual effects, you need to set both the width and height greater than 2.\n")
          show_surface_models(painter = drawer, yHigh = y + h, yLow = y, xStart = x, xEnd = x + w)

        }

        if (w %% 0.1 > 0 || h %% 0.1 > 0) {
          message("We detect the input membrane shape is round rectangle, but the width or height is not an integer multiple of 0.1 .\nSo, round(x,digits =  1) apply to them for the beauty.")
          w <- round(w, digits = 1);
          h <- round(h, digits = 1);
        }

        r <- 1
        space <- 0.1
        angle_for_roundCorner <- 5;
        df_lipidsPoints <- create_round_rectangle_xyCoords(
          x = x,
          y = y,
          w = w,
          h = h,
          r = r,
          space = space,
          angle_for_roundCorner = angle_for_roundCorner
        )

        drawer$draw_lipid_along_curve(lineX = df_lipidsPoints[1,] ,
                                      lineY = df_lipidsPoints[2,],
                                      closedPolygon = T)

        # grid.circle(df_lipidsPoints[1,],df_lipidsPoints[2,],r = unit(0.04 , 'in'), default.units = 'in')
      }

    },

    #' @param nuclearDNA the list of input parameters
    #'
    #' @description
    #' Draw the nuclear DNA of the figure
    #'
    #' We separate the drawing process, this is the DNA drawing process
    #'
    draw_nuclearDNA = function(nuclearDNA){
      # pp$nuclearDNA = list(x = x,
      #                      y = y ,
      #                      dnaType = dnaType,
      #                      numOfRepeats = numOfRepeats,
      #                      desired_height = desired_height)

      dnaHeight <- nuclearDNA$desired_height;
      scaler <- self$scaleCoordinate
      drawer_NDA <-
        create_DNA_drawer(
          dnaType = nuclearDNA$dnaType,
          numOfRepeats = nuclearDNA$numOfRepeats,
          desired_height = nuclearDNA$desired_height
        )
      drawer_NDA$draw_me(nuclearDNA$x * scaler[1], nuclearDNA$y * scaler[2] - 0.5 * dnaHeight)
    },

    #' @description
    #'
    #' Draw the edges of the figure
    #'
    #' We separate the drawing process, this is the edges drawing process
    #'
    draw_edges = function(){
      fix_indicator_df <- self$edges$fix_indicator_df
      free_indicator_df <- self$edges$free_indicator_df

      scaler <- self$scaleCoordinate

      if (!is.null(fix_indicator_df)) {
        ##TODO
      }

      if (!is.null(free_indicator_df)) {
        free_indicator_df <- as.data.frame(free_indicator_df,stringsAsFactors = FALSE)
        columnNames <- colnames(free_indicator_df)
        if (!all(c('x1','y1','x2','y2') %in% columnNames)) {
          stop("Sorrry, the x1/y1/x2/y2 columns are necessary. Please provide data.frame contains these values.")
        }

        ## Do scale
        free_indicator_df$x1 <- free_indicator_df$x1 * scaler[1]
        free_indicator_df$x2 <- free_indicator_df$x2 * scaler[1]
        free_indicator_df$y1 <- free_indicator_df$y1 * scaler[2]
        free_indicator_df$y2 <- free_indicator_df$y2 * scaler[2]

        ##
        indicator_drawer <- create_dege_drawer()

        pwalk(free_indicator_df, function(...) {
          aList <- list(...)
          curvature <- aList[['curvature']]
          if (is.null(curvature)) {
            indicator_drawer$curvature <- 0 # default is 0
          }else {
            indicator_drawer$curvature <- curvature
          }

          arrowLen <- aList[['arrowLen']]

          if (is.null(arrowLen)) {
            indicator_drawer$arrow_instance <- NULL
          }else {
            indicator_drawer$arrow_instance <- arrow(type = 'closed',length = unit(arrowLen, 'in'))
          }

          indicator_drawer$draw_me(aList$x1, aList$y1, aList$x2, aList$y2)
        })


      }
    },

    #' Draw the nodes of the figure
    #' @description
    #' We separate the drawing process, this is the nodes drawing process
    #'
    draw_nodes = function(){
      nodes <- self$nodes;
      data <- nodes$data; customizedDF <- nodes$customizedDF;

      scaler <- self$scaleCoordinate

      customizedParameters <- NULL;
      if (!is.null(customizedDF)) {
        customizedParameters <- new.env()
        walk(customizedDF, function(...) {
          aList <- list(...)

          customizedParameters[[aList$label]] <- aList

        })
      }

      data <- as.data.frame(data, stringsAsFactors = FALSE)
      columnNames <- colnames(data)
      if (!all(c('shape','x','y','width','height') %in% columnNames)) {
        stop("Sorrry, the shape/x/y/width/height columns are necessary. Please provide data.frame contains these values.")
      }

      pwalk(data, function(...) {
        aList <- list(...)

        shape <- aList[['shape']]
        x <- aList[['x']] * scaler[1]
        y <- aList[['y']] * scaler[2]
        width <- aList[['width']] * scaler[1]
        height <- aList[['height']] * scaler[2]


        bioGraphicNode <- switch(shape,
                                 ellipse = {
                                   create_oval_node(width, height)
                                 },
                                 rect = {
                                   create_rectangular_node(width, height)
                                 },
                                 roundRect = {
                                   create_round_rectangular_node(width, height)
                                 },
                                 {
                                   ## TODO
                                   ## add the specfic shape of self-contained

                                 })

        bioGraphicNode$xAxis_or_radius <- x + 0.5 * width;
        bioGraphicNode$yAxis_or_angle <- y + 0.5 * height;

        if (!is.null(aList[['label']])) {
          bioGraphicNode$label <- aList[['label']]
        }


        if (!is.null(customizedParameters)) {
          val <- customizedParameters[[bioGraphicNode$label]]
          if (!is.null(val)) {
            aList <- val;
          }
        }

        if (!is.null(aList[['inner_extension_ratio']])) {
          bioGraphicNode$inner_extension_ratio <- aList[['inner_extension_ratio']]
        }

        if (!is.null(aList[['drawLabel']])) {
          bioGraphicNode$if_draw_label <- aList[['drawLabel']]
        }
        if (!is.null(aList[['text_position']])) {
          bioGraphicNode$text_position <- aList[['text_position']]
        }
        if (!is.null(aList[['gpar_shape']])) {
          bioGraphicNode$gpar_shape <- aList[['gpar_shape']]
        }
        if (!is.null(aList[['gpar_text']])) {
          bioGraphicNode$gpar_text <- aList[['gpar_text']]
        }

        bioGraphicNode$draw_me();
      })

    },

    #' @param nuclearEnvelop the list of the parameters
    #'
    #' @description
    #' Draw the nuclear envelop of the figure
    #'
    #' We separate the drawing process, this is the nuclear envelop drawing process
    #'
    draw_nuclearEnvelop = function(nuclearEnvelop) {
      scaler <- self$scaleCoordinate
      # pp$nuclearEnvelop = list(
      #   shape = shape,
      #   x = x,
      #   y = y ,
      #   w = w,
      #   h = h,
      #   numOfIntervals = numOfIntervals,
      #   percentageOfRectangle = percentageOfRectangle,
      #   phos_height = phos_height
      # )
      #

      x <- scaler[1] * nuclearEnvelop$x;
      y <- scaler[2] * nuclearEnvelop$y;
      w <- scaler[1] * nuclearEnvelop$w;
      h <- scaler[2] * nuclearEnvelop$h;
      ## phos_height <- scaler[2] * nuclearEnvelop$phos_height
      ## Not this
      phos_height <- nuclearEnvelop$phos_height

      painter2 <- create_roundRectBilayer_drawer(numberOfIntervals = nuclearEnvelop$numOfIntervals,
                                                 percentageOfRectangle = nuclearEnvelop$percentageOfRectangle)
      painter2$desired_height <- phos_height

      if (nuclearEnvelop$shape == 'ellipse') {
        # Note: when drawing the circle unit, the unit is 0.5 not 1;
        df_nucleus_membrane <-
          produce_model_coordinate_points(
            from = 0,
            to = 360,
            by = 2,
            radius = 0.5
          ) |> do_scale_affine(scaleHeight = h, scaleWidth = w) |>
          do_translate_affine(moveX = x + w * 0.5,
                              moveY = y + h * 0.5)

        painter2$draw_along_curve(lineX = df_nucleus_membrane[1,], lineY = df_nucleus_membrane[2,])
      }else {

        # Use 20% of the shortest edge as the circle radius
        shortestEdge <- min(w, h)
        r <- 0.2 * shortestEdge
        space <- shortestEdge / 30
        angle_for_roundCorner <- space / r / ONE_DEGREE_IN_RADIAN;
        df_nucleus_membrane <- create_round_rectangle_xyCoords(
          x = x,
          y = y,
          w = w,
          h = h,
          r = r,
          space = space,
          angle_for_roundCorner = angle_for_roundCorner
        )

        # grid.lines(x = df_nucleus_membrane[1,], y = df_nucleus_membrane[2,],default.units = 'in')
        painter2$draw_along_curve(lineX = df_nucleus_membrane[1,], lineY = df_nucleus_membrane[2,])
      }

    },

    #' Draw the title of the figure
    #' @description
    #' We separate the drawing process, this is the title drawing process
    #'
    draw_title = function(){
      dim <- par('din')
      xx <- 0.5 * dim[1]; yy <- dim[2] - 0.3;
      grid.text(label = self$name, x = xx,y = yy, default.units = 'in',gp = self$gpar_title)
    }
  )

)

#' The main plus function to combine the parameters.
#'
#' @description
#' The returned instance is the later `pDescriptor` isntance.
#'
#'
#' @param e1 the `pDescriptor` instance
#' @param e2 the `pDescriptor` instance
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' p1 <- pDescriptor$new()
#' p1$name = 'p1'
#'
#' p2 <- pDescriptor$new()
#' p2$name = 'p2'
#'
#' p1 + p2
`+.pDescriptor` <- function(e1, e2) {


  e2$membrane <- list(e1$membrane, e2$membrane) |> unlist(recursive = F)

  e2$nuclearEnvelop <- list(e1$nuclearEnvelop, e2$nuclearEnvelop) |> unlist(recursive = F)

  e2$nuclearDNA <- list(e1$nuclearDNA, e2$nuclearDNA) |> unlist(recursive = F)

  if (is.null(e2$nodes)) {
    e2$nodes <- e1$nodes
  }

  if (is.null(e2$edges)) {
    e2$edges <- e1$edges
  }

  if (is.na(e2$name)) {
    e2$name <- e1$name
  }

  if (is.null(e2$gpar_title)) {
    e2$gpar_title <- e1$gpar_title
  }

  return(e2)
}


class_name_membrane <- 'membrane'

#' Prepare the membrane instance parameters
#'
#' @description
#' A ggplot style to create the parameters related to the membrane.
#'
#' @details
#' Users can specify necessary parameters with two approaches.
#' The first way is input the `shape`, `x`, `y`, `w` and `h`.
#' Or by input the `xyLocation` of the shape, the format is same as the conventions: 2xn matrix.
#'
#'
#' @param shape 'ellipse','roundRect', 'points' see details
#' @param x x of the start point
#' @param y y of the start point
#' @param w width of the shape
#' @param h height of the shape
#' @param xyLocation x,y locations of the shape, the format is 2xn matrix
#' @param phos_height height of the phospholipid model, in 'inchs' not apply the `pScaleCoord`
#' @param phos_dist the distance between two phospholipid
#'
#' @details
#' In `shape` parameter, the
#' ellipse and round rectangle are the regular shape.
#'
#' `points` means the users could directly import the x and y vector that contains the x/y locations.
#' In this case the w and h parameters are ignored.
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' membrane(shape = 'ellipse', x = 1, y = 0.2, w = 3, h =2)
membrane <-
  function(shape = c('ellipse','roundRect', 'points'),
           x = 0.5,
           y = 0.5,
           w = 2,
           h = 1.5,
           xyLocation = NULL,
           phos_height = 0.15,
           phos_dist = 0.1) {
    pp <- pDescriptor$new()

    mm <- structure(
      list(
        shape = shape,
        x = x,
        y = y ,
        w = w,
        h = h,
        xyLocation = xyLocation,
        phos_height = phos_height,
        phos_dist = phos_dist
      ),
      class = class_name_membrane
    )
    pp$membrane = list(mm)

    return(pp)
  }


class_name_nuclearEnvelop <- 'nuclearEnvelop'

#'  Prepare the nuclear envelop instance parameters
#'
#' @param x x of the start point
#' @param y y of the start point
#' @param w width of the shape
#' @param h height of the shape
#' @param numOfIntervals number of the intervals
#' @param percentageOfRectangle the percentage of the rectangle
#' @param shape c('ellipse','roundRect'), current support two of them
#' @param phos_height the phosolipid model height, in 'inchs' not apply the `pScaleCoord`
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' nuclearEnvelop(x = 3.3, y = 0.5, w = 1, h = 0.6)
nuclearEnvelop <-
  function(shape = c('ellipse','roundRect'),
           x = 2,
           y = 2,
           w = 1,
           h = 1,
           numOfIntervals = 10,
           percentageOfRectangle = 0.7,
           phos_height = 0.06) {

    shape <- match.arg(shape)
    pp <- pDescriptor$new()

    mm = structure(list(
      shape = shape,
      x = x,
      y = y ,
      w = w,
      h = h,
      numOfIntervals = numOfIntervals,
      percentageOfRectangle = percentageOfRectangle,
      phos_height = phos_height
    ), class = class_name_nuclearEnvelop)

    pp$nuclearEnvelop <- list(mm)

    return(pp)
  }

class_name_nuclearDNA <- 'nuclearDNA'

#' Prepare the nuclear DNA instance parameters
#'
#' @param x x of the start point
#' @param y y of the start point
#' @param dnaType choose the visual effects of the DNA, currently we support two types, 1 or 2
#' @param numOfRepeats the repeat of the DNA double helix motif
#' @param desired_height the height of the DNA
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' nuclearDNA(x = 3.5, y = 0.7)
nuclearDNA <-
  function(x,
           y,
           dnaType = 2,
           numOfRepeats = 3,
           desired_height = 0.35) {
    pp <- pDescriptor$new()

    mm <- structure(
      list(
        x = x,
        y = y ,
        dnaType = dnaType,
        numOfRepeats = numOfRepeats,
        desired_height = desired_height
      ),
      class = class_name_nuclearDNA
    )


    pp$nuclearDNA = list(mm)

    return(pp)
  }

#' The main pathway component expressed in here.
#'
#' <pre>
#'    Feature information:
#'    label : the name of the node
#'    x     : the x axis of the node
#'    y     : the y axis of the node
#'    width : the width the node, it can be used to change the circle to the ellipse
#'    height: the height of the node, it can be used to change the square to rectangle
#'    inner_extension_ratio
#'          : the extension ratio to make the pretty visual effect
#'    drawLabel
#'          : whether draw the label, default T, if the label not NA, the label will be drawed
#'    text_position
#'          : the numeric two elements vector, values between 0 and 1
#'    gpar_shape
#'          : the graphics parameters of the shape, same as the [grid] package.
#'    gpar_text
#'          : the graphics parameters of the text, same as the [grid] package.
#'    shape : the shape can be 'ellipse', 'rect' or 'roundRect', which means ellipse, rectangle and round corner rectangle.
#'            the node can be a template of the self-contained icon shape. see [create_selfContained_simple_bioGraphicsNode()]
#' </pre>
#' @param data the needed data.frame to draw the figure
#' @param customizedDF the customized elements in the data. see [[details]] for more information
#'
#' @details
#' Note: the `data` must should contain the columns of `shape/x/y/width/height` and the names should match exactly.
#'
#' If the `customizedDF` is non-null, the `label` should exist in the `data` and other features should be in the `customizedDF` data.frame.
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' df <- rbind(
#'   list('Lipoxygenases',5,4,0.4,0.6,0.2,c(0.5, 1.1), 'roundRect'),
#'   list(label='PPARa-1',x=3.2,y=2.5,w=1,h=0.8,ratio=0,tp=c(0.5, 0.5),'ellipse'),
#'   list(label='PPARa-2',x=4.8,y=2,w=0.7,h=0.8*0.7,ratio=0,tp=c(0.5, 0.5),'ellipse'),
#'   list(label='RXR-1',x=5,y=1.8,w=0.5,h=0.4,ratio=0.2,tp=c(0.5, 0.5),'roundRect'),
#'   list(label='PPARy',x=6,y=2,w=0.7,h=0.8*0.7,ratio=0,tp=c(0.5, 0.5),'ellipse'),
#'   list(label='RXR-2',x=6.3,y=1.8,w=0.5,h=0.4,ratio=0.2,tp=c(0.5, 0.5),'roundRect'),
#'   list(label=NA,x=4.3,y=1.45,w=0.5,h=0.4,ratio=0.2,tp=c(0.5, 0.5),'roundRect'),
#'   list(label=NA,x=5.9,y=1.45,w=0.5,h=0.4,ratio=0.2,tp=c(0.5, 0.5),'roundRect')
#' )
#' colnames(df) <- c("label","x","y","width","height","inner_extension_ratio","text_position", "shape")
#'
#' nodes(data = df)
nodes <- function(data, customizedDF = NULL) {
  pp <- pDescriptor$new()

  ret = list(data = data)

  if (!is.null(customizedDF)) {
    ret[['customizedDF']] <- customizedDF;
  }

  pp$nodes <- ret;
  return(pp)
}

#' Assign the edges component in this function.
#'
#' @param fix_indicator_df data.frame of fixed indicator, see `details` for format
#' @param free_indicator_df data.frame of free indicator, see `details` for format
#'
#' @details
#' The `fix_indicator_df` should must contain: `from` and `to` two columns.
#' The `free_indicator_df` should must contain: `x1`, `y1` , `x2` and `y2` four columns.
#' The `curvature` and `arrowLen` are optional, default values are 0 and 0.2 inchs.
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' ## Add the free arrow
#' df <- rbind(
#'   c(4.6,1.45,5,1.45,-1,0.1),
#'   c(6.2,1.45,6.6,1.45,-1,0.1),
#'   c(2.5,3.1,2.9,2.7,-1,0.1),
#'   c(3.5, 2.3, 4.3, 2,-1,0.1)
#' )
#'
#' colnames(df) <- c('x1','y1','x2','y2','curvature','arrowLen')
#'
#' edges(free_indicator_df = df)
#'
edges <- function(fix_indicator_df = NULL, free_indicator_df = NULL) {
  pp <- pDescriptor$new()

  ret = list(fix_indicator_df = fix_indicator_df)
  if (!is.null(free_indicator_df)) {
    ret[['free_indicator_df']] <- free_indicator_df;
  }
  pp$edges <- ret

  return(pp)
}

#' Assign the title in this function.
#'
#' @param name the title string
#' @param gpar_text the gprahic parameter of the text
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' pTitle("This is title")
pTitle <- function(name, gpar_text = NULL){
  pp <- pDescriptor$new()

  pp$name <- name;
  pp$gpar_title <- gpar_text;

  return(pp)
}


#' Assign the Scale Coordinate in this function.
#'
#' @details
#' For example, if users assign a rectangle with `x = 0.5, y = 0.5, w = 0.5, h =0.5` ,
#' while the `width = 1, height = 1`, and assign the `pScaleCoordinate(2, 2)` .
#' Then this function can scale the coordinate, making the
#' rectangle as `x = 1, y = 1, w = 1, h = 1`.
#'
#'
#' @param realWidth the width that you scale to
#' @param realHeight the height that you scale to
#'
#' @return the `pDescriptor` instance
#' @export
#'
#' @examples
#' pScaleCoord(2, 2)
pScaleCoord <- function(oriWidth = 1, oriHeight = 1, destWidth = 1, destHeight = 1){
  pp <- pDescriptor$new()

  pp$scaleCoordinate <- c(destWidth / oriWidth, destHeight/oriHeight);

  return(pp)
}


pTheme <- function(name = 'raw') {
  if (condition) {

  }
}
