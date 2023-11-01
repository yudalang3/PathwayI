# 定义 Point 类
setClass("Point",
         slots = list(x = "numeric",
                      y = "numeric"))

# 创建 Point 对象的构造函数
setMethod("initialize", "Point",
          function(.Object, x = 0, y = 0) {
            .Object@x <- x
            .Object@y <- y
            return(.Object)
          })

# 创建显示 Point 对象的方法
setMethod("show", "Point", function(object) {
  cat("Point (x =", object@x, ", y =", object@y, ")\n")
})


# 定义 Rectangle 类
setClass("Rectangle",
         slots = list(
           x = "numeric",
           y = "numeric",
           w = "numeric",
           h = "numeric"
         ))

# 创建 Rectangle 对象的构造函数
setMethod("initialize", "Rectangle",
          function(.Object,
                   x = 0,
                   y = 0 ,
                   w = 1,
                   h = 1) {
            .Object@x <- x
            .Object@y <- y
            .Object@w <- w
            .Object@h <- h
            return(.Object)
          })

# 创建显示 Rectangle 对象的方法
setMethod("show", "Rectangle", function(object) {
  cat(
    sprintf(
      "Rectangle (x = %d, y = %d, w = %d, h = %d)\n",
      object@x,
      object@y,
      object@w,
      object@h
    )
  )
})

#' Note: for some situations, we need to define a formal point.
#' Thus, we could thinking like a object.
#' This is also support the vectorised usage.
#'
#' @param x x value
#' @param y y value
#'
#' @return the point instance
#' @export
#'
#' @examples
#' create_point(1,2)
#' create_point(x = 1:10, y = 2:11)
create_point <- function(x = 0, y = 0) {
  myPoint <- new("Point", x = x, y = y)
  return(myPoint)
}

#' Create the Rectangle object.
#'
#' @param x left bottom point x
#' @param y left bottom point y
#' @param w width
#' @param h height
#'
#' @return The Rectangle object
#' @export
#'
#' @examples
#' create_rectangle(1,1,1,1)
create_rectangle <- function(x = 0,
                             y = 0 ,
                             w = 1,
                             h = 1) {
  myPoint <- new(
    "Rectangle",
    x = x,
    y = y,
    w = w,
    h = h
  )
  return(myPoint)
}

#' Get distance from two points.
#' Vectorised operations are supported.
#'
#' @param x0 x of point1
#' @param y0 y of point1
#' @param x1 x of point2
#' @param y1 y of point2
#'
#' @return distance
#' @export
#'
#' @examples
#' distance(0,0,1,1)
distance <- function(x0, y0, x1, y1) {
  return(sqrt((x1 - x0) ^ 2 + (y1 - y0) ^ 2))
}

#' create a round rectangle xy coords, with the correct sequence order.
#'
#' @param x left bottom x
#' @param y left bottom y
#' @param w width
#' @param h height
#' @param r radius of the circle
#' @param space the space of the two points
#' @param angle_for_roundCorner the angle in the round corner, in degree, default is 5
#'
#' @return a desired 2xn matrix
#' @export
#' @importFrom dplyr distinct
#'
#' @examples
#' This is a beautiful
#' coords <- create_round_rectangle_xyCoords(x = 1, y = 1, w = 5, h = 4, r = 1, space = 0.1,angle_for_roundCorner = 6)
create_round_rectangle_xyCoords <-
  function(x, y, w, h, r, space, angle_for_roundCorner = 5) {
    if (space > r || r > w || r > h) {
      stop('Please check your input w,h,r and space. The space should <= r and r<= w, r<=h ')
    }

    xStart <- x
    yStart <- y

    ## directly is in radian
    angle_of_space <- angle_for_roundCorner * ONE_DEGREE_IN_RADIAN

    all_points <-
      seq.int(from = 0,
              to = degree_of_90_inRadian,
              by = angle_of_space)
    break_points_rightTop <-
      produce_model_coordinate_points(break_points = all_points, radius = r)


    break_points_rightTop[1, ] <-
      break_points_rightTop[1, ] + x + w - r
    break_points_rightTop[2, ] <-
      break_points_rightTop[2, ] + y + h - r

    all_points <-
      seq.int(from = degree_of_90_inRadian,
              to = degree_of_180_inRadian - 0.0000001,
              by = angle_of_space)
    break_points_rightBottom <-
      produce_model_coordinate_points(break_points = all_points, radius = r)
    break_points_rightBottom[1, ] <-
      break_points_rightBottom[1, ] + x + w - r
    break_points_rightBottom[2, ] <-
      break_points_rightBottom[2, ] + y + r

    all_points <-
      seq.int(from = degree_of_180_inRadian,
              to = degree_of_270_inRadian - 0.0000001,
              by = angle_of_space)
    break_points_leftBottom <-
      produce_model_coordinate_points(break_points = all_points, radius = r)
    break_points_leftBottom[1, ] <-
      break_points_leftBottom[1, ] + x + r
    break_points_leftBottom[2, ] <-
      break_points_leftBottom[2, ] + y + r

    all_points <-
      seq.int(from = degree_of_270_inRadian,
              to = degree_of_360_inRadian - 0.0000001,
              by = angle_of_space)
    break_points_leftTop <-
      produce_model_coordinate_points(break_points = all_points, radius = r)
    break_points_leftTop[1, ] <- break_points_leftTop[1, ] + x + r
    break_points_leftTop[2, ] <- break_points_leftTop[2, ] + y + h - r


    line_top <-
      rbind(seq.int(
        from = x + r,
        to = x + w - r,
        by = space
      ), y + h)
    line_right <-
      rbind(x + w, seq.int(
        from = y + h - r,
        to = y + r,
        by = -space
      ))
    line_bottom <-
      rbind(seq.int(
        from = x + w - r ,
        to = x + r,
        by = -space
      ), y)
    line_left <-
      rbind(x, seq.int(
        from = y + r,
        to = y + h - r,
        by = space
      ))


    # grid.newpage()
    # grid.lines(x = line_top[1, ] ,
    #            y = line_top[2, ],
    #            default.units = 'in')
    # grid.lines(x = line_right[1, ] ,
    #            y = line_right[2, ],
    #            default.units = 'in')
    # grid.lines(x = line_bottom[1, ] ,
    #            y = line_bottom[2, ],
    #            default.units = 'in')
    # grid.lines(x = line_left[1, ] ,
    #            y = line_left[2, ],
    #            default.units = 'in')
    #
    # grid.lines(x = break_points_rightTop[1, ] ,
    #            y = break_points_rightTop[2, ],
    #            default.units = 'in')
    # grid.lines(x = break_points_rightBottom[1, ] ,
    #            y = break_points_rightBottom[2, ],
    #            default.units = 'in')
    # grid.lines(x = break_points_leftBottom[1, ] ,
    #            y = break_points_leftBottom[2, ],
    #            default.units = 'in')
    # grid.lines(x = break_points_leftTop[1, ] ,
    #            y = break_points_leftTop[2, ],
    #            default.units = 'in')


    xyCoords <- cbind(
      line_top,
      break_points_rightTop,
      line_right,
      break_points_rightBottom,
      line_bottom,
      break_points_leftBottom,
      line_left,
      break_points_leftTop
    )

    ## remove duplication
    ##  I have try the for look with the thought of compare the current value and the next value
    ##  But it need lots of operations, and it get slow. So I use this function.
    ret <- distinct(data.frame(x = xyCoords[1,], y = xyCoords[2,]))

    # grid.newpage()
    # grid.lines(x = xyCoords[1, ] ,
    #            y = xyCoords[2, ],
    #            default.units = 'in')

    return(as.matrix(t(ret)))
  }



checkCurvePoints <- function(x, y) {
  #check
  len_ofX <- length(x)
  if (length(y) != len_ofX) {
    stop("The length of x and y should be equal.")
  }
  if (len_ofX < 2) {
    stop("The number should greater than 2.")
  }
  return(len_ofX)
}


#' Define the constont values
#' @export
ONE_DEGREE_IN_RADIAN <- pi / 180
#' @export
degree_of_180_inRadian <- pi
#' @export
degree_of_270_inRadian <- 270 * ONE_DEGREE_IN_RADIAN
#' @export
degree_of_360_inRadian <- 2 * pi
#' @export
degree_of_90_inRadian <- 90 * ONE_DEGREE_IN_RADIAN

#' Polar coordinates to Cartesian coordinates.
#' @description
#' The function supports the vectorised invoke usage.
#'
#' The implementation employs the euler's formula to accelerate the computation.
#'
#' NOTE the radius and location (xCenter, yCenter) should be the same unit with values(NOT unit object instances).
#'
#' * For example, you can input the inch unit values, not the inch object.
#'
#' @details
#' For the perspective of the generate mechanism, this function is same as [produce_model_coordinate_points()].
#' While the purpose is different, the latter is for the affine transformation.
#' * User can implement the same purpose for these two functions.
#'
#' @seealso [produce_model_coordinate_points()]
#'
#' @param radius: the circular radius
#' @param angle: unit is in degrees, NOT radian.
#' @param xCenter: the xCenter coordinate point
#' @param yCenter: the yCenter coordinate point
#'
#' @return the 2 x n matrix list, first row is the sin related values, second row is cos related values.
#' @export
#'
#' @examples
#' polar2cartesianCoor(1,5)
#' polar2cartesianCoor(radius = 1, angle = 5)
#'
#' # Vectorised usage example:
#' a <- polar2cartesianCoor(radius = 2, angle = 0:360,xCenter = 2,yCenter = 2)
#' grid.newpage()
#' grid.lines(x = a[1,], y = a[2,], default.units = 'in')

polar2cartesianCoor <- function(radius,
                                angle,
                                xCenter = 0.5,
                                yCenter = 0.5) {
  radian <- angle * ONE_DEGREE_IN_RADIAN

  # Way 1:
  # x <- radius * cos(radian) + xCenter
  # y <- radius * sin(radian) + yCenter
  # rbind(x, y)

  # Way 2:
  cc <- exp((1i) * radian)
  rbind(Im(cc) * radius + xCenter, Re(cc) * radius + yCenter)

}


#' Employ the bilateral extansion along a curve
#'
#' @param x x axis of curve
#' @param y y axis of curve
#' @param d Euclidean distance
#' @param closedPolygon whether this is a closed polygon
#'
#' @return a list of two line points
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- x ^ (1 / 3) + 2
#' do_bilateral_extension_alongCurve(x,y,d = 1.2)
do_bilateral_extension_alongCurve <- function(x, y, d = 1 , closedPolygon = F) {
  len_ofX <- checkCurvePoints(x, y)

  if (closedPolygon) {
    x_a <- x[1:len_ofX]
    y_a <- y[1:len_ofX]
    x_b <- c(x[2:len_ofX],x[1])
    y_b <- c(y[2:len_ofX],y[1])
  }else {
    x_a <- x[1:(len_ofX - 1)]
    y_a <- y[1:(len_ofX - 1)]
    x_b <- x[2:len_ofX]
    y_b <- y[2:len_ofX]
  }

  ## qq = - 1 / k
  qq <- (x_a - x_b) / (y_b - y_a)

  if (any(is.na(qq))) {
    stop('The input data has duplicated x,y locations, please check your data.')
  }

  # browser()
  t_square <- d * d / (1 + qq * qq)
  # Do not need this at all, because the qq is Inf, so the value will be zero.
  # t_square[is.infinite(qq)] <- 0
  #
  ## the choose of the t1 and t2 should be caution
  ## when the slot is positive and negative the value should revert
  t_1 <- sqrt(t_square)
  t_1[qq > 0] <- -t_1[qq > 0]
  t_1[x_b > x_a] <- -t_1[x_b > x_a]
  ## add the y consideration
  ##
  is_qq_equal_zero_plus_yB_greater_yA <- qq == 0 & y_b > y_a
  t_1[is_qq_equal_zero_plus_yB_greater_yA] <-
    -t_1[is_qq_equal_zero_plus_yB_greater_yA]

  t_2 <- -t_1


  xPrime_1 <- t_1 + x_a

  yPrime_1 <- t_1 * qq + y_a


  xPrime_2 <- t_2 + x_a

  yPrime_2 <- t_2 * qq + y_a

  is_inf <- is.infinite(qq)

  horizontal_to_right_indexes <- is_inf & qq < 0
  yPrime_1[horizontal_to_right_indexes] <-
    (y_a + d)[horizontal_to_right_indexes]
  yPrime_2[horizontal_to_right_indexes] <-
    (y_a - d)[horizontal_to_right_indexes]

  horizontal_to_left_indexes <- is_inf & qq > 0
  yPrime_1[horizontal_to_left_indexes] <-
    (y_a - d)[horizontal_to_left_indexes]
  yPrime_2[horizontal_to_left_indexes] <-
    (y_a + d)[horizontal_to_left_indexes]

  ret <-
    list(
      xPrime_1 = xPrime_1,
      yPrime_1 = yPrime_1,
      xPrime_2 = xPrime_2,
      yPrime_2 = yPrime_2
    )
  return(ret)
}


tan_to_sin_cos <- function(tan_value) {
  hypotenuse <- sqrt(1 + tan_value * tan_value)

  result <-
    list(sin = tan_value / hypotenuse , cos = 1 / hypotenuse)

  return(result)
}

#' Get the rotation of the two points.
#'
#' @param x1 point1 x
#' @param y1 point1 y
#' @param x2 point2 x
#' @param y2 point2 y
#'
#' @description
#' Note the return angle is already between 0 <= angle <= 90.
#'
#' <pre>
#'               point2 A.(x2,y2)       |       A
#'                                      |
#'    point1 B.(x1,y1)            C.    |    D       B           C
#' </pre>
#' Angle ABC is the returned value for left picture; angle ABD is the returned value in right picture
#'
#'
#' @return the rotation angle in radian
#' @export
#'
#' @examples
#' get_rotation_of_twoPoints(0,0,1,1)
get_rotation_of_twoPoints <- function(x1, y1, x2, y2) {
  # x_distance <- x1 - x2
  #
  # if (x_distance > 0) {
  #   direction <- -1
  # } else {
  #   direction <- -1
  # }
  #
  # horizontal_distance <- abs(x_distance)
  # vertical_distance <- abs(y1 - y2)
  #
  # if (horizontal_distance == 0) {
  #   theta <- 90 * ONE_DEGREE_IN_RADIAN
  #
  # } else {
  #   theta <- atan2(vertical_distance, horizontal_distance)
  # }
  #
  # return(direction * theta)

  return(atan2(y1 - y2, x2 - x1))
}


#' Extract xy Coordinate of points from a grid grob.
#'
#' @param grob The grob of the grid package.
#'
#' @return a 2xn matrix, where first row is the x axis and second row is the y axis.
#' @export
#'
#' @examples
#' extract_xyCoordinate_points(circleGrob)
extract_xyCoordinate_points <- function(grob) {
  coords <- grobCoords(x = grob, closed = T)
  a <- coords[[1]]
  ret <- matrix(data = c(a$x, a$y),
                nrow = 2,
                byrow = T)
  return(ret)
}
