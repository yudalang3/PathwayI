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


#' Note: for some situations, we need to define a formal point.
#' Thus, we could thinking like a object.
#'
#' @param x x value
#' @param y y value
#'
#' @return the point instance
#' @export
#'
#' @examples
#' create_point(1,2)
create_point <- function(x = 0, y = 0) {
  myPoint <- new("Point", x = x, y = y)
  return(myPoint)
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

#' Employ the bilateral extansion along a curve
#'
#' @param x x axis of curve
#' @param y y axis of curve
#' @param d Euclidean distance
#'
#' @return a list of two line points
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- x ^ (1 / 3) + 2
#' do_bilateral_extension_alongCurve(x,y,d = 1.2)
do_bilateral_extension_alongCurve <- function(x, y, d = 1) {

  len_ofX <- checkCurvePoints(x, y)

  x_a <- x[1:(len_ofX - 1)]
  y_a <- y[1:(len_ofX - 1)]
  x_b <- x[2:len_ofX]
  y_b <- y[2:len_ofX]

  ## qq = - 1 / k
  qq <- (x_a - x_b) / (y_b - y_a)

  if(any(is.infinite(qq))){
    ret <-
      list(
        xPrime_1 = x_a,
        yPrime_1 = y_a + d,
        xPrime_2 = x_a,
        yPrime_2 = y_a - d
      )

    return(ret)
  }

  t_square <- d * d / (1 + qq * qq)

  ## the choose of the t1 and t2 should be caution
  ## when the slot is positive and negative the value should revert
  t_1 <- sqrt(t_square)
  t_1[qq > 0] <- -t_1[qq > 0]
  t_1[x_b > x_a] <- -t_1[x_b > x_a]

  t_2 <- -t_1


  xPrime_1 <- t_1 + x_a

  yPrime_1 <- t_1 * qq + y_a


  xPrime_2 <- t_2 + x_a

  yPrime_2 <- t_2 * qq + y_a


  ret <-
    list(
      xPrime_1 = xPrime_1,
      yPrime_1 = yPrime_1,
      xPrime_2 = xPrime_2,
      yPrime_2 = yPrime_2
    )
  return(ret)
}


#' Get distance from two points.
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

  return(atan2( y1 - y2, x2 - x1))
}

#' Draw the 2xn matrix in points
#'
#' @param default.units default is in
#' @param ... the 2xn matrix list
#'
#' @return null
#' @export
#'
#' @examples
#' draw_2xn_matrix_points(rbind(1:5,rep(3,5)))
draw_2xn_matrix_points <- function(... , default.units = 'in'){
  all_objs <- list(...)
  walk(all_objs, function(x){
    grid.lines(x[1,],x[2,],default.units = default.units)
  })
}
