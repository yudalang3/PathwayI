# 定义 Point 类
setClass("Point",
         slots = list(
           x = "numeric",
           y = "numeric"
         ))

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
create_point <- function(x = 0, y =0) {
  myPoint <- new("Point", x = x, y = y)
  return(myPoint)
}
