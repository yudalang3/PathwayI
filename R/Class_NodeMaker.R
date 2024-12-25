#' Make complex graphic node from a plenty of nodes.
#'
#' @param bioNodes is list of r package level grobs . @seealso [get_global_bioGraphics_nodes_list()]
#'
#' @return list of [[basic_bioGraphics_templates]]
#' @export
#'
#' @examples
#' make_complex_fromNodes()
make_complex_fromNodes <- function(bioNodes = get_global_bioGraphics_nodes_list()) {

  if (length(bioNodes) == 0) {
    stop("Current no bioGraphics nodes in the work space. You need to draw a picture first.")
  }

  bioNodes <- as.list(bioNodes)
  points <- imap(bioNodes, function(x,y){
    points <- extract_xyCoordinate_points(x$grob)
    points
  })

  rangeList <- map(points, function(x){
    x_range <- range(x[1,])
    y_range <- range(x[2,])
    return(c(x_range, y_range))
  })

  df_range <- do.call(rbind, rangeList)
  xMin <- min(df_range[, 1])
  yMin <- min(df_range[, 3])
  xMax <- max(df_range[, 2])
  yMax <- max(df_range[, 4])

  xCenter <- 0.5 * (xMin + xMax)
  yCenter <- 0.5 * (yMin + yMax)

  points <- map(points, function(x){
    x[1,] <- x[1,] - xCenter
    x[2,] <- x[2,] - yCenter
    x
  })

  ret <- list(
    width = xMax - xMin,
    height = yMax - yMin,
    points = points
  )
  return(ret)
}
