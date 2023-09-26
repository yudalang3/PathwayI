# The blank area left for visualization
#

global_pars <- new.env()
global_pars[['blank_area_ratio']] <- list(l = 0.05, r = 0.05, t = 0.02, b = 0.02)

#' Set the parameters for the global value.
#'
#' @param name e.g. blank_area_ratio
#' @param value the value could be list or single value or automic vector
#'
#' @export
#'
#' @examples
#' set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.05, t = 0.02, b = 0.02))
set_global_pars <- function(name, value) {
  global_pars[[name]] <- value

  invisible()
}

#' Get global parameters.
#'
#' @return the global pars
#' @export
#'
#' @examples
#' get_global_pars()
get_global_pars <- function() {
  global_pars
}

calculate_blankArea <- function(blank_area_ratio = global_pars[['blank_area_ratio']], width, height) {
  list(l = width * blank_area_ratio$l,
       r = width * blank_area_ratio$r,
       t = height * blank_area_ratio$t,
       b = height * blank_area_ratio$b
       )
}
