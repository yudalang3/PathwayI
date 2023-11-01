# The blank area left for visualization
#

global_pars <- new.env()
global_pars[['blank_area_ratio']] <- list(l = 0.05, r = 0.05, t = 0.02, b = 0.02)
global_pars[['fontsize']] <- 12

#' Set the parameters for the global value.
#'
#' @description
#' There will be a lot of parameters to set for the plot, so this is the final entry to set the prameters.
#'
#' currently supportted parameters:
#' `fontsize` : the default fontsize of the text
#' `blank_area_ratio` : the blank area ratio of the picture, like margin in the 2D plot
#'
#'
#' @param name e.g. blank_area_ratio
#' @param value the value could be list or single value or automic vector
#'
#' @export
#'
#' @examples
#' set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.05, t = 0.02, b = 0.02))
#' set_global_pars('fontsize', 7)
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
#' get_global_pars() |> names()
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


## Global BioGraphicNodes
global_bioGraphics_nodes <- new.env()

#' Each bioGraphicsNode instance as well as some properties will be record, so users can get the instance here.
#'
#' The record will increase by the [[register_global_bioGraphics_nodes_list()]] function.
#'
#' @return a list of the needed information
#' @export
#'
#' @examples
#' get_global_bioGraphics_nodes_list() |> names()
get_global_bioGraphics_nodes_list <- function() {
  return(global_bioGraphics_nodes)
}

#' clear the records to release the memory
#'
#' @export
#'
#' @examples
#' clear_global_bioGraphics_nodes_list()
clear_global_bioGraphics_nodes_list <- function() {
  rm(list = ls(envir = global_bioGraphics_nodes),envir = global_bioGraphics_nodes)
}

#' Register global bioGraphics nodes list
#'
#' @param name your grob name
#' @param value a grob
#'
#' @export
#'
#' @examples
#' register_global_bioGraphics_nodes_list('name', grob)
register_global_bioGraphics_nodes_list <- function(name, value){
  global_bioGraphics_nodes[[name]] <- value;
  invisible()
}
