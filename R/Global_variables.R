# The blank area left for visualization
#

global_pars <- new.env()
global_pars[['blank_area_ratio']] <- list(l = 0.05, r = 0.05, t = 0.02, b = 0.02)
global_pars[['gpar_text']] <- gpar( fontsize = 12)

#' Set the parameters for the global value. see the description for details.
#'
#' @description
#' There will be a lot of parameters to set for the plot, so this is the final entry to set the prameters.
#'
#' currently supportted parameters:
#' `blank_area_ratio` : the blank area ratio of the picture, like margin in the 2D plot
#' `gpar_text` : the text graphic parameters
#'
#' Note: see [set_global_text_pars()] if you only want to change the properties of the text.
#'
#' In most cases, the text properties are highly demand to be global.
#'
#' @param name e.g. blank_area_ratio
#' @param value the value could be list or single value or atomic vector
#'
#' @export
#' @examples
#' set_global_pars('blank_area_ratio', list(l = 0.05, r = 0.05, t = 0.02, b = 0.02))
#' set_global_pars('gpar_text', gpar(col = 'blue', fontsize = 7))
set_global_pars <- function(name, value) {

  if ('fontsize' == name ) {
    stop('Please use the new way to set the fontsize.')
  }
  global_pars[[name]] <- value

  invisible()
}

#' Quick set the text graphic parameters.
#'
#' @description
#' all the [gpar()] function supported parameter names, like:
#'
#' <pre>
#' col	Colour for lines and borders.
#' fill	Colour for filling rectangles, polygons, ...
#' alpha	Alpha channel for transparency
#' lty	Line type
#' lwd	Line width
#' lex	Multiplier applied to line width
#' lineend	Line end style (round, butt, square)
#' linejoin	Line join style (round, mitre, bevel)
#' linemitre	Line mitre limit (number greater than 1)
#' fontsize	The size of text (in points)
#' cex	Multiplier applied to fontsize
#' fontfamily	The font family
#' fontface	The font face (bold, italic, ...)
#' lineheight	The height of a line as a multiple of the size of text
#' font	Font face (alias for fontface; for backward compatibility)
#' </pre>
#'
#' @param name the name of the parameter
#' @param value the value
#'
#' @export
#'
#' @examples
#' set_global_text_pars('fontsize',  7)
set_global_text_pars <- function(name, value){
  global_pars$gpar_text[[name]] <- value
}

#' Quick get the global text gpar
#'
#' @return gpar class of text
#' @export
#'
#' @examples
#' get_global_text_pars()
get_global_text_pars <- function() {
  global_pars$gpar_text
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
  if (is.na(name) || is.null(name)) {
    return()
  }
  global_bioGraphics_nodes[[name]] <- value;
  invisible()
}
