#' Open the GUI program written in the JAVA language.
#'
#' @description
#' The major purpose is to combine the merits of R and JAVA language.
#' and bridge the gap between scripted program and GUI software.
#'
#' @details
#' Why the `SkeletonMaker` exists?
#' The biggest reason is that the R can not do click click and click.
#'
#' Technique:
#'
#' Note: currently, we do not automatically reference the package `rJava` as the dependence in this package.
#'
#' Users need to install in yourself. The installation process is very easy. Just run `install.packages('rJava)`.
#'
#' Any problems, please contact us
#'
#' @export
#'
#' @examples
#' open_locationPicker_javaPrograme()
open_locationPicker_javaPrograme <- function() {

  # 使用require函数
  if (!require("rJava", character.only = TRUE)) {
    stop("Please install the rJava package first.\nYou can install via install.package('rJava')")
  }

  library(rJava)
  .jinit()
  .jclassPath()

  jarFile <- system.file('java', 'SkeletonMaker.jar',package = 'PathwayIlluminator',mustWork = T)
  .jaddClassPath(jarFile)
  .jclassPath()

  instance <- .jnew("edu.ucas.hias.pill.SkeletonMaker")
  .jcall(obj = instance,method = "start")


  # https://github.com/rstudio/rstudio/issues/11076
  # This is useful for aborted issue
  # .jcall("java/lang/System", "V", "exit", 0L)

}
