# S7 dispatch for ggplot2 geom functions
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
