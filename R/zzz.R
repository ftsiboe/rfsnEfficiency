.onLoad <- function(libname, pkgname) {
  # Avoid scientific notation in printed output.
  options(scipen = 999)

  # data.table's `:=` creates these columns at runtime; register them so
  # R CMD check does not flag "no visible binding for global variable".
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(PACKAGE_GLOBALVARIABLES)
  }
  invisible()
}
