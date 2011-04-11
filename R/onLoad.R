.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname,
            lib.loc=libname,
            morePaths=getOption('morePaths'))
  .jengine(TRUE)
}
