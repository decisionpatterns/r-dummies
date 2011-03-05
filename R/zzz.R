
# .onLoad <- function( libname, pkgname ) {
.First.lib <- function( libname, pkgname ) {

  packageStartupMessage(
    pkgname ,
    "-" ,
    utils::installed.packages()[ pkgname , "Version"],
    " provided by Decision Patterns.\n" ,
    domain = NULL 
  )

  # THIS SETS THE DEFAULTS FOR dummy.classes
  if( is.null( getOption("dummy.classes") ) )  
    options( "dummy.classes" = c("factor","character") )

}


