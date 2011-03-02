
# .onLoad <- function( libname, pkgname ) {
.First.lib <- function( libname, pkgname ) {

  # .odg.logo()
  .pack.banner(pkgname) 

  # THIS SETS THE DEFAULTS FOR dummy.classes
  if( is.null( getOption("dummy.classes") ) )  
    options( "dummy.classes" = c("factor","character") )

}


