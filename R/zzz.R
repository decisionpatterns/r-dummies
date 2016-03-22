
.onAttach <- function( libname, pkgname ) {

  if( interactive() )
    packageStartupMessage( 
      pkgname ,
      "-" ,
      utils::packageVersion(pkgname, libname),
      " - Copyright \u00a9 ", substr(Sys.Date(),1,4),
      " Decision Patterns" ,
      domain = NA
    )
  
  # THIS SETS THE DEFAULTS FOR dummy.classes
  if( is.null( getOption("dummy.classes") ) )
    options( "dummy.classes" = c("factor","character") )

}


