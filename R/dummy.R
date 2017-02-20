#' Flexible, efficient creation of dummy variables.
#' 
#' This package flexibly and efficiently creates dummy variables for a variety
#' of structures.
#' 
#' @param x single variable or variable _name_
#'
#' @param data  object such as a data.frame, data.table, tibble or matrix that 
#' has colnames.
#' 
#' @param drop logical; Whether to drop (i.e. omit) dummy variables for unused levels.
#' When \code{x} or \code{data[[x]]} is a factor, this parameter variables for
#' only the used levels. By default, dummies are created only for the used
#' levels, i.e. TRUE.
#' 
#' @param sep string; separator character used between the variable name and 
#' the value in the dummy variable name
#' 
#' @param fun Function used to coerce values in the resulting matrix or frame.
#' Default: \code{as.integer}. \code{as.logical} or \code{as.factor} also work.
#' 
#' @param verbose logical; whether to print(cat) the number of dummy variables
#' created Default: \code{FALSE}
#' 
#' @details  
#' 
#' \code{dummy} take a single variable OR the name of single variable and a
#' data frame. It coerces the variable to a factor and returns a matrix of
#' dummy variables using \code{\link{model.matrix}}.  If the \code{data} has
#' rownames, these are retained.
#' 
#' The parameter \code{drop} indicates that that dummy variables
#' will be created for only the expressed levels of factors.  Setting it to
#' false will produce dummy variables for all levels of all factors.
#' 
#' If there is only one level for the variable and \code{verbose == TRUE}, a
#' warning is issued before creating the dummy variable. Each element of this
#' dummy variable, will have the same value.
#' 
#' A seperator, \code{sep}, can be specified for the seperator between the
#' variable name and the value for the construction of new variable names.  The
#' default is to provide no seperator.
#' 
#' The type of values returned can be affected using the \code{fun} argument.
#' \code{fun} is called on each of the resultant dummy variables. The only
#' useful functions that the author has employed are \code{as.interger} (the
#' default) or \code{as.logical}.
#' 
#' \code{dummy.data.frame} takes a data.frame or matrix and returns a
#' data.frame in which all specified columns are expanded as dummy variables.
#' Specific columns can be named with the \code{names} argument or specified on
#' a class basis by the \code{dummy.classes} argument. Specified names take
#' precedent over classes.  The default is to expand dummy variables for
#' character and factor classes, and can be controlled globally by
#' \code{options('dummy.classes')}.
#' 
#' If the argument \code{all} is FALSE.  The resulting data.frame will contain
#' only the new dummy variables.  By default, all columns of the object are
#' returned in the order of the original frame.  Dummy variables are expanded
#' in place.
#' 
#' \code{omit.constants} indicates whether to omit dummy variables that assume
#' only a single value.  This is the default.  If \code{drop==FALSE}, constant
#' variables are retained regardless of the setting.
#' 
#' @aliases dummy dummy.data.frame
#'
#' 
#' @return 
#' 
#' If \code{x} is atomic, a matrix (or data.frame) with the number of rows equal 
#' to the number elements of \code{x}. By result is an integer matrix, but the 
#' exact type can be affected by the \code{fun} argument. See examples. 
#'  
#' For data structures (i.e. data.frames, data.tables or tibbles), a similar 
#' structure with dummy columns expanded and replaced. Columns order is 
#' preserved. Rownames are maintained if applcable. 
#'  
#' @author Christopher Brown
#' 
#' @seealso
#' 
#'   \code{\link{model.frame}}, \cr
#'   \code{\link{model.matrix}}, \cr 
#'   \code{\link{factor}}
#' 
#' @keywords manip models
#' @examples
#'   
#'   letters <- c( "a", "a", "b", "c", "d", "e", "f", "g", "h", "b", "b" )
#'   dummy( letters )
#'   dummy( letters[1:6] )
#'   
#'   l <- as.factor(letters)[ c(1:3,1:6,4:6) ]
#'   dummy(l)
#'   dummy(l, drop=FALSE)
#'   dummy(l, sep=":")
#'   dummy(l, sep="::", fun=as.logical)
#'   
#'   # TESTING NAS
#'   l <- c( NA, l, NA)
#'   dummy(l)
#'   dummy(l,sep=":")
#'   
#'   
#'   dummy(iris$Species)
#'   dummy(iris$Species[ c(1:3,51:53,101:103) ] )
#'   dummy(iris$Species[ c(1:3,51:53,101:103) ], sep=":" )
#'   dummy(iris$Species[ c(1:3,51:53) ], sep=":", drop=FALSE )     
#'   
#' 
#'   # TESTING TRAP FOR ONE LEVEL
#'   dummy( as.factor(letters)[c(1,1,1,1)] )
#'   dummy( as.factor(letters)[c(1,1,2,2)] )
#'   dummy( as.factor(letters)[c(1,1,1,1)] , drop = FALSE )   
#' 
#'   
#' @export

dummy <- function( x, ... ) UseMethod("dummy")

#' @rdname dummy
#' @export


dummy.default <- function( 
    x
  , data    = NULL
  , sep     = getOption('dummy.sep', "" )
  , drop    = TRUE
  , fun     = as.integer
  , verbose = FALSE 
) { 

  # HANDLE IF DATA IS MISSING.  
    if( is.null(data) ) {
      name <- as.character( sys.call(1) )[2]   
      name <- sub( "^(.*\\$)", "", name )    # REMOVE prefix e.f
      name <- sub( "\\[.*\\]$", "", name )   # REMOVE suffix   
    } else {
      if( length(x) > 1 ) stop( "More than one variable provided to produce dummy variable." )  
      name <- x
      x    <- data[[name]]
    }


  # CHANGE TO FACTOR: KEEP LEVELS?
    if( drop == FALSE && class(x) == "factor" ) {
      x <- factor( x, levels=levels(x), exclude=NULL ) 
    } else {
      x<-factor( x, exclude=NULL )
    }
   

  # TRAP FOR ONE LEVEL :  
  #   model.matrix does not work on factor w/ one level.  Here we trap for the spacial case.
    if( length(levels(x))<2 ) {
      
      if( verbose ) warning( name, " has only 1 level. Producing dummy variable anyway." )

      return(          
        matrix( 
          rep(1,length(x)), 
          ncol=1, 
          dimnames=list( rownames(x), c( paste( name, sep, x[[1]], sep="" ) ) ) 
        )
      )

    }


  # GET THE MODEL MATRIX   
    mm <- model.matrix( ~ x - 1, model.frame( ~ x - 1 ),  contrasts=FALSE )  # vec
    colnames.mm <- colnames(mm) 

    if( verbose ) cat( " ", name, ":", ncol(mm), "dummy varibles created\n" ) 

    mm <- matrix( fun(mm), nrow=nrow(mm), ncol=ncol(mm), dimnames=list(NULL, colnames.mm) ) 


  # Replace the column names 'x'... with the true variable name and a seperator
    colnames(mm) <- sub( "^x", paste( name, sep, sep="" ), colnames(mm) )
    if(! is.null(row.names(data)) ) rownames(mm) <- rownames(data)

    return(mm)   

}
