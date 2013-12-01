# -----------------------------------------------------------------------------
# FUNCTIONS: which.dummy which dummies
#   Which variables are dummy variables
#  
#  TODO: 
#   - allow for multiple names.
# -----------------------------------------------------------------------------



#' Identify which columns are dummy variables on a data frame.
#' 
#' Given a data frame and an optional variable name, \code{which.dummy}
#' identifies which colummns are dummy variables by the column index.
#' 
#' Given a data frame and an optional variable name, returns the indices of the
#' dummy columns.
#' 
#' Requires an \code{dummies} attribute, usually created by
#' \code{\link{dummy.data.frame}}.
#' 
#' @param data An object with a 'dummies' attribute
#' @param name Optional.  The name of a column that has been expanded to a
#' dummy variable.
#' @return integer vector of column indices corresponding to the dummy
#' variable(s)
#' @author Christopher Brown
#' @seealso \code{\link{dummy.data.frame}}, \code{\link{dummy}}
#' @keywords manip attribute
#' @examples
#' 
#'   data(iris)
#'   dat <- dummy.data.frame(iris)
#'   which.dummy(dat)
#' 
which.dummy <- function(data, name=NULL) {

  indexes <- integer()  

  if( ! is.null(name) ) {
     indexes <- attr( data, 'dummies' )[[name]] 
  } else {  

    if( is.null( attr( data, 'dummies' ) ) )
      stop( "Data does not appear to have dummy variables." )

    for( name in names( attr( data, 'dummies' ) ) )
      indexes <- append( indexes, attr( data, 'dummies')[[name]] )

      # indexes <- sapply( attr( data, 'dummies' ), I, USE.NAMES=F ) 

  }

  return( sort( as.integer( indexes ) ) )
}

