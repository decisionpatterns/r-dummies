#' Identify which columns are dummy variables on a data frame.
#' 
#' Given a data frame and an optional variable name, \code{which.dummy}
#' identifies which colummns are dummy variables by the column index.
#' 
#' Requires an \code{dummies} attribute, usually created by
#' \code{\link{dummy.data.frame}}.
#' 
#' @param data An object with a 'dummies' attribute
#' @param name Optional.  The name of a column that has been expanded to a
#' dummy variable.
#' 
#' @details 
#' 
#' Given a data frame and an optional variable name, returns the indices of the
#' dummy columns.
#' 
#' @return integer vector of column indices corresponding to the dummy
#' variable(s)
#' 
#' @seealso 
#'   \code{\link{dummy.data.frame}}, \cr
#'   \code{\link{dummy}} \cr
#'
#' @examples
#' 
#'   data(iris)
#'   dat <- dummy.data.frame(iris)
#'   which.dummy(dat)
#' 
#' @export 

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

