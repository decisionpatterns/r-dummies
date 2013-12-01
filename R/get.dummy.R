# -----------------------------------------------------------------------------
# FUNCTIONS: get.dummy get.dummies
#  Retrieve the dummy variables for a data.frame
# -----------------------------------------------------------------------------



#' Get the dummy variable columns from a data frame.
#' 
#' Given a data.frame and an optional variables name, return only the columns
#' that are dummy variables
#' 
#' This uses \code{\link{which.dummy}} to identify the dummy columns.
#' 
#' @param data A data.frame with an dummies attributes
#' @param name Optional.  The name of a variable.
#' @return The subset of \code{data} that are dummy columns.
#' @author Christopher Brown
#' @seealso \code{\link{which.dummy}}, \code{\link{dummy.data.frame}}
#' @keywords manip
#' @examples
#' 
#' 
#'     data( iris ) 
#'     d <- dummy.data.frame( iris )
#'     get.dummy( d, 'Species' )
#' 
#' 
get.dummy <- function(data,name=NULL) {
    
  if( ! is.null(name) ) {
    dat <- data[ , which.dummy(data, name) ]  
  } else {
    dat <- data[ , which.dummy(data) ] 
  }

  return(dat)
}
