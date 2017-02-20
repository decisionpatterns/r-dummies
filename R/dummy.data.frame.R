#' @param names The names of the columns to expand to dummy variables. Takes
#' precedent over \code{dummy.classes} parameter.
#' 
#' @param dummy.classes ( For \code{dummy.data.frame} only ) A vector of
#' classes names for which dummy variables are created -or- "ALL" to create
#' dummy variables for all columns irregardless of type.  By default, dummy
#' variables are produced for factor and character class and be modified
#' globally by \code{options('dummy.classes')}.
#' 
#' @param omit.constants Whether to omit dummy variables that are constants,
#' i.e. contain only one value. Overridden by \code{drop==FALSE}.
#' 
#' @param all ( For \code{dummy.data.frame} only ).  Whether to return columns
#' that are not dummy classes. The default is \code{TRUE} and returns all 
#' columns; non-dummy classes are untouched.
#' 
#' @param ...  arguments passed to \code{\link{dummy}}
#'
#' @examples
#'   dummy(iris)
#'   dummy(iris, all=FALSE)
#' 
#'   dummy(mtcars, dummy.class="numeric" )
#'   dummy(iris, dummy.class="ALL" )
#' 
#' @rdname dummy
#' @export

dummy.data.frame <- 
  function( 
      data
    , names = NULL
    , omit.constants = TRUE
    , dummy.classes = getOption("dummy.classes", c("factor","character") )
    , all = TRUE
    , ... 
) {

  # Initialize the data.frame
    df<-data.frame( row.names=row.names(data) )     
    new.attr <- list()  # Track location of dummy variables

    for( nm in names(data) ) {
      
      old.attr <- attr(df,'dummies')
      
      if(
        nm %in% names || 
        ( is.null(names) && ( dummy.classes == "ALL" || class(data[[nm]]) %in% dummy.classes ) )
      ) {

        dummies <- dummy( nm, data, ... )

        # OMIT CONSTANT COLUMNS:
        #  Variables that are constant will return a matrix with one column
        if( ncol(dummies) == 1  & omit.constants ) {
          dummies <- matrix( nrow=nrow(data), ncol=0 ) 
        }
            
        if( ncol(dummies)>0 ) new.attr[[nm]] <- (ncol(df)+1):( ncol(df)+ncol(dummies) ) 

      } else {
        if( ! all ) next()
        dummies <- data[,nm, drop=FALSE ]
      }

      df <- cbind(df, dummies)

    }

    attr( df, 'dummies' ) <- new.attr
    class(df) <- class(data)
    return(df)

}

#' @export 
#' @rdname dummy
dummy.matrix <- function(data, ... )
  dummy.data.frame(data,...)
  