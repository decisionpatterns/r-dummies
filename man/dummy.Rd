\name{dummy}
\Rdversion{1.1}
\alias{dummy}
\alias{dummy.data.frame}

\title{
  Flexible, efficient creation of dummy variables.
}
\description{
  This package flexibly and efficiently creates dummy variables for
  a variety of structures. 
}
\usage{
dummy(x, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE)

dummy.data.frame(data, names = NULL, omit.constants=TRUE, dummy.classes = getOption("dummy.classes"), all = TRUE, ...)
}

\arguments{

  \item{x}{ a single variable or variable _name_ } 
  \item{data}{ an object such as a data.frame or matrix that has colnames }
  \item{drop}{ 
    Whether to drop (i.e. omit) dummy variables for unused levels. 
    When \code{x} or \code{data[,x]} is a factor, this parameter  
    variables for only the used levels. By default, dummies are created only 
    for the used levels, i.e. TRUE.  
  }
  \item{sep}{
    For the names of the created dummy variables, sep is the character used 
    between the variable name and the value. 
  }
  \item{fun}{
    Function used to coerce values in the resulting matrix or frame.
  }  
  \item{verbose}{
    logical.  Whether to print(cat) the number of dummy variables created 
    Default: FALSE 
  }

For \code{dummy.data.frame} only: 

  \item{names}{
    The names of the columns to expand to dummy variables. Takes
    precedent over \code{dummy.classes} parameter.   
  }

  \item{dummy.classes}{ 
    ( For \code{dummy.data.frame} only )  A vector of classes names for 
    which dummy variables are created -or- "ALL" to create dummy variables
    for all columns irregardless of type.
    By default, dummy variables are produced for factor and character 
    class and be modified globally by options('dummy.classes' ).
  }

  \item{omit.constants}{
    Whether to omit dummy variables that are constants, i.e. contain only one 
    value. Overridden by \code{drop==FALSE}.  
  }
    
  \item{all}{ 
    ( For \code{dummy.data.frame} only ).  Whether to return columns
    that are not dummy classes.  The default is TRUE and returns all
    classes.  Non dummy classes are untouched.  
  }

  \item{...}{ arguments passed to \code{\link{dummy}} }

}

\details{

    \code{dummy} take a single variable OR the name of single variable  
    and a data frame. It coerces the variable to a factor and 
    returns a matrix of dummy variables using \code{\link{model.matrix}}.
    If the \code{data} has rownames, these are retained.

    Optionally, the parameter \code{drop} indicates that that dummy variables
    will be created for only the expressed levels of factors.  Setting it
    to false will produce dummy variables for all levels of all factors. 

    If there is only one level for the variable and \code{verbose == TRUE}, a 
    warning is issued before creating the dummy variable. Each element of this 
    dummy variable, will have the same value.

    A seperator, \code{sep}, can be specified for the seperator between
    the variable name and the value for the construction of new variable
    names.  The default is to provide no seperator.

    The type of values returned can be affected using the \code{fun}
    argument.  \code{fun} is called on each of the resultant dummy 
    variables. The only useful functions that the author has employed
    are \code{as.interger} (the default) or \code{as.logical}.


    \code{dummy.data.frame} takes a data.frame or matrix and returns
    a data.frame in which all specified columns are expanded as 
    dummy variables.  Specific columns can be named with the \code{names}
    argument or specified on a class basis by the \code{dummy.classes}
    argument. Specified names take precedent over classes.  The default
    is to expand dummy variables for character and factor classes, and
    can be controlled globally by \code{options('dummy.classes')}.  

    If the argument \code{all} is FALSE.  The resulting data.frame
    will contain only the new dummy variables.  By default, all columns
    of the object are returned in the order of the original frame.  
    Dummy variables are expanded in place.

    \code{omit.constants} indicates whether to omit dummy variables that
    assume only a single value.  This is the default.  If \code{drop==FALSE},
    constant variables are retained regardless of the setting.

}

\value{

\code{dummy} returns a matrix with the number of rows equal 
to the that of given variable.  By default, the matrix contains 
integers, but the exact type can be affected by \code{fun} argument.
Rownames are retained if the supplied variable has associate row names.

\code{dummy.data.frame} returns a data.frame in which variables are
expanded to dummy variables if they are one of the dummy classes. 
The columns are return in the same order as the input with dummy 
variable columns replacing the original column.

}

\references{

  http://wiki.r-project.org/rwiki/doku.php?id=tips:data-manip:create_indicator

  http://tolstoy.newcastle.edu.au/R/help/00b/1199.html

  http://tolstoy.newcastle.edu.au/R/help/03a/6409.html

  http://tolstoy.newcastle.edu.au/R/help/01c/0580.html

  Many other discussions on R-Help.  Too many to list.
}

\author{
  Christopher Brown
}

\seealso{

  \code{\link{model.frame}}, 
  \code{\link{model.matrix}}, 
  \code{\link{factor}}

}

\examples{

  letters <- c( "a", "a", "b", "c", "d", "e", "f", "g", "h", "b", "b" )
  dummy( as.character(letters) )
  dummy( letters[1:6] )
  
  l <- as.factor(letters)[ c(1:3,1:6,4:6) ]
  dummy(l)
  dummy(l, drop=FALSE)
  dummy(l, sep=":")
  dummy(l, sep="::", fun=as.logical)
  
  # TESTING NAS
  l <- c( NA, l, NA)
  dummy(l)
  dummy(l,sep=":")
  
  
  dummy(iris$Species)
  dummy(iris$Species[ c(1:3,51:53,101:103) ] )
  dummy(iris$Species[ c(1:3,51:53,101:103) ], sep=":" )
  dummy(iris$Species[ c(1:3,51:53) ], sep=":", drop=FALSE )     
  

  # TESTING TRAP FOR ONE LEVEL
  dummy( as.factor(letters)[c(1,1,1,1)] )
  dummy( as.factor(letters)[c(1,1,2,2)] )
  dummy( as.factor(letters)[c(1,1,1,1)] , drop = FALSE )   

  
  dummy.data.frame(iris)
  dummy.data.frame(iris, all=FALSE)


  dummy.data.frame(iris, dummy.class="numeric" )
  dummy.data.frame(iris, dummy.class="ALL" )


}

\keyword{ manip }
\keyword{ models }
