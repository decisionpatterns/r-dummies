\name{which.dummy}
\Rdversion{1.1}
\alias{which.dummy}
\title{
  Identify which columns are dummy variables on a data frame.
}
\description{
  Given a data frame and  an optional variable name, 
  \code{which.dummy} identifies which colummns are dummy variables
  by the column index.
}

\usage{
which.dummy(data, name=NULL)
}
\arguments{
  \item{data}{ An object with a 'dummies' attribute }
  \item{name}{
     Optional.  The name of a column that has been expanded to 
     a dummy variable.
  }
}

\details{
  Given a data frame and an optional variable name, returns the 
  indices of the dummy columns.  

  Requires an \code{dummies} attribute, usually created by 
  \code{\link{dummy.data.frame}}.

}
\value{ 
  integer vector of column indices corresponding to the dummy variable(s)
}

\author{
  Christopher Brown 
}


\seealso{
  \code{\link{dummy.data.frame}},
  \code{\link{dummy}}
}

\examples{
  data(iris)
  dat <- dummy.data.frame(iris)
  which.dummy(dat)
}
\keyword{ manip }
\keyword{ attribute }
