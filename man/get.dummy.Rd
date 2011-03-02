\name{get.dummy}
\Rdversion{1.1}
\alias{get.dummy}
\title{
   Get the dummy variable columns from a data frame.
}
\description{
  Given a data.frame and an optional variables name, return only the 
  columns that are dummy variables
}
\usage{
get.dummy(data, name = NULL)
}

\arguments{
  \item{data}{
    A data.frame with an dummies attributes
  }
  \item{name}{
    Optional.  The name of a variable. 
  }
}

\details{
   This uses \code{\link{which.dummy}} to identify the dummy columns.
}
\value{
    The subset of \code{data} that are dummy columns.
}
\author{
Christopher Brown
}

\seealso{
  \code{\link{which.dummy}},
  \code{\link{dummy.data.frame}}
}

\examples{

    data( iris ) 
    d <- dummy.data.frame( iris )
    get.dummy( d, 'Species' )

}
\keyword{ manip }
