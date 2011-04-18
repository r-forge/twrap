\name{tbl}
\alias{tbl}
\alias{tbl.default}
\alias{tbl.formula}

\title{Create a simple table showing counts and column proportions}

\description{ This function will calculate counts and column proportions for a
  one-way or two-way table.}

\usage{
\method{tbl}{default}(rowvar, ...)
\method{tbl}{formula}(formula, data = NULL, ..., subset, na.action = NULL, overall = FALSE, overall.label = "Overall")
}

\arguments{
  \item{rowvar}{
    The character vector or factor for which to report counts and proportions.
  }
  
  \item{formula}{
    A formula with the row variable on the left hand side
    and the column variable(s) on the right hand side. Think of this as a
    \dQuote{lattice-like} formula with the left hand side indicating the
    variable to place on the vertical axis and the right hand side
    indicating the variable to place on on the horizontal axis.
  }
  
  \item{data}{
    A \code{\link{data.frame}} from which the variables in \code{formula}
    should be taken.
  }
  
  \item{subset}{
    An optional vector specifying a subset of observations to be used
    for the calculations.
  }
  \item{na.action}{ }

  \item{overall}{
    If \code{TRUE}, then an overall summary is included. The default
    is \code{FALSE}.
  }

  \item{overall.label}{ If \code{overall = TRUE}, then this string is the identifier
    for the overall summary. The default is \code{"Overall"} but
    in the unlikely event that \code{"Overall"} is already a level in the
    data set then \code{overall.label} can be changed to something
    like \code{"All"}.
  }
  
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}

\references{
  R Development Core Team (2008).
  \emph{An introduction to R}.
  R Foundation for Statistical Computing.
  Available at \url{http://cran.r-project.org/manuals.html}
}

\author{Stephen Weigand \email{Weigand.Stephen@mayo.edu}}

\seealso{
  \code{\link{table}}, \code{\link{ftable}}, \code{\link{xtabs}}
}

\examples{

}
\keyword{ univar }
\keyword{ misc }

