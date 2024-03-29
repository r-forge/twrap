\name{meanSD}
\alias{meanSD}
\alias{meanSD.default}
\alias{meanSD.formula}

\title{Calculate the mean and standard deviation}

\description{This function will calculate the mean and standard
  deviation of a distribution, possibly for each level of a grouping variable.}

\usage{
\method{meanSD}{default}(x, na.rm = TRUE, \dots)
\method{meanSD}{formula}(formula, data = NULL, \dots, subset,
   na.action = NULL,
   overall = FALSE,
   overall.label = "Overall")
}
  
\arguments{
  \item{x}{
    The numeric vector to summarize.
  }
  
  \item{formula}{
    A formula with the numeric vector on the left hand side and
    one or more grouping variables/factors on the right hand side.
  }
  
  \item{data}{
    A \code{\link{data.frame}} from which the variables in \code{formula}
    should be taken.
  }

  \item{subset}{
    An optional vector specifying a subset of observations to be used
    for the calculations.
  }

  \item{overall}{
    If \code{TRUE}, then an overall summary is included. The default
    is \code{FALSE}.
  }

  \item{overall.label}{
    If \code{overall = TRUE}, then this string is the identifier
    for the overall summary. The default is \code{"Overall"} but
    in the unlikely event that \code{"Overall"} is already a level in the
    data set then \code{overall.label} can be changed to something
    like \code{"All"}.
  }  
  
  \item{na.rm}{
    A logical value indicating whether \code{NA} values should
    be stripped before the computation proceeds.
  }
  
  \item{na.action}{
    A function which indicates what should happen when the data contain
    \code{NA}s. The default is to ignore missing values in either the
    response or the grouping variable(s).
  }
  
  \item{\dots}{
    Arguments passed to \code{\link{mean}}.
  }
}

\details{
  For the formula method, multiple variables on the right hand side
  of the formula are combined to form the unique levels by which
  the numeric vector is summarized. The underlying code is borrowed
  from \code{\link{boxplot}} so whatever works there will work here.

  An intercept-only model does not work currently but that would be
  useful.
}

\value{
  The default method returns a numeric vector of length 3 showing the
  median, Q1, and Q3.

  The formula method returns a numeric matrix with rows reprsenting
  the median, Q1, and Q3 and columns representing the factor levels.

  For both methods, the result is of class \code{"meanSD"} but
  it will probably change to to class \code{"sumry"} or something
  similar so that functions like \code{\link{meanSD}} return
  an object of the same class.
}

\author{
  Stephen Weigand \email{Weigand.Stephen@mayo.edu}
}

\seealso{
  \code{\link{medIQR}}, \code{\link{medR}}, \code{\link{summary}}, \code{\link{mean}}
}
\keyword{ univar }
\keyword{ misc }
\examples{
data(sleep)
meanSD(sleep$extra)
meanSD(extra ~ group, data = sleep)
}
