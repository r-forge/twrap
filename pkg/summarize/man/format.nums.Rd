\name{format.medIQR}
\alias{format.medIQR}
\alias{print.medIQR}
\alias{format.medR}
\alias{print.medR}
\alias{format.meanSD}
\alias{print.meanSD}

\title{
  Format or print nice summaries of the median and IQR,
  the median and range, or the mean and SD.
}
\description{
  Format the median and interquartile range as either
  \dQuote{median (Q1 to Q3)} or \dQuote{median (Q1, Q3)};
  format the median and range similarly; format the
  mean and SD as \dQuote{mean (SD)}.
}
\usage{
\method{format}{medIQR}(x, digits = 2, format = "f", isep = " to ", ...)
\method{print}{medIQR}(x, quote = FALSE, ...)
\method{format}{medR}(x, digits = 2, format = "f", isep = " to ", ...)
\method{print}{medR}(x, quote = FALSE, ...)
\method{format}{meanSD}(x, digits = 2, format = "f", isep = " to ", ...)
\method{print}{meanSD}(x, quote = FALSE, ...)
}

\arguments{
  \item{x}{
    The object to format or print.
  }
  
  \item{digits}{
    The number of digits to display. This is passed to
    \code{\link{formatC}} so details are to be found there.
    Briefly, whether \code{digits} is the number of
    significant digits or number of digits after the
    decimal point depends on the \code{\link{format}}
    argument. By default \code{digits} is the number
    of digits after the decimal place.
  }
  \item{format}{
    The style of formatting. This is passed to
    \code{\link{formatC}} so details are to be found there.
    When \code{format = "f"}, the \code{digits} argument
    is interpreted as the number of digits after the decimal place.
    When \code{format = "g"} it is the number of significant digits.
  }
  \item{isep}{
    The interval separator, or how
    the quartiles should be separated. By default, the quartiles
    are separated by the word \emph{to} as in \dQuote{(Q1 to Q3)}.
    An alternative is to separate the quartiles with a comma
    as in \dQuote{(Q1, Q3)}. These are both preferred to
    separating the quartiles with a dash which is easily confused
    with a negative sign.
  }
  \item{quote}{
    Logical, indicating whether or not strings should be printed
    with surrounding quotes.
  }
  \item{\dots}{
    Other arguments passed to \code{\link{formatC}}.
  }
}
\details{  
  The convention of many biomedical journals is to summarize a
  distribution using a shorthand notation of the form \dQuote{median
  (IQR)} or \dQuote{mean (SD)}.  For example, some journal text might
  read \dQuote{\ldots the median (IQR) age of the treated group was 55
  (32 to 81) \ldots} while a table stub might read \dQuote{Age, median (IQR),
  years}.  This function facilitates this shorthand.
 }

\value{
  For univariate summaries, the formatted result is a character vector
  of length one.  For conditional summaries as given by the formula
  method, the formatted result is a one-row matrix with informative
  \code{\link{dimnames}}. The print method returns \code{x} invisibly.
}

\note{ Formatting and rounding floating point numbers is
  complicated. Important ideas are the number of significant digits
  versus number of decimal places to display, the round-to-even rule,
  and the fact that some numbers (even ``simple'' ones like like
  \code{0.15}) cannot be represented exactly and thus may not be rounded
  or formatted as some users expect.
}
  
\author{
  Stephen Weigand \email{Weigand.Stephen@mayo.edu}
}
\seealso{
  \code{\link{medIQR}}, \link{medR}, and
  \link{meanSD}; \code{\link{formatC}}, \code{\link{format}}. 
}
\examples{

data(sleep)
meanSD(sleep$extra)
result <- medIQR(extra ~ group, data = sleep)
result # to print
format(result)

}

\keyword{print}

