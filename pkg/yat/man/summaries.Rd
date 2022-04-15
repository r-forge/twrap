\name{summaries}
\alias{summaries}
\alias{mediqr}
\alias{meansd}
\alias{npct}

\title{Simple Univariate Summary Functions}

\description{
  Simple univariate summary functions which are applied
  to each level of a grouping factor. These functions include
  arguments for labeling and formatting the summary.
}

\usage{
mediqr(x, lab, dig)
meansd(x, lab, dig)
npct(x, lab, ref, dig)
}

\arguments{

  \item{x}{The variable to summarize. If \code{x} is a categorical
    variable, it is suggested that it be a \code{\link{factor}}.
  }

  \item{lab}{A length-one character vector indicating the label to give
    to the summary.
  }

  \item{ref}{A length-one character vector indicating the single
    \dQuote{reference level} to use for a number-and-percent
    summary provided by \code{npct}. For example, with a factor
    variable with levels of \code{"Yes"} and \code{"No"}, \code{ref =
      "Yes"} would give the number and percentage for the Yeses.
  }

  \item{dig}{The number of digits past the decimal place to display
    in the formatted summary.
  }

}

\value{

  For \code{mediqr} and \code{meansd}, a length-one character vector
  providing the formatted numeric summary of \code{x}. For \code{npct}
  without a specified reference level (i.e., \code{ref = NULL}) a
  character vector of number and percent for each category or level in
  \code{x}. (And if \code{ref} is specified, a length-one character
  vector of the number and percent for the reference level.)

  Each of these functions attaches the value of \code{lab} as a
  \code{\link{comment}} attribute. For the \code{npct} function, the
  \code{\link{comment}} will be a character vector where the first
  element is the value of \code{lab} and the other elements are the
  categories or levels summarized. This use or abuse of the
  \code{\link{comment}} attribute stems from \code{\link{model.frame}}
  allowing the \code{\link{comment}} attribute to \dQuote{pass through}
  to the resulting \code{\link{data.frame}}

}

\author{
  Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

set.seed(1)

mediqr(rnorm(25), dig = 1)

meansd(rnorm(25), lab = "Mean (SD) of 25 N(0, 1)s", dig = 1)

npct(factor(rbinom(25, size = 1, prob = 0.5),
            labels = c("Heads", "Tails")),
     lab = "Number (\%)",
     ref = "Heads")

}

\keyword{univariate}
