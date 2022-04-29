\name{summaries}
\alias{summaries}

\alias{nequals}
\alias{npct}
\alias{mediqr}
\alias{meansd}


\title{Simple Univariate Summary Functions}

\description{
  Simple univariate summary functions which are applied
  to each level of a grouping factor. These functions include
  arguments for labeling and formatting the summary.
}

\usage{
nequals(x, lab = NULL)
npct(x, lab = NULL, ref = NULL, dig = 0, p = NULL, p.args = NULL, ...)
mediqr(x, lab = NULL, dig = 0, p = NULL, p.args = NULL, ...)
meansd(x, lab = NULL, dig = 0, p = NULL, p.args = NULL, ...)
}

\arguments{

  \item{x}{The variable to summarize. If \code{x} is a categorical
    variable, it is suggested that it be a \code{\link{factor}}.
  }

  \item{lab}{A length-one character vector indicating the label to give
    to the summary.
  }

  \item{ref}{A length-one character vector indicating the single
    \dQuote{reference level} to use for a number-and-percentage
    summary provided by \code{npct}. For example, with a factor
    variable with levels of \code{"Yes"} and \code{"No"}, \code{ref =
      "Yes"} would give the number and percentage only for the Yeses.
    By default \code{ref} is \code{NULL} which means to summarize all
    levels of a factor.
  }

  \item{dig}{The number of digits past the decimal place to display
    in the formatted summary.
  }

  \item{p}{The abbreviation for the function to call for calculating a
    p-value.
    \tabular{ll}{
      \code{tt} \tab \code{t.test}\cr
      \code{ct} \tab \code{chisq.test}
    }
  }
  \item{p.args}{A list of further arguments that is passed to the p-value
    function. For example, \code{yat(outcome ~ npct(treatment, p = "ct",
      p.args = list(correct = FALSE)))}.
  }

  \item{...}{
    Currently ignored.
  }
}

\value{

  \describe{
    \item{nequals}{a character vector giving the number
      at each level of \code{x} in the form of the level followed
      by the number in parentheses. For example,
      \code{c("<group1> (n=<number1>)", ...)}. This kind of output
      often appears at the top of a summary table.}
   
    \item{npct}{a character vector giving, by default, the number and
      percentage at each level of \code{x}. If \code{ref} is
      specified, the result is a length-one character vector giving
      the number and percent for just the \dQuote{reference} level.}

    \item{mediqr}{a length-one character vector providing the formatted
      numeric summary of the median and inter-quartile range of \code{x}.}

    \item{meansd}{a length-one character vector providing the formatted
      numeric summary of the mean and SD of \code{x}.}

  }
  
  Each of these functions attaches the value of \code{lab} as a
  \code{\link{comment}} attribute. For the \code{npct} function, the
  \code{\link{comment}} will be a character vector where the first
  element is the value of \code{lab} and the other elements are the
  categories or levels summarized. This use (or abuse) of the
  \code{\link{comment}} attribute stems from \code{\link{model.frame}}
  allowing the \code{\link{comment}} attribute to \dQuote{pass through}
  to the resulting \code{\link{data.frame}.}

}

\note{
  If these functions are called on a vector of data the \code{p} and
  \code{p.args} arguments are ignored. It is only when these functions
  are used in a formula with a grouping variable on the left hand side
  that \code{yat} will calculate a p-value.
}

\author{
  Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

set.seed(1)

nequals(rpois(25, 1), lab = "Poisson lambda=1")

npct(factor(rbinom(25, size = 1, prob = 0.5),
            labels = c("Heads", "Tails")),
     lab = "Number (\%)",
     ref = "Heads")

mediqr(rnorm(25), dig = 1)

## 'p' ignored in univariate case
mediqr(rnorm(25), dig = 1, p = "tt")

meansd(rnorm(25), lab = "Mean (SD) of 25 N(0, 1)s", dig = 1)


data(warpbreaks)
yat(1 + wool ~
      meansd(breaks, p = "tt"),
    data = warpbreaks)

yat(1 + wool ~ npct(tension) + mediqr(breaks), warpbreaks)

}

\keyword{univariate}
