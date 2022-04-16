\name{yat}
\alias{yat}

\title{
  Yet Another Table 
}

\description{
  Create a table of summary statistics using a formula interface. The
  left-hand-side of the formula corresponds to groups to create
  summaries by and the right hand side consists of functions of
  variables to summarize.
}

\usage{
yat(formula, data)
}

\arguments{

  \item{formula}{a \code{\link{formula}} with one or more grouping variables on the
    left hand side and \emph{functions} of one or more variables on the right
    hand side. The left hand side of the formula can include \code{1}
    which is treated like a \dQuote{total} variable for the data set.
    
    For example, to summarize sepal length and sepal width overall and
    by species in the \code{\link{iris}} data set, one might use
    \code{1 + Species ~ mean(Sepal.Length) + mean(Sepal.Width)}.
  }
  \item{data}{a \code{\link{data.frame}} to summarize}
}

\value{
  A \code{\link{data.frame}} with columns corresponding to the
  left hand side (grouping) variable.
}

\author{
  Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

## Using base functions on the right hand side
data(iris)
yat(Species + 1 ~ # left hand side means by Species and overall
      median(Sepal.Length) +
      median(Sepal.Width) +
      median(Petal.Length) +
      median(Petal.Width),
    iris)


## Two different summaries of 'Sepal.Length' by species
yat(Species ~ mean(Sepal.Length) + sd(Sepal.Length), iris)

## Using the 'mediqr' function in yat which provides formatted output
## and can have labels
yat(Species ~
      mediqr(Sepal.Length, lab = "Median length (IQR) --- cm", dig = 1) +
      mediqr(Sepal.Width, lab = "Median width (IQR) --- cm", dig = 1),
    iris)


## Numbers and percentages by group plus mean and SD for cylinders
## and horsepower by automatic (am=0) or manual (am=1) transmission
data(mtcars)

yat(am ~ npct(cyl, "Cylinders, n (\%)") +
         meansd(hp, "Mean horsepower (SD)"),
    mtcars)

## Writing a simple custom summary to use on the right hand side
minmax <- function(x, dig = 0) {
    paste(formatC(range(x, na.rm = TRUE),
                  format = "f",
                  digits = dig),
          collapse = " to ")
}
yat(1 + am ~ minmax(hp), mtcars)

}

\keyword{univar}
\concept{summary table}
