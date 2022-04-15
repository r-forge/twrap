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
  \item{formula}{a formula of the form \code{y ~ f(x1) + g(x2)} where
    the variable on the left hand side is the grouping variable and
    the terms on the right hand side are functions of
    variables. For example, to summarize the \code{\link{iris}}
    data set one might use \code{Species ~ mean(Sepal.Length) +
      mean(Sepal.Width)}}

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
yat(Species ~
      median(Sepal.Length) +
      median(Sepal.Width) +
      median(Petal.Length) +
      median(Petal.Width),
    iris)


## Two different summaries of 'Sepal.Length'
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

## Writing a new simple summary function to use on the right hand side
minmax <- function(x, dig = 0) {
    paste(formatC(range(x, na.rm = TRUE),
                  format = "f",
                  digits = dig),
          collapse = " to ")
}
yat(am ~ minmax(hp), mtcars)

}

\keyword{univar}
\concept{summary table}
