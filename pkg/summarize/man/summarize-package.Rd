\name{summarize-package}
\alias{summarize-package}
\alias{summarize}
\docType{package}
\title{
  Summarize distributions in a concise way
}
\description{
  Provide concise marginal or conditional numeric summaries
  of distributions using a formula interface
}
\details{
\tabular{ll}{
Package: \tab summarize\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-04-14\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Stephen Weigand \email{Weigand.Stephen@mayo.edu}
}

\keyword{ package }
\keyword{ univar }
\keyword{ misc }
\examples{
data(sleep)
meanSD(sleep$extra)
meanSD(extra ~ group, data = sleep)
medIQR(extra ~ group, data = sleep)
}
