% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{print.plotSummaryAlphaPart}
\alias{print.plotSummaryAlphaPart}
\title{Print a plot generate by the function
\code{plotSummaryAlphaPart}}
\usage{
\method{print}{plotSummaryAlphaPart}(x, ask, ...)
}
\arguments{
\item{x}{plotSummaryAlphaPart, output object from
\code{\link[AlphaPart]{plot.summaryAlphaPart}} function}

\item{ask}{Logical, ask before printing another plot?}

\item{...}{Arguments passed to other functions (not used at the
moment).}
}
\description{
Plot output object from
\code{\link[AlphaPart]{plot.summaryAlphaPart}}.
}
\examples{
## Partition additive genetic values
(res <- AlphaPart(x=AlphaPart.ped, colPath="country", colBV=c("bv1", "bv2")))

## Summarize population by generation (=trend)
(ret <- summary(res, by="gen"))

## Plot the partitions
p <- plot(ret, ylab=c("BV for trait 1", "BV for trait 2"), xlab="Generation")
print(p[[1]])
print(p[[2]])
#print(p)
}
\seealso{
\code{\link[AlphaPart]{plot.summaryAlphaPart}}
}
