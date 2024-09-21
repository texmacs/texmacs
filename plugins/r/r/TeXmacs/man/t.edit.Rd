\name{t.edit}
\alias{t.edit}
\title{Edit an R object inside TeXmacs, with nice formating.}
\description{Before TeXmacs had syntax highlighting for R, this function was a way to edit 
an R object formatted nicely.
The object is inserted into the input field of TeXmacs, so that it is easy to edit.}
\usage{\method{t}{edit}(x)}
\arguments{
  \item{x}{
}
}



\author{Michael Lachmann Tamarlin}



\seealso{\code{\link{edit}}, \code{\link{highlight}}, \code{\link{library(highlight)}}}
\examples{
f=function(n) {
	for(i in 1:100) {
		print(i)
	}
}
t.edit(f)
}
