\name{v}
\alias{v}
\title{v}
\description{insert the current plot into the TeXmacs buffer. The plot is converted to eps, and then put in the buffer}
\usage{v(width = F, height = F, format = "eps", method = "ps:", ...)}
\arguments{
  \item{width}{width of the plot. The actual width is controlled in TeXmacs.}
  \item{height}{height of the plot. Again, actual height is controlled in TeXmacs.}
  \item{format}{controls the format of the inserted image when method is "scheme:raw" or "scheme:hex". Possible values: "pdf", "png", "eps".}
  \item{method}{How should the image be inserted in TeXmacs. Supported values: "ps:", "scheme:raw", and "scheme:hex".}
  \item{\dots}{Additional parameters sent to dev.copy2eps}
}



\author{Michael Lachmann Tamarlin}





