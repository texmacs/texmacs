\name{tm}
\alias{tm}
\title{Convert classes and functions to classes of type "texmacs.*", for easy printing in TeXmacs}
\description{Classes \code{matrix} and \code{data.frame} can be nicely printed in TeXmacs if
the class is converted to \code{c("texmacs.matrix","matrix")} or \code{c("texmacs.data.frame","data.frame")}
This function will convert objects to that class.
It can also convert functions to create objects of that type (see example). }
\usage{tm(x)}
\arguments{
  \item{x}{
}
}

\value{The object returned is either an object where a class "texmacs.OLD-CLASS" has been
put into \code{class()}, or for a function, a new function that automatically
converts its output to a .texmacs class.}

\author{Michael Lachmann Tamarlin}




\examples{
a=matrix(1:200,20,10)
print(a)
print(tm(a))
matrix=tm(matrix)
a=matrix(1:200,20,10)
print(a)
assign("[.texmacs.matrix", tm( get ("[") ) )
assign("[.texmacs.data.frame", tm( get( "[")))
a[1:3,3:5] # We converted the subscript operator to maintain the texmacs. class
}
