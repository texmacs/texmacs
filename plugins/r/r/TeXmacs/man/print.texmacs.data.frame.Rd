\name{print.texmacs.data.frame}
\alias{print.texmacs.data.frame}
\title{print texmacs data frame}
\description{A matrix can be converted to an object of type texmacs.matrix, which
then is printed as a table into TeXmacs, instead of as text.
Notice that most of the formatting options are usually set via
\code{options()}, instead of as arguments to this function.}
\usage{\method{print}{texmacs.data.frame}(x, row.names = T, col.names = T, left.border = options("texmacs.table.borders")[[1]], 
    right.border = options("texmacs.table.borders")[[1]], top.border = options("texmacs.table.borders")[[1]], 
    bottom.border = options("texmacs.table.borders")[[1]], rownames.color = options("texmacs.rownames.color")[[1]], 
    colnames.color = options("texmacs.colnames.color")[[1]], 
    grid = options("texmacs.table.grid")[[1]])}
\arguments{
  \item{x}{
}
  \item{row.names}{Should rownames be printed on the left}
  \item{col.names}{Should column names be printed}
  \item{left.border}{Width of borders, in pt.}
  \item{right.border}{
}
  \item{top.border}{
}
  \item{bottom.border}{
}
  \item{rownames.color}{background color of row labels}
  \item{colnames.color}{background color of column labels}
  \item{grid}{Width of grid separating cells in pt.}
}



\author{Michael Lachmann Tamarlin}




\examples{
options(texmacs.colnames.color="yellow")
options(texmacs.table.borders=0.3)
options(texmacs.table.grid=0.1)
a=tm(matrix(1:12,3,4))
print( a )
data.frame=tm(data.frame)
b=data.frame(x=1:10,y=10:1)
print(b)
}
