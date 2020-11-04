\name{start.view}
\alias{start.view}
\title{Start a postsctipt device for TeXmacs}
\description{Start a postscript device that is ready for sending graphs to TeXmacs. This is usefull when you are interacting with
a remote system that has no X11 interface, for example.}
\usage{\method{start}{view}(width = 0, height = 0, file = 0, ...)}
\arguments{
  \item{width}{Width of the device}
  \item{height}{Height of the device}
  \item{file}{Filename for the created plots. Defaults to tempfile("postscipt")}
  \item{\dots}{
}
}


\references{\url{https://www.texmacs.org}, \url{https://www.texmacs.org/tmweb/plugins/statistics.en.html#r}}
\author{Michael Lachmann Tamarlin}



\seealso{\code{\link{v}},\code{\link{end.view}},\code{\link{postscript}}}
\examples{
start.view()
plot(1:10,10:1)
v()
end.view()
}
