\name{TeXmacsView}
\alias{start.view}
\alias{end.view}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function to start postscript TeXmacs interface  }
\description{
   Usually one uses the X11 device when interacting with TeXmacs. When
  X11 is not available (for example in a remote login, start.view() will
  open a postscript file with all defaults set correctly. end.view()
  will close that file. While working, use v() to insert the current
  file into the TeXmacs buffer.
}
\usage{
start.view()
end.view()
}
%- maybe also 'usage' for other objects documented here.
\details{
   
}
\author{ Michael Lachmann Tamarlin }

Make other sections like Warning with \section{Warning }{....} 
\seealso{ See also \code{\link{TeXmacs}},  }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function()
  {
    dev.off()
    op <- options("texmacs")$texmacs
    op$nox11 <- F
    options(texmacs=op)
    unlink(op$file)
  }
}
\keyword{ TeXmacs }% at least one, from doc/KEYWORDS
\keyword{ view }% __ONLY ONE__ keyword per line
\keyword{ device }
\keyword{ graphics }
\eof
\name{view}
\alias{v}
\alias{plotv}
\alias{pointsv}
\alias{linev}
\title{ Insert current graphic into TeXmacs }
\description{
  Once a graph is plotted and ready to go into the TeXmacs buffer,
  you insert it into the buffer using the v() function. The plotv,
  linev, and pointsv are just shortcuts for plot and then v. 
}
\usage{
v(width = 4, height = 4, ...)
plotv(...)
pointsv(...)
linev(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{width}{ Width of graph in inches. }
  \item{height}{ Height of graph in inches. }
  \item{\dots}{ Additional arguments for postscript. }
}
\author{ Michael Lachmann Tamarlin }

\seealso{ See Also \code{\link{TeXmacs}},  }
\examples{
x<-(0:600)/100;
plot(x,sin(x),type="l");
lines(x,cos(x),col=2);
legend(0,-0.5,c("sin(x)","cos(x)"),lty=1,col=c(1:2));
v();
plotv(x,sin(60/x),type="l");
}
\keyword{ TeXmacs }% at least one, from doc/KEYWORDS
\keyword{ graphics }% __ONLY ONE__ keyword per line
\keyword{ view }% __ONLY ONE__ keyword per line
\keyword{ device }% __ONLY ONE__ keyword per line
\keyword{ plot }% __ONLY ONE__ keyword per line
\eof
