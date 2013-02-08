\name{t.browseURL}
\alias{t.browseURL}
\title{Ask TeXmacs to open a url}
\description{This function will ask TeXmacs to open the url in a new buffer.}
\usage{\method{t}{browseURL}(url, help = F)}
\arguments{
  \item{url}{URL to be opened}
  \item{help}{Should the new buffer opened be a help buffer?}
}



\author{Michael Lachmann Tamarlin}
\note{TeXmacs sometimes takes a long time to render a web page. Just be patient...}



\examples{
t.browseURL("http://stat.ethz.ch/R-manual/")
}
