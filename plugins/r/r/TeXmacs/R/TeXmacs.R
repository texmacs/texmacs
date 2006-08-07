.First.lib<-function(dir,package)
  {
    options(pager="cat")
    ps.options(pointsize=10)
  }
"end.view" <-
function()
  {
    dev.off()
    op <- options("texmacs")$texmacs
    op$nox11 <- F
    options(texmacs=op)
    unlink(op$file)
  }
"linev" <-
function(...) {
  line(...);v()
}
"plotv" <-
function(...) {
  plot(...);v()
}
"pointsv" <-
function(...) {
  points(...);v()
}
"start.view" <-
function(width=0,height=0,file=0,...){
  op <- options("texmacs")$texmacs
  if( length(op) <= 1 ) {
    op <- list()
    if( width > 0 ) {
      op$width <- width
    } else {
      op$width <-  4
    }
    if( height > 0 ) {
      op$height <- height
    } else {
      op$height <-  4
    }
    if( file==0) {
      op$file <-tempfile("postscript")
    } else {
      op$file <-  file
      
    }
    cat(op$file)
  }
  op$nox11 <- T
  postscript(file=op$file,one=F,horiz=F,width=op$width,height=op$height,...)
  options(texmacs=op)
}
"v" <-
function(width=F,height=F,...) {
  op <- options("texmacs")$texmacs
  got.par=T
  if( width== F ) { width=4;height=4;got.par=F }
  if( length(op) <= 1 ) {
    op <- list()
    op$width <- 4
    op$height <- 4
    op$nox11 <- F
  } else {
    if( !got.par ) {
      width = op$width
      height = op$height
    }
  }
  if( op$nox11 ) {
    dev.off()
    cat("\2ps:") ;
    system(paste("cat",op$file))
    cat("\5\n")
    postscript(file=op$file,one=F,horiz=F,width=width,height=height,...)
  } else {
    x<-recordPlot()
    op$file<-tempfile("postscript")
    postscript(command="echo",file=op$file,one=F,print.it=F,horiz=F,width=width,height=height,...) 
    replayPlot(x)
    dev.off()
    cat("\2ps:") ;
    system(paste("cat",op$file))
    cat("\5\n")
    unlink(op$file)
  }
  options(texmacs=op)
}
