# package for interface between TeXmacs and R
# this package uses inlinedocs, so to regenerate the documentation do, in R:
# library(inlinedocs)
# package.skeleton.dx( "TeXmacs" )
# while being one directory above, i.e. where you can see the directory TeXmacs


.packageName <- "TeXmacs"
.onAttach<-function(dir,package)
  {
    options(pager="cat")
    options(width=78)

    options(texmacs.table.grid = NULL)
    options(texmacs.rownames.color="#eeeeee")
    options(texmacs.colnames.color="#eeeeee")
    options(texmacs.table.borders=0.1 )
    options(max.print=1000)
	
    assign("[.texmacs.matrix", tm( get ("[") ), envir=as.environment("package:TeXmacs") )
    assign("[.texmacs.data.frame", tm( get( "[")), envir=as.environment("package:TeXmacs"))

    ps.options(pointsize=10)
	
    assign( "t.help.quest.old", get("?"),envir=as.environment("package:TeXmacs"))
    assign("?",t.display.html.help, ,envir=as.environment("package:TeXmacs"))

    assign("TeXmacsR.version","0.16",envir=as.environment("package:TeXmacs") )
    motd="Run start.view() to use graphics without x11().\
    Notice: if you want density plots produced with image() to render much faster, use the option useRaster=T"

    packageStartupMessage("TeXmacs to R interface version ",TeXmacsR.version,"\n") 
    packageStartupMessage( paste(strsplit(motd," ")[[1]]," "))

    if( as.numeric( version$minor )*0.0001 + as.numeric(version$major ) >= 2.0011 ) {
      assign( "t.gethelpfile", utils:::.getHelpFile, envir=as.environment("package:TeXmacs"))
    } else if( as.numeric( version$minor )*0.0001 + as.numeric(version$major ) >= 2.0010 ) {
      assign( "t.gethelpfile" , 
        function (file) {
          path <- dirname(file)
          dirpath <- dirname(path)
          if (!file.exists(dirpath)) 
            stop(gettextf("invalid '%s' argument", "file"), domain = NA)
          pkgname <- basename(dirpath)
          RdDB <- file.path(path, pkgname)
          if (!file.exists(paste(RdDB, "rdx", sep = "."))) 
            stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", 
                          sQuote(pkgname)), domain = NA)
          tools:::fetchRdDB(RdDB, basename(file))
        }, envir=as.environment("package:TeXmacs") )
    }
    t.help.quest.old = get("?")
    assign("?",t.display.html.help, envir=as.environment("package:TeXmacs"))
  }

.First.lib=.onAttach

### Code for TeXmacs for start of section
DATA_BEGIN <- "\002"


### Code for TeXmacs for end of section
DATA_END <- "\005"

"end.view" <- # Close the postscript device for TeXmacs
	### This will call \code{dev.off} on the postscript device, and also remove the files created.
structure( function()
  {
    op <- options("texmacs")$texmacs
    dev.off( op$dev )
    rm( op$dev )
    
    unlink(op$file)
    rm( op$file )
    
    options(texmacs=op)
	##seealso<< \code{\link{v}},\code{\link{start.view}},\code{\link{postscript}}
	##references<< \url{http://www.texmacs.org}, \url{http://www.texmacs.org/tmweb/plugins/statistics.en.html}
  }	,ex=function() {
		start.view()
		plot(1:10,10:1)
		v()
		end.view()
	} )


"start.view" <- structure( function( # Start a postsctipt device for TeXmacs
	### Start a postscript device that is ready for sending graphs to TeXmacs. This is usefull when you are interacting with
	### a remote system that has no X11 interface, for example.
	width=0, ##<< Width of the device
	height=0, ##<< Height of the device
	file=0, ##<< Filename for the created plots. Defaults to tempfile("postscipt")
	... ##< additional paraeters to send to postscript().
	){
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
  postscript(file=op$file,one=F,horiz=F,width=op$width,height=op$height,...)
  dev.control("enable")
  op$dev = dev.cur()
  options(texmacs=op)
##seealso<< \code{\link{v}},\code{\link{end.view}},\code{\link{postscript}}
##references<< \url{http://www.texmacs.org}, \url{http://www.texmacs.org/tmweb/plugins/statistics.en.html#r}
},ex=function() {
	start.view()
	plot(1:10,10:1)
	v()
	end.view()
} )


t.cat.in2out<-function (
	### 
	filename, 
	before= paste(DATA_BEGIN,"ps: ", sep=""),
	after = paste("\n",DATA_END,"\n"),
	fix.html.bugs=F)
{
    con <- file(filename, "r")
    on.exit(close(con), add = T)
    a = rep("", 1000)
    start = T
    while (length(a) == 1000) {
        a = readLines(con, 1000)

        if( fix.html.bugs ) {
            # The following is a hack because TeXmacs fails on the backtick.
            a=gsub("`","[backtick]",a)
            a=gsub("&#96;","[backtick]",a)
            a=gsub("</p>","",a)
        }
        if (start) {
            on.exit(cat(after), add = T)
            cat( before)
            start = F
        }
        cat(a, sep = "\n");cat("\n")
    }
}


"v" <-
function
### insert the current plot into the TeXmacs buffer. The plot is converted to eps, and then put in the buffer
(width=F, ##<< width of the plot. The actual width is controlled in TeXmacs.
	height=F, ##<< height of the plot. Again, actual height is controlled in TeXmacs.
	format="eps", ##<< controls the format of the inserted image when method is "scheme:raw" or "scheme:hex". Possible values: "pdf", "png", "eps".
	method="ps:",
	### How should the image be inserted in TeXmacs. Supported values: "ps:", "scheme:raw", and "scheme:hex".
	... ##<< Additional parameters sent to dev.copy2eps
	) {
		
  op <- options("texmacs")$texmacs
  got.par=T
  if( width== F ) { width=4;height=4;got.par=F }
  if( length(op) <= 1 ) {
    op <- list()
    op$width <- 4
    op$height <- 4
  } else {
    if( !got.par ) {
      width = op$width
      height = op$height
    }
  }

  op$file<-tempfile("R_texmacs_plot")

  # currently there are several ways to add an image.
  # 1. using \2 ps:
  if( method == "ps:" ) {
    dev.copy2eps(file=op$file,print.it=F,width=width,height=height,...)
    try(    t.cat.in2out( op$file, 
		before=paste( DATA_BEGIN,"ps:width=0.8par\n",sep="") ), silent=T )
  }
  # 2. using \3 scheme: and raw-data
  if( method == "scheme:raw" ) {
    if( format == "eps" ) {
      dev.copy2eps(file=op$file,print.it=F,width=width,height=height,...)
    }
    if( format == "pdf" ) {
      dev.copy2pdf(file=op$file,width=width,height=height,...)
    }
    if( format == "png" ) {
      dev.print(device=png,file=op$file,width=400,height=400,...)
    }
    cat( DATA_BEGIN,"scheme:(image (tuple (raw-data \"",sep="")
    flush(stdout())
    system2("sed",args=c(" -e 's/\\\\/\\\\\\\\/g' -e 's/\\\"/\\\\\\\"/g' ",paste("<",op$file,sep="")),stdout=stdout())
    flush(stdout())    
    cat("\") \"",format,"\") \"0.8par\" \"\" \"\" \"\")",DATA_END,"\n",sep="")
  }
  
  if( method == "scheme:hex" ) {
    if( format == "eps" ) {
      dev.copy2eps(file=op$file,print.it=F,width=width,height=height,...)
    }
    if( format == "pdf" ) {
      dev.copy2pdf(file=op$file,width=width,height=height,...)
    }
    if( format == "png" ) {
      dev.print(device=png,file=op$file,width=400,height=400,...)
    }
    cat( DATA_BEGIN,"scheme:(image (tuple (#",sep="")
    flush(stdout())
    system2("hexdump",args=c("-v","-e","'1/1 \"%02X\"'",op$file),stdout=stdout())
    flush(stdout())    
    cat(") \"",format,"\") \"0.8par\" \"\" \"\" \"\")",DATA_END,"\n",sep="")
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
  

#  im=readLines(op$file)
#  cat(im,sep="\n")
#  cat(">png>||||>)\005\n",sep="")
#  unlink(op$file)
  options(texmacs=op)
}

t.matchAvailableTopics=function (text) 
{
    if (length(text) != 1L || text == "") 
        return(character(0L))
    ll = installed.packages()
    i = .packages()
    i = i[ i %in% rownames(ll) ]
    ll <- ll[ i, "LibPath"]
    indexFiles <- file.path(ll, names(ll), "help", "AnIndex")
    unique(unlist(lapply(indexFiles, function(f) {
        if (!file.exists(f)) 
            return(character(0L))
        foo <- scan(f, what = list("", ""), sep = "\t", quote = "", 
            na.strings = "", quiet = TRUE)[[1L]]
        grep(sprintf("^%s", utils:::makeRegexpSafe(text)), foo, value = TRUE)
    })))
}



t.tab.comp<-function(s,curs) # Send tab completion to TeXmacs
  ### t.tab.comp is usually not invoked by the user. When TeXmacs asks for a tab completion,
  ### this function is invoked, which sends the completion back to TeXmacs.
  {
    rc.settings(help=F) #This is because of a bug in matchAvailableTopics in package utils
    rc.settings(file=T)

    utils:::.assignLinebuffer(substr(s,1,curs))
    utils:::.assignEnd(nchar(s))
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
    l=utils:::.retrieveCompletions()
    l=sapply(l,function(x) {
      substr(x, nchar(utils:::.CompletionEnv[["token"]])+1,nchar(x) )
    } )
    i=grep("=$",l)
    if( (length(l[-i]) > 10) & (length(i)>0 ) )
    l=c(l[i])
    s3=utils:::.CompletionEnv[["token"]]
    s3=gsub("\"","\\\\\"",s3)
    deb.l <<- l
    
    cat("\2scheme:(tuple \"",s3,"\"",sep="") ;
    cat(" \"")
    if( length(l) > 0 )
    {
      cat(l,sep="\" \"")
    }
    else
    {
      #cat(s)
      cat("\"\"")
    }
    cat("\"")
    cat(")\5")
    
  }



t.send.menu = function(L, top=T, max.len=30) {
  if(top) {
    ret =  t.issue( "menu-bind","r-menu",
      "(\"update menu\" (insert \"t.update.menus( max.len=",max.len,")\")) (\"R help in TeXmacs\" (insert \"t.start.help()\")) --- ",
      t.send.menu(L,top=F,max.len) )
  } else {
    if( length(L) > max.len ) {
      L = c(L[1:(max.len-1)],list(NULL),More=list(L[-(1:(max.len-1))]) )
    }
    n=names(L)
    ret=""
    for(i in 1:length(L) ) {
      if( is.list(L[[i]]) ) {
        ret = paste(ret, "(-> \"",n[i],"\"",t.send.menu(L[[i]],top=F,max.len)," )" ,sep="",collapse="")
      } else {
        if( length(L[[i]] )==0 ) {
          ret = paste(ret," --- ")
        } else {
          ret = paste( ret, "(\"",n[i],"\" (insert \"",L[[i]],"\"))",sep="",collapse="")
        }
      }
    }
  }
  ret
}

t.make.menu=function() {
  b=unique(gsub("/[^/]*$","",searchpaths()))
  b=b[b!=".GlobalEnv" & b!="Autoloads"]
  libs=installed.packages(lib.loc=b)[,1:2]


  ret = lapply( 1:(dim(libs)[1]), function(i)  {
    deb.lib<<-i
    fs=c()
    try({fs = help.search(".*",package=libs[i,1],lib.loc=libs[i,2])$matches[,1]},TRUE)
    fs=as.list(fs)
    names(fs) = fs
    c( "load library"=paste("library(",libs[i,1],")"),
      "help "=paste("help(package=\\\"",libs[i,1],"\\\")",sep=""),
      list(NULL), fs )
  } )
  deb.ret <<- ret #debug info
  deb.libs <<- libs #debug info
  names(ret) = libs[,1]
  names(ret)[ names(ret) %in% .packages() ] = paste("*",names(ret[ names(ret) %in% .packages() ] ),sep="")
  deb.ret <<- ret #debug info
  
  ret
}

t.update.menus = function( # Update TeXmacs menus for R. 
	### Sends an updated set of menus from R to TeXmacs.
	max.len=30 ##< maximum length of single menu.
	) {
  cat( t.send.menu( t.make.menu(), max.len=max.len ) )
}

t.issue = function( ... ) {
      paste( "\2command:(",...,")\5\n",sep=" ",collapse="")
    }

# the following will allow you to edit a function inside TeXmacs.
t.edit = structure( function(x) { # Edit an R object inside TeXmacs, with nice formating.
	### Before TeXmacs had syntax highlighting for R, this function was a way to edit 
	### an R object formated nicely. 
	### The object is inserted into the input field of TeXmacs, so that it is easy to edit.
    cat( DATA_BEGIN,"input#",sep="")    
    cat( DATA_BEGIN,"texmacs: ",sep="")    

    on.exit({ cat( DATA_END,DATA_END,"\n",sep="") })
    cat(deparse(substitute(x)), " = ",sep="")
    t.nice(x)
    ##seealso<< \code{\link{edit}}, \code{\link{highlight}}, \code{\link{library(highlight)}}
#    source = deparse(x,control="useSource")  
#    if( tail(source,1)=="") source = head(source,-1)  
#    cat(paste(source,collapse="\n"))    
}, ex=function() {
	f=function(n) {
		for(i in 1:100) {
			print(i)
		}
	}
	t.edit(f)
} )



print.texmacs.matrix=
### Print a matrix into TeXmacs as a table
structure(
function( 
	### A matrix can be converted to an object of type texmacs.matrix, which
	### then is printed as a table into TeXmacs, instead of as text.
	### Notice that most of the formating options are usually set via
	### \code{options()}, instead of as arguments to this function.
	x,
	row.names=T, ##<< Should rownames be printed on the left
	col.names=T, ##<< Should column names be printed
	left.border=options("texmacs.table.borders")[[1]], ##<< Width of borders, in pt.
	right.border=options("texmacs.table.borders")[[1]],
	top.border=options("texmacs.table.borders")[[1]],
	bottom.border = options("texmacs.table.borders")[[1]],
	rownames.color=options("texmacs.rownames.color")[[1]], ##<< background color of row labels
	colnames.color=options("texmacs.colnames.color")[[1]], ##<< background color of column labels
	grid=options("texmacs.table.grid")[[1]] ##<< Width of grid separating cells in pt.
	) {
		if( length(x) > getOption("max.print")) {
			n = floor(getOption("max.print")/(dim(x)[2]))
			old.x.rows = dim(x)[1]
			on.exit(cat( paste( "\n[ reached getOption(\"max.print\") -- omitted",(old.x.rows-dim(x)[1])," rows ]\n") ))
			x = x[1:n,]
			bottom.border = F
		}
    text.width = apply(x,2,function(col) max(nchar(as.character(col))))
    if(is.null(rownames(x)) & row.names) rownames(x)=paste("[",1:dim(x)[1],",]",sep="")
    if(is.null(colnames(x)) & col.names) colnames(x)=paste("[,",1:dim(x)[2],"]",sep="")
	if( row.names ) 
	  if( col.names)
     	text.width = c(max(nchar(rownames(x))), pmax( text.width,nchar(colnames(x)))) + 2 
	  else 
	    text.width = max(nchar(rownames(x)))  + 2 
	if( 	( dim(x)[2]>1) && (sum(text.width) > options()$width) ) {
		text.width = cumsum( text.width )
		if( col.names )
		  i = max( c(2,which(text.width < options()$width ) ) )-1
		else 
		  i = max( c(1,which(text.width < options()$width ) ) )
		print(x[,1:i,drop=F],right.border=NULL,left.border=left.border,col.names=col.names,row.names=row.names,grid=grid)
		cat("\n\n\n")
		print(x[,-(1:i),drop=F],left.border=NULL,right.border=right.border,
		col.names=col.names,row.names=row.names,grid=grid)
	} else {
	pr.item=function(x) paste("  (cell \"",x,"\")",sep="")
	pr.row=function(x) { 
	  paste(c(" (row ",
	      sapply(x,pr.item),
	      	" )"))
	}

  expr = "(tabular (tformat "
  if(col.names) expr=c(expr,"(cwith 1 1 1 -1 \"cell-bborder\" \"0.5pt\")")
  if( !is.null(top.border) ) expr=c(expr,"(cwith 1 1 1 -1 \"cell-tborder\" \"",top.border,"pt\")")
  if( !is.null(bottom.border) ) expr=c(expr,"(cwith -1 -1 1 -1 \"cell-bborder\" \"",bottom.border,"pt\")")
  if( !is.null(left.border) ) expr=c(expr,"(cwith 1 -1 1 1 \"cell-lborder\" \"",left.border,"pt\")")
  if( !is.null(right.border) ) expr=c(expr,"(cwith 1 -1 -1 -1 \"cell-rborder\" \"",right.border,"pt\")")
  if( !is.null(grid) ) expr=c(expr,"(cwith 1 -1 1 -1 \"cell-rborder\" \"",grid,"pt\")")
  if( !is.null(grid) ) expr=c(expr,"(cwith 1 -1 1 -1 \"cell-bborder\" \"",grid,"pt\")")
  if( col.names & !is.null(colnames.color) )  
       expr=c(expr,"(cwith 1 1 1 -1 \"cell-background\" \"",colnames.color,"\")")
  if( row.names & !is.null(rownames.color) ) 
       if( col.names ) {
  	     if( !is.null(rownames.color) ) 
   		    expr=c(expr,"(cwith 2 -1 1 1 \"cell-background\" \"",rownames.color,"\")")
	     expr=c(expr,"(cwith 2 -1 1 1 \"cell-rborder\" \"0.5pt\")")
       } else {
	     if( !is.null(rownames.color) ) 
           expr=c(expr,"(cwith 1 -1 1 1 \"cell-background\" \"",rownames.color,"\")")
         expr=c(expr,"(cwith 1 -1 1 1 \"cell-rborder\" \"0.5pt\")")
       }
  expr=c(expr,"(table")
if(col.names) expr=c(expr,pr.row(c(if( row.names ) {""} else {c()},colnames(x))))
  expr = c(expr,sapply(1:dim(x)[1],
	function(i) { 
		pr.row( c(
  	     if( row.names ) {rownames(x)[i]} else {c()},
        	sapply(x[i,],as.character)))}))

  expr1=paste(c(expr,")))"),collapse="")
#  cat("scheme:",expr1,sep="")
  cat(DATA_BEGIN,"scheme:",expr1,"\n",DATA_END,sep="")
#cat("scheme:",expr1,sep="")
#invisible(expr)
}
},ex=function(){
options(texmacs.colnames.color="yellow")
options(texmacs.table.borders=0.3)
options(texmacs.table.grid=0.1)
a=tm(matrix(1:12,3,4))
print( a )
data.frame=tm(data.frame)
b=data.frame(x=1:10,y=10:1)
print(b)
} )

print.texmacs.data.frame=print.texmacs.matrix


tm=
structure(
function(x) { # Convert classes and functions to classes of type "texmacs.*", for easy printing in TeXmacs
    ### Classes \code{matrix} and \code{data.frame} can be nicely printed in TeXmacs if
    ### the class is converted to \code{c("texmacs.matrix","matrix")} or \code{c("texmacs.data.frame","data.frame")}
    ### This function will convert objects to that class.
    ### It can also convert functions to create objects of that type (see example). 
	if( is.function(x) ) {
		# return function that fixes class
		if( !is.null(attributes(x)$belongs.to) && attributes(x)$belongs.to=="texmacs") {
			x
		} else {
		structure( function( ob=NULL, ...) {
			if( !is.null(ob)) {
		   old.class = class(ob)
		   class(ob) = class(ob)[!grepl("^texmacs.",class(ob))]
		   ret = x(ob, ...)
		} else {
			ret=x(...)
		}
		   if( length(class(ret) > 0) ) {
			if( !grepl("^texmacs[.]",class(ret)[1]) ) {
			  n = paste( "texmacs.",class(ret)[1],sep="" )
			  if( length(grep( n, class(ret)) )==0 )
			     class(ret) = c(n, class(ret))
		   } else { 
		      class(ret) = old.class
		   }
		   ret		
		}
	}, belongs.to="texmacs" )
}
	} else {
 		old.class = class(x)
		i=grep("^(matrix|data.frame)$", old.class)
		ii = sapply( i, function(j)  length(grep( paste("^texmacs.",old.class[j],"$",sep=""), old.class))>0)
		i=i[!ii]
		if( length(i) > 0) {
		class(x)  = c( paste("texmacs.",old.class[i],sep=""),old.class)
	}
   		x
    }
### The object returned is either an object where a class "texmacs.OLD-CLASS" has been
### put into \code{class()}, or for a function, a new function that automatically
### converts its output to a .texmacs class.
}, ex=function() {
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
)










t.browseURL = structure( function(# Ask TeXmacs to open a url
	### This function will ask TeXmacs to open the url in a new buffer.
	url, ##<< URL to be opened
	help=F) { ##<< Should the new buffer opened be a help buffer?
  Qurl = paste("\"",url,"\"",sep="")
  if( help ) {
    cat(t.issue( "load-help-buffer", Qurl ))
  } else {
    cat(t.issue( "load-buffer", Qurl ))
  }
 ##note<< TeXmacs sometimes takes a long time to render a web page. Just be patient...
},ex=function() {
	t.browseURL("http://stat.ethz.ch/R-manual/")
} )
  

t.help = function( ... ) {
  if( as.numeric( version$minor )*0.0001 + as.numeric(
version$major ) >= 2.0011 ) {
    h = help(...,help_type="html")  
  } else {
    h = help(...,htmlhelp=T)
  }

  t.browseURL(h,help=T)
}

t.start.html.help = function() { # Start the R help system within TeXmacs
	### This doesn't currently work.
  assign("?",t.help.quest.old,envir = .GlobalEnv)
      options(browser=t.browseURL)
    options(help_type="html")
    }

t.start.inline.help = function() { # Start the R help system within TeXmacs
	  assign("?",t.help.quest.old,envir = .GlobalEnv)
	      options(browser=t.browseURL)
	    options(help_type="html")
	}


# Doesn't currently work. Also seems to be a moving target,
# because R keeps changing how things are done....
#
#assign("?",t.help)



t.display.html.help=function( # Display R help within the R session in TeXmacs
	### This function will display a help document within the current R session.
	### Usually, you will not call this function, but instead do
	### \code{assign("?",t.display.html.help)}
	e1, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type"))  {
  {
  type <- NULL
  topicExpr <- substitute(e1)
    if (is.call(topicExpr) && topicExpr[[1L]] == "?") {
        search <- TRUE
        topicExpr <- topicExpr[[2L]]
        if (is.call(topicExpr) && topicExpr[[1L]] == "?" && is.call(topicExpr[[2L]]) && 
            topicExpr[[2L]][[1L]] == "?") {
            cat("Contacting Delphi...")
            flush.console()
            Sys.sleep(2 + rpois(1, 2))
            cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
            return(invisible())
        }
    }
    else search <- FALSE
    if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == 
        ":::")) {
        package <- as.character(topicExpr[[2L]])
        topicExpr <- topicExpr[[3L]]
    }
    else package <- NULL
    if (search) {
        if (is.null(type)) 
            file=(eval(substitute(help.search(TOPIC, package = PACKAGE), 
                list(TOPIC = as.character(topicExpr), PACKAGE = package))))
        else file=(eval(substitute(help.search(TOPIC, fields = FIELD, 
            package = PACKAGE), list(TOPIC = as.character(topicExpr), 
            FIELD = as.character(type), PACKAGE = package))))
    }
    else {
        if (is.null(type)) {
            if (is.call(topicExpr)) 
                file=(.helpForCall(topicExpr, parent.frame()))
            if (is.name(topicExpr)) 
                topic <- as.character(topicExpr)
            else topic <- e1
            file=(eval(substitute(help(TOPIC, package = PACKAGE), 
                list(TOPIC = topic, PACKAGE = package))))
        }
        else {
            if (is.name(type)) 
                type <- as.character(type)
            else type <- e1
            if (is.name(topicExpr)) 
                topic <- as.character(topicExpr)
            else {
                if (is.call(topicExpr) && identical(type, "method")) 
                  file=(.helpForCall(topicExpr, parent.frame(), 
                    FALSE))
                topic <- e2
            }
            doHelp <- .tryHelp(topicName(type, topic), package = package)
            if (inherits(doHelp, "try-error")) {
                if (is.language(topicExpr)) 
                  topicExpr <- deparse(topicExpr)
                stop(gettextf("no documentation of type '%s' and topic '%s' (or error in processing help)", 
                  type, topicExpr), domain = NA)
            }
        }
    }
}
  
 # file = help(topic )
  page = t.gethelpfile( file )
  filename = tools::Rd2HTML(page,out=tempfile() )
  t.cat.in2out (filename, 
		before=paste(DATA_BEGIN,"html: ",sep=""), 
		after = paste("\n",DATA_END,"\n",sep=""),fix.html.bugs=T)
  unlink(filename)
}



translator_texmacs=function (x, size = c("normalsize", "tiny", "scriptsize", "footnotesize", 
"small", "large", "Large", "LARGE", "huge", "Huge")) 
{
    size <- match.arg(size)
    s <- function(rx, rep) {
        x <<- gsub(rx, rep, x, fixed = TRUE)
    }
    wrap <- function(x) {
        sprintf("%s%s%s", paste(rep("@", 3), collapse = ""), 
            x, paste(rep("@", 3), collapse = ""))
    }
    s("\\",wrap("bs"))
 

    s("<", wrap("lt"))
    s(">",wrap("gt"))

    s(wrap("lt"),"\\<less\\>")
    s(wrap("gt"),"\\<gtr\\>")
    s(wrap("bs"),"\\\\")
    x
}

t.nice=function(x) {
  cat(t.highlight(x),"\n",sep="")
  }


t.highlight=function(x) {
  source <- attr( x, "source" )
  if( is.null(source ) ){
    source <- deparse( x )
  }
  require(highlight)
  highlight( parser.output = parser( text = source ),
            renderer = renderer(document=F,
              translator=translator_texmacs,
              formatter=formatter_texmacs,
              space=function() "\\ ",
              newline=function() "\n\n",
#              header=function()"\002texmacs:",
#              footer=function() "\n\005\n"),
              header=function()"",
              footer=function() ""),
            output="/dev/null" )
}

formatter_texmacs = function (tokens, styles, ...) 
{
  co=function(x) paste("<with|color|",x,"|%s>",sep="")

  
  trans=c(
    number="#11D",
    functioncall="#209",
    string="#500",
    argument="#080",
    comment="brown",
    roxycomment="#09f",
    formalargs="#1A1",
    eqformalargs="#1a1",
    assignement="#447",
    package="#9b3",
    prompt="red",
    keyword="#006",
    nothing="black"
    )
  trans[]=co(trans)
  trans["symbol"]="%s"
#  trans["keyword"]=sprintf("<strong|%s>",trans["keyword"])
  trans["slot"]="<em|%s>"
  i=is.na(trans[styles])
  styles[i] = "nothing"

  sprintf(trans[styles],tokens)
}

