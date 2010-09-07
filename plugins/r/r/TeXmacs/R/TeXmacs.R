TeXmacsR.version="0.11"
cat("TeXmacs to R interface version ",TeXmacsR.version,"\n") 

.packageName <- "TeXmacs"
.First.lib<-function(dir,package)
  {
    cat("TeXmacs to R interface version ",TeXmacsR.version) 
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


t.cat.in2out<-function (filename, before="\002ps:width=0.75par\nheight=0.75par\n", after = "\n\005\n",fix.html.bugs=F)
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
            cat(a, sep = "\n")
    }
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
#    cat("\2ps:width=0.75par\nheight=0.75par\n") ;
#    system(paste("cat",op$file))
#    cat("\5\n") 
    try(    t.cat.in2out( op$file ), silent=T )
    postscript(file=op$file,one=F,horiz=F,width=width,height=height,...)
  } else {
    x<-recordPlot()
    op$file<-tempfile("postscript")
    postscript(command="echo",file=op$file,one=F,print.it=F,horiz=F,width=width,height=height,...) 
    replayPlot(x)
    dev.off()
    try(    t.cat.in2out( op$file ), silent=T )
    unlink(op$file)
  }
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



t.tab.comp<-function(s,curs)
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
  deb.ret <<- ret
  deb.libs <<- libs
  names(ret) = libs[,1]
  names(ret)[ names(ret) %in% .packages() ] = paste("*",names(ret[ names(ret) %in% .packages() ] ),sep="")
  deb.ret <<- ret
  
  ret
}

t.update.menus = function(max.len=30) {
  cat( t.send.menu( t.make.menu(), max.len=max.len ) )
}

t.issue = function( ... ) {
      paste( "\2command:(",...,")\5\n",sep=" ",collapse="")
    }

#t.file.show = function( 

t.browseURL = function(url,help=F) {
  Qurl = paste("\"",url,"\"",sep="")
  if( help ) {
    cat(t.issue( "load-help-buffer", Qurl ))
  } else {
    cat(t.issue( "load-buffer", Qurl ))
  }
}
  

t.help = function( ... ) {
  if( as.numeric( version$minor ) >= 11 ) {
    h = help(...,help_type="html")  
  } else {
    h = help(...,htmlhelp=T)
  }

  t.browseURL(h,help=T)
}

t.start.help = function() {
  assign("?",t.help.quest.old,envir = .GlobalEnv)
      options(browser=t.browseURL)
    options(help_type="html")
    }

# Doesn't currently work. Also seems to be a moving target,
# because R keeps changing how things are done....
#
#assign("?",t.help)

if( as.numeric( version$minor ) >= 11 ) {
  t.gethelpfile = utils:::.getHelpFile
} else if( as.numeric( version$minor ) >= 10 ) {
  t.gethelpfile = 
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
    }
}


t.display.html.help=function( e1, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
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
  t.cat.in2out (filename, before="\002html: ", after = "\n\005\n",fix.html.bugs=T)
  unlink(filename)
}

t.help.quest.old = get("?")
assign("?",t.display.html.help)


