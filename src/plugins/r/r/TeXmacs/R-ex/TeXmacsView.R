### Name: TeXmacsView
### Title: function to start postscript TeXmacs interface
### Aliases: start.view end.view
### Keywords: TeXmacs view device graphics

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function()
  {
    dev.off()
    op <- options("texmacs")$texmacs
    op$nox11 <- F
    options(texmacs=op)
    unlink(op$file)
  }



