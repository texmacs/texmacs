### Name: view
### Title: Insert current graphic into TeXmacs
### Aliases: v plotv pointsv linev
### Keywords: TeXmacs graphics view device plot

### ** Examples

x<-(0:600)/100;
plot(x,sin(x),type="l");
lines(x,cos(x),col=2);
legend(0,-0.5,c("sin(x)","cos(x)"),lty=1,col=c(1:2));
v();
plotv(x,sin(60/x),type="l");



