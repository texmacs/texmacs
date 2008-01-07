
###############################################################################
# MODULE     : makefile which can be included in administrative makefiles
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

prefix = /usr/local
exec_prefix = ${prefix}
includedir = ${prefix}/include
libdir = ${exec_prefix}/lib
bindir = ${exec_prefix}/bin
datadir = ${prefix}/share
mandir = ${prefix}/man
tmdir = TeXmacs
tmtgz = TeXmacs-1.0.6.12
tmrpm = TeXmacs-1.0.6.12-1
tmorig = /home/vdhoeven/texmacs/src
tmsrc = /home/vdhoeven/texmacs/src/TeXmacs
tmbin = ${exec_prefix}/libexec/TeXmacs
tmdata = ${prefix}/share/TeXmacs
so = so
os = gnu-linux

MKDIR = mkdir -p
RM = rm -f
CP = cp -r -f
MV = mv -f
LN = ln -f
CHMOD = chmod -f
GZIP = gzip -f
STRIP = strip
TOUCH = touch
TAR = tar
