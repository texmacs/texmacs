
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
datarootdir = ${prefix}/share
datadir = ${datarootdir}
mandir = ${datarootdir}/man
tmdir = TeXmacs
tmtgz = TeXmacs-1.0.6.15
tmrpm = TeXmacs-1.0.6.15-1
tmorig = /home/vdhoeven/texmacs/src
tmsrc = /home/vdhoeven/texmacs/src/TeXmacs
tmbin = ${exec_prefix}/libexec/TeXmacs
tmdata = ${datarootdir}/TeXmacs
so = so
os = gnu-linux

MKDIR = mkdir -p
RM = rm -f
CP = cp -r -f
MV = mv -f
LN = ln -f
CHMOD = chmod -f
GZIP = gzip -f
STRIP = true
TOUCH = touch
TAR = tar
