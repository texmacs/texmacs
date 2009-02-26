
###############################################################################
# MODULE     : makefile which can be included in administrative makefiles
# COPYRIGHT  : (C) 1999-2008  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
tmtgz = TeXmacs-1.0.7.2
tmrpm = TeXmacs-1.0.7.2-1
tmorig = /Users/vdhoeven/texmacs/src
tmsrc = /Users/vdhoeven/texmacs/src/TeXmacs
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
STRIP = strip
TOUCH = touch
TAR = tar
