#! /usr/bin/awk -f

# (C) 2002-2003  David Allouche
#
# This software falls under the GNU general public license and comes WITHOUT
# ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details. If
# you don't have this file, write to the Free Software Foundation, Inc., 59
# Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# Use the .scm files produced by ent-to-scm.awk to produce a HTML
# document containing a table of entities. Useful for debugging and
# knowing which unicode-tmuniversal pairs are still to be defined.

BEGIN { print "<HTML><BODY><TABLE>" ; }
END { print "</TABLE></BODY></HTML>" ; }

match ($0, "\\(\"([^\"]*)", a) {
  printf "<TR><TD>%s<TD>&%s;\n", a[1], a[1] ;
}
