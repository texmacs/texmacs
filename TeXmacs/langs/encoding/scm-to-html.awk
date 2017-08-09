#! /usr/bin/awk -f

# (C) 2002-2003  David Allouche
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

# Use the .scm files produced by ent-to-scm.awk to produce a HTML
# document containing a table of entities. Useful for debugging and
# knowing which unicode-tmuniversal pairs are still to be defined.

BEGIN { print "<HTML><BODY><TABLE>" ; }
END { print "</TABLE></BODY></HTML>" ; }

match ($0, "\\(\"([^\"]*)", a) {
  printf "<TR><TD>%s<TD>&%s;\n", a[1], a[1] ;
}
