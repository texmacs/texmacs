#! /usr/bin/awk -f

# Use the .scm files produced by ent-to-scm.awk to produce a HTML
# document containing a table of entities. Useful for debugging and
# knowing which unicode-tmuniversal pairs are still to be defined.

BEGIN { print "<HTML><BODY><TABLE>" ; }
END { print "</TABLE></BODY></HTML>" ; }

match ($0, "\\(\"([^\"]*)", a) {
  printf "<TR><TD>%s<TD>&%s;\n", a[1], a[1] ;
}
