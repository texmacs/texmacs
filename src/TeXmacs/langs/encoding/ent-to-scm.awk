#! /usr/bin/awk -f

# (C) 2002-2003  David Allouche
#
# This software falls under the GNU general public license and comes WITHOUT
# ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details. If
# you don't have this file, write to the Free Software Foundation, Inc., 59
# Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# Convert normative .ent files used in the HTML4 recommandation to a
# Scheme format suitable for consumption by TeXmacs.

match ($0, "<!ENTITY[[:blank:]]+([[:graph:]]+)[[:blank:]]+CDATA[[:blank:]]+\"([^\"]*)\"[[:blank:]]+--[[:blank:]]+([^,]+)", a) {
  while (1) {
#    if (match ($0, "U\\+([[:xdigit:]]\\{4\\})", b))
    if (match ($0, "U\\+([[:xdigit:]]+)", b))
      hex=b[1] ;
    if (match ($0, "-->$")) break;
    getline ;
  }
  s1= sprintf("(\"%s\"", a[1]) ;
  # s2= sprintf("\"%s\")", a[2]) ; # decimal references
  s2= sprintf("\"&#x%s\")", hex) ; # hexa references
  printf "%-16s%-16s; %s\n", s1, s2, a[3] ;
    
  next ;
}

match ($0, "^[[:blank:]]*<!--[[:blank:]]+(.*)[[:blank:]]+-->[[:blank:]]*$", a)\
{
  print ";;", a[1] ;
  next
}

/^$/ { print ; next ;}

match ($0, "^[[:blank:]]*<!--[[:blank:]]+(.*)[[:blank:]]*$", a) {
  print ";;", a[1] ;
  next ;
}

match ($0, "^[[:blank:]]*(.*)[[:blank:]]*-->[[:blank:]]*$", a) {
  if (length(a[1]) != 0) print ";;", a[1] ;
  next ;
}

match ($0, "^[[:blank:]]*([[:graph:]]+([[:blank:]]+[[:graph:]]+)*)[[:blank:]]*$", a) {
  print ";;", a[1] ;
  next ;
}

{ print ";!!!", $0, "!!!" }
