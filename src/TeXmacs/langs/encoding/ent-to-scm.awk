#! /usr/bin/awk -f

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
