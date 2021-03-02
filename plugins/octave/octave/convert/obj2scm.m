
###############################################################################
##
## MODULE      : obj2scm.m
## DESCRIPTION : Convert an Octave object to a Scheme expression
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##                   2004  Joris van der Hoeven
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= obj2scm (n,c)
  if (nargin<2)
  	c= 0;
  endif
  switch (typeinfo (n))
    case {"range", "matrix", "complex matrix", "bool matrix", "char matrix", "diagonal matrix"}
      ret= mat2scm (n);
    case {"int32 scalar", "int64 scalar", "complex scalar", "scalar", "bool"}
      ret= num2scm (n);
    case {"scalar struct", "struct"}
      ret= struct2scm (n, c+1, with_mode_math ("(big \"triangleup\")"));
    case ("list")
      ret= list2scm (n, c);
    case ("cell")
      ret= cell2scm (n);
    case {"sq_string", "string"}
      [r, c]= size(n);
      if (r == 1)
        ret = "";
      else
        ret= str2scm (n);
      endif
    otherwise
      ret= "";
  endswitch
endfunction
