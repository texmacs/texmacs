
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
    case ("range")
      ret= mat2scm (n);
    case ("matrix")
      ret= mat2scm (n);
    case ("complex matrix")
      ret= mat2scm (n);
    case ("bool matrix")
      ret= mat2scm (n);
    case ("char matrix")
      ret= mat2scm (n);
    case ("scalar")
      ret= num2scm (n);
    case ("complex scalar")
      ret= num2scm (n);
    case ("bool")
      ret= num2scm (n);
    case ("struct")
      ret= struct2scm (n,c+1,"(with \"mode\" \"math\" (big \"triangleup\"))");
    case ("string")
      ret= str2scm (n);
    case ("list")
      ret= list2scm (n,c);
    otherwise
      ret= "";
  endswitch
endfunction
