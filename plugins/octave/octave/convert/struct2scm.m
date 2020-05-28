
###############################################################################
##
## MODULE      : struct2scm.m
## DESCRIPTION : Convert an Octave structure to Scheme
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= struct2scm (n,a,b)

  global TMSTRUCT;

  if (TMSTRUCT) # Bullets!
    if ((nargin<2)||(a<6))
      a= 1; 
    endif
    ret= struct2bullet (n, a);
  else # Tree
    if (nargin<3)
      b= "name";
    endif
    ret= struct2tree (n, b);
  endif
endfunction
