
###############################################################################
##
## MODULE      : obj2scm.m
## DESCRIPTION : Convert an Octave string to a Scheme string
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= str2scm (n)
  global TMCOLORS;
  global TMCOLIDX;
  ret= with_color (deblank (TMCOLORS(TMCOLIDX,:)), dquote (n));
endfunction
