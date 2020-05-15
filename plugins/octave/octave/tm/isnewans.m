
###############################################################################
##
## MODULE      : isnewans.m
## DESCRIPTION : Determines if we should display an answer
## COPYRIGHT   : (C) 2004     Joris van der Hoeven
##               (C) 2014     François Poulain
##               (C) 2020     Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function L= isnewans (A)
  # typeinfo (A) is not string/sq_string/dq_string
  if !strcmp(typeinfo (A), "string") && ...
     !strcmp(typeinfo (A), "sq_string") && ...
     !strcmp(typeinfo (A), "dq_string")
    L= 1;
  # A is not an empty string
  elseif !strcmp(A, "")
    L= 1;
  else
    L= 0;
  endif
endfunction
