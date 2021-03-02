
###############################################################################
##
## MODULE      : ends_with.m
## DESCRIPTION : Test if a string ends with
## COPYRIGHT   : (C) 2021  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= ends_with (str, x)
  ret= strncmp(substr(str, -length(x)), x, length(x));
endfunction
