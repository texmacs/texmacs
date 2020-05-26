
###############################################################################
##
## MODULE      : with_mode_math.m
## DESCRIPTION : with mode math
## COPYRIGHT   : (C) 2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= with_mode_math (content, math_display= false)
  if (math_display) 
    ret= sprintf("(with \"mode\" \"math\" \"math-display\" \"true\" %s)", content);
  else
    ret= sprintf("(with \"mode\" \"math\" %s)", content);
  endif
endfunction

