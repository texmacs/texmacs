
###############################################################################
##
## MODULE      : with_color.m
## DESCRIPTION : with color
## COPYRIGHT   : (C) 2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= with_color (color, content)
  ret= sprintf('(with "color" "%s" %s)', color, content);
endfunction
