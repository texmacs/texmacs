
###############################################################################
##
## MODULE      : dquote.m
## DESCRIPTION : dquote a string 
## COPYRIGHT   : (C) 2020  Darcy Shen 
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= dquote (content)
  ret= ["\"", content, "\""];
endfunction
