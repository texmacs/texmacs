
###############################################################################
##
## MODULE      : unquote.m
## DESCRIPTION : unquote a string 
## COPYRIGHT   : (C) 2021  Darcy Shen 
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= unquote (content)
  if length(content) > 2 && starts_with (content, "\"") && ends_with (content, "\"")
    ret = substr(content, 2, length(content)-2);
  else
    ret = content;
  endif
endfunction
