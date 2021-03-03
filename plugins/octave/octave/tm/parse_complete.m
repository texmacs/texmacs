
###############################################################################
##
## MODULE      : parse_complete.m
## DESCRIPTION : Parse Completion Command
## COPYRIGHT   : (C) 2021     Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

# example line: (complete "hel" 3)
function ret= parse_complete (line)
  for x=strsplit(line, " ")
    if starts_with(x{1}, "\"") && ends_with(x{1}, "\"")
      ret= unquote(x{1});
      return
    endif
  endfor
  ret= ""
endfunction

