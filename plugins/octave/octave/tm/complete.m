
###############################################################################
##
## MODULE      : complete.m
## DESCRIPTION : Tab Completion
## COPYRIGHT   : (C) 2021     Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= complete (key)
  lf= lookfor (key);
  ret = ["(tuple ", "\"", key, "\""];
  for cmd=lf
    if length(cmd{1}) > length(key) && starts_with (cmd{1}, key)
      part= substr(cmd{1}, length(key) + 1);
      ret= [ret, " ", "\"", part, "\""];
    endif
  endfor
  ret = [ret, ")"];
endfunction
