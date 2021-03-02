
###############################################################################
##
## MODULE      : isplot.m
## DESCRIPTION : Determines if we should plot
## COPYRIGHT   : (C) 2020     Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= isplot (cmds, line)
  trimmed_line = strtrim(line);
  [r, c]= size(cmds);
  for i=1:c
    if (strncmpi (trimmed_line, cmds{i}, length (cmds{i})))
      ret= true;
      return
    else
      ret= false;
    endif
  endfor
endfunction
