
###############################################################################
##
## MODULE      : flush_any.m
## DESCRIPTION : flush any content to stdout
## COPYRIGHT   : (C) 2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function flush_any (output)
  DATA_BEGIN= 2;
  DATA_END= 5;
  surrounded= sprintf ("%c%s%c", DATA_BEGIN, output, DATA_END);
  disp (surrounded);
endfunction
