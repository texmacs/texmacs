
###############################################################################
##
## MODULE      : flush_scheme.m
## DESCRIPTION : flush scheme content to stdout
## COPYRIGHT   : (C) 2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function flush_scheme (scheme)
  flush_any (["scheme:", scheme]);
endfunction
