
###############################################################################
##
## MODULE      : tmdisp.m
## DESCRIPTION : Displays the Octave object via the TeXmacs interface
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##                   2004  Joris van der Hoeven
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function tmdisp (M)
  scheme_to_flush= obj2scm (M);
  if length (scheme_to_flush) == 0
    flush_verbatim (M);
  else
    flush_scheme (scheme_to_flush);
  endif
endfunction
