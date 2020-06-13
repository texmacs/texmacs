
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
  if (length (getenv ("TEXMACS_PATH")) > 0)
    tmp= obj2scm (M);
    if strcmp (tmp,"")
      disp (M);
    else
      flush_scheme (tmp)
    endif
  else
    disp (M);
  endif
endfunction
