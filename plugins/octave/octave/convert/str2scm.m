
###############################################################################
##
## MODULE      : obj2scm.m
## DESCRIPTION : Convert an Octave string to a Scheme string
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##                   2021  Darcy Shen      sadhen@zoho.com
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= str2scm (n)
  [r, c]= size(n);

  ret= "(stack (tformat (table";
  for i=1:r
    ret= [ret,"(row "];
    ret= [ret, "(cell ", dquote(n(i, :)), ") "];
    ret= [ret,") "];
  endfor
  ret= [ret,")))"];
endfunction
