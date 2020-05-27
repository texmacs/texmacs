
###############################################################################
##
## MODULE      : mat2scm.m
## DESCRIPTION : Convert a matrix to a Scheme expression
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##                   2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= mat2scm (M)
  ret= "(matrix (tformat (table";
  [r,c]= size(M);
  for i=1:r
    ret= [ret,"(row "];
    for j=1:c
      ret= [ret, "(cell ", num2scm(M(i,j)), ") "];
    endfor
    ret= [ret,") "];
  endfor
  ret= [ret,")))"];
  ret= with_mode_math (ret, true);
endfunction
