## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert a matrix to a Scheme expression
## Created: Sept 2002

function ret= mat2scm(M)
  global TMCOLORS;
  global TMCOLIDX;
  ret= "(with \"mode\" \"math\" \"math-display\" \"true\" (matrix (tformat (table";
  [r,c]=size(M);
  for i=1:r
    ret= [ret,"(row "];
    for j=1:c
      ret= [ret,"(cell ", num2scm(M(i,j)), ") "];
    endfor
    ret= [ret,") "];
  endfor
  ret= [ret,"))))"];
endfunction
