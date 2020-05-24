## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert an Octave list to a Scheme expression
## Created: Sept 2002

function ret= list2scm (L,c)
  if (nargin<2)
    c=1;
  endif	
  ret= "(enumerate-numeric (document";
  len= length(L);
  for i=1:len
    ret= [ret, " (concat (item) ", obj2scm(nth(L,i),c),")"];
  endfor
  ret= [ret,"))"];
endfunction
