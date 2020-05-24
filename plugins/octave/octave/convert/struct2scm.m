## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert an Octave structure to Scheme
## Created: Sept 2002

function ret= struct2scm(n,a,b)

  global TMSTRUCT;

  if (TMSTRUCT) # Bullets!
    if ((nargin<2)||(a<6))
      a= 1; 
    endif
    ret= struct2bullet (n,a);
  else # Tree
    if (nargin<3)
      b= "name";
    endif
    ret= struct2tree (n,b);
  endif
endfunction
