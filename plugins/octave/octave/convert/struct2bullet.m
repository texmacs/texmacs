## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert an Octave structure to a Scheme bulleted list
## Created: Sept 2002

function tmp= struct2bullet (n,c)
  
  global TMCOLORS;
  global TMCOLIDX;
  
  # Constrain c to 1:TMCOLIDX, and insure it is defined
  if ((nargin<2) || (c>TMCOLIDX))
    c=1;
  endif
  
  color= deblank (TMCOLORS(c,:)); ## Pick a color, any color
  
  ## Use arrows first then we itemize with bullets
  if (c==1)
    tmp= "(itemize-arrow (document ";
  else
    tmp= "(itemize (document ";
  endif
  
  
  for [val, key]= n
    switch (typeinfo (val))
      case {"struct", "scalar struct"}
  	    ## Itemize the new struct, and switch the key color
  	    tmp1= ["(concat (item) ", with_color (color, dquote ([key, " = "])), " ", struct2bullet (val, c+1), ")"];
      otherwise
  	    tmp1= ["(concat (item) ", with_color (color, dquote ([key, " = "])), " ", obj2scm (val, c), ")"];
    endswitch
    tmp= [tmp, tmp1];
  endfor
  tmp= [tmp, "))"];
endfunction
