
###############################################################################
##
## MODULE      : struct2tree.m
## DESCRIPTION : Convert an Octave structure to a Scheme tree
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.



function ret= struct2tree (n,name)

  global TMCOLORS;
  global TMCOLIDX;

  # Make sure name is defined 
  if ((nargin<2))
    name= with_mode_math ("(big \"triangleup\")");
  endif
  
  keycolor= deblank (TMCOLORS(1,:));
  valcolor= deblank (TMCOLORS(TMCOLIDX,:));
  colored_name= with_color (keycolor, name);
  
  ret=["(tree ", colored_name, " "];
  for [val, key] = n
    colored_key= with_color (keycolor, dquote (key));
    colored_val= with_color (valcolor, obj2scm(val));
    switch (typeinfo (val))
      case {"struct", "scalar struct"}
        ## Itemize the new struct, and switch the key color
        ret1= struct2tree (val, key);
      otherwise
        ret1= colored_val;
    endswitch
    ret= [ret,ret1];
  endfor
  ret= [ret,")"];
endfunction
