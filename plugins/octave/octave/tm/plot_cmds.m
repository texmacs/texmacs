
###############################################################################
##
## MODULE      : plot_cmds.m
## DESCRIPTION : Return a list of plot cmds
## COPYRIGHT   : (C) 2021  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= plot_cmds()
  octave_home= OCTAVE_HOME();
  ind = 0;
  ret = cell();
  for x=strsplit(path(), ":")
    p = x{1};
    if (starts_with(p, octave_home) && 
        (ends_with(p, "m/plot/appearance") ||
         ends_with(p, "m/plot/draw") ||
         ends_with(p, "m/image")))
      files= ls (p);
      [r, c]= size (files);
      for i=1:r
        name = deblank(files(i, :));
        if (ends_with(name, ".m") && !starts_with(name, "__"))
          ind= ind+1;
          ret{ind} = substr(name, 1, length(name) - 2);
        endif
      endfor
    endif
  endfor
endfunction
