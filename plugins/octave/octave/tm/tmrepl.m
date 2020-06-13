
###############################################################################
##
## MODULE      : tmrepl.m
## DESCRIPTION : REPL loop
## COPYRIGHT   : (C) 2004-2010  Joris van der Hoeven
##               (C) 2014       François Poulain
##               (C) 2020       Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function tmrepl()
  while (true)
    r= input ("", "s");

    if r(length (r)) != ";"
      disp_ans= true;
    else
      disp_ans= false;
    endif

    trimed_r= strtrim (r);
    if isvarname (trimed_r) && exist (trimed_r)
      r= sprintf ("ans= %s;", r);
    else
      # if ans is not changed, do not display it
      # otherwise, display it
      ans= "";
      r= sprintf ("%s;", r);
    endif

    eval (r, "tmlasterr");

    if (get (0, "currentfigure"))   ##  if there is a figure in octave
      plotted= tmplot ();	## call TeXmacs plotting interface
      if plotted && disp_ans
        disp_ans= false;
      endif
    endif 

    if disp_ans && isnewans (ans)
      tmdisp (ans);
    else
      flush_verbatim ("\n");
    endif
  endwhile
endfunction
