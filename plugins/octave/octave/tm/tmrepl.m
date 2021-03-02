
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
  cmds_for_plot= plot_cmds();
  while (true)
    __r = input ("", "s");
    __r0= input ("", "s");
    while (!strcmp(__r0,"<EOF>"))
      __r = strcat(__r,"\n",__r0);
      __r0= input ("", "s");
    endwhile
    if __r(length (__r)) != ";"
      disp_ans= true;
    else
      disp_ans= false;
    endif

    trimed_r= strtrim (__r);
    if isvarname (trimed_r) && exist (trimed_r)
      __r= sprintf ("ans= %s;", __r);
    else
      # Reset ans to empty string
      ans= "";
      # Suppress the output
      __r= sprintf ("%s;", __r);
    endif

    # NOTE: the evaled code will use the polluted env
    eval (__r, "tmlasterr");

    if disp_ans
      if (isplot (cmds_for_plot, __r))
        plotted= tmplot (); ## call TeXmacs plotting interface
        if plotted
          disp_ans= false;
        endif
      endif
    endif 

    if disp_ans && isnewans (ans)
      tmdisp (ans);
    else
      flush_verbatim ("\n");
    endif

    # Debugging Hints:
    # fid= fopen ("/tmp/octave.log", "a");
    # fprintf (fid, "command: %s\n", r);
    # fclose (fid);
  endwhile
endfunction
