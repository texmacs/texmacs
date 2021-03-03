
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
    line = input ("", "s");
    code = line;
    if (index(line, char(16)) == 1)
      flush_scheme (complete (parse_complete (substr (line, 2))));
      continue
    else
      line = input ("", "s");
      while (!strcmp (line, "<EOF>"))
        code = [code, "\n", line];
        line = input ("", "s");
      endwhile
    endif
  
    if code(length (code)) != ";"
      disp_ans= true;
    else
      disp_ans= false;
    endif

    trimed_r= strtrim (code);
    if isvarname (trimed_r) && exist (trimed_r)
      code= sprintf ("ans= %s;", code);
    else
      # Reset ans to empty string
      ans= "";
      # Suppress the output
      code= sprintf ("%s;", code);
    endif

    # NOTE: the evaled code will use the polluted env
    eval (code, "tmlasterr");

    if disp_ans
      if (isplot (cmds_for_plot, code))
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

    flush_prompt (PS1 ());

    # Debugging Hints:
    # fid= fopen ("/tmp/octave.log", "a");
    # fprintf (fid, "command: %s\n", r);
    # fclose (fid);
  endwhile
endfunction
