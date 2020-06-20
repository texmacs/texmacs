
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
    __r= input ("", "s");

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
      global TM_OCTAVE_PLOT_DIGEST;
      
      updated_digest= hash ("md5", serialize (get(gcf())));
      if !strcmp (TM_OCTAVE_PLOT_DIGEST, updated_digest)
        TM_OCTAVE_PLOT_DIGEST= updated_digest;
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
