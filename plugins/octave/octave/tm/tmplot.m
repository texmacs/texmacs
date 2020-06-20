
###############################################################################
##
## MODULE      : tmplot.m
## DESCRIPTION : TeXmacs plotting interface
##               v 0.5 Aug 2018
##               v 0.6 Nov 2019 - added Windows compatibility, added plot
##                                windows visibility, switched form sleep
##                                (obsolete) to pause
## COPYRIGHT   : (C) 2003       Michael Graffam mikegraffam@yahoo.com
##                   2018-2019  Lorenzo Lunelli
##                   2020       Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function ret= tmplot ()
  ret= false;

  # Set the platform dependent $TEXMACS_OCTAVE_TMP
  if (ispc ())
    TEXMACS_OCTAVE_TMP= [getenv("TEMP"), filesep, "tmplot.eps"];
  else
    TEXMACS_OCTAVE_TMP= ["/tmp", filesep, "tmplot.eps"];
  endif

  global TEXMACS_OCTAVE_PLUGIN_CONTROL;

  h= gcf ();

  # Set the visibility of the plotting
  if (bitget (TEXMACS_OCTAVE_PLUGIN_CONTROL, 2) == 0)   # check bit 2 state (figure visibility off)
    set (h, "visible", "off");
  endif
  if (bitget (TEXMACS_OCTAVE_PLUGIN_CONTROL, 2) == 1)   # check bit 2 state (figure visibility on)
    set (h, "visible", "on");
  endif


  if (sizeof (get (h, "currentaxes")) != 0)  # there is a figure with a plot
    if (bitget (TEXMACS_OCTAVE_PLUGIN_CONTROL, 1) == 1)   # check bit 1 state (plotting mode): interactive mode, only show the figure
      pause (0.1);	 # this seems to be necessary on Linux
      refresh (h);
      refresh (h); # Cygwin: this seems to be necessary to update the image (the same behavior happens in octave)
    else
      if (exist (TEXMACS_OCTAVE_TMP) == 2)
        delete (TEXMACS_OCTAVE_TMP);
      endif
      print (h, TEXMACS_OCTAVE_TMP, "-deps", "-color");
      flush_file (TEXMACS_OCTAVE_TMP);
      ret= true;
    endif
  endif
endfunction
