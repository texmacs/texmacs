
###############################################################################
##
## MODULE      : _tm_oct_.m
## DESCRIPTION : TeXmacs plotting interface
##               v 0.5 Aug 2018
##               v 0.6 Nov 2019 - added Windows compatibility, added plot
##                                windows visibility, switched form sleep
##                                (obsolete) to pause
## COPYRIGHT   : (C) 2003       Michael Graffam mikegraffam@yahoo.com
##                   2018-2019  Lorenzo Lunelli
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function dum = _tm_oct_ ()
  if (ispc())
    TEXMACS_OCTAVE_TMP=[getenv("TEMP") filesep() "tmplot.eps"];
  else
    TEXMACS_OCTAVE_TMP="/tmp/tmplot.eps";
  endif

  global TEXMACS_OCTAVE_PLUGIN_CONTROL;

  h = gcf ();
  if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,2)==0)   # check bit 2 state (figure visibility off)
    set (h,"visible","off");
  endif
  if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,2)==1)   # check bit 2 state (figure visibility on)
    set (h,"visible","on");
  endif
  if (sizeof(get (h,"currentaxes"))!=0)  # there is a figure with a plot
    if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,1)==1)   # check bit 1 state (plotting mode): interactive mode, only show the figure
      pause (0.1);	 # this seems to be necessary on Linux
      refresh (h);
      refresh (h); # Cygwin: this seems to be necessary to update the image (the same behavior happens in octave)
    endif
  	if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,1)==0)   # check bit 1 state 	# normal mode, load the data in TeXmacs
      print(h,TEXMACS_OCTAVE_TMP, "-deps", "-color");
      clf(h);
      if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,2)==1) # if figures are visible
        ##	close(h,"force");   # this function is missing in octave version before 4.2
        ##	close("all","force");
        close(h); # if mode is normal and figures are visible, then close them after printing 
      endif
      P=[2;112;115;58]; #P= "\002ps:" - 002: DATA START
      g=fopen(TEXMACS_OCTAVE_TMP);
      while (g==-1)
        pause(0.2);
        g=fopen(TEXMACS_OCTAVE_TMP);
      endwhile
      f=fread(g);   # Lorenzo - read the whole file
      P=[P;f];
      fclose(g);
      P=[P;5]; # - 005: DATA END
      disp(sprintf("%cverbatim:\n",2));
      disp(char(P'));
      delete(TEXMACS_OCTAVE_TMP);
    endif
  endif
endfunction
