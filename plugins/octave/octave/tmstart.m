
###############################################################################
##
## MODULE      : tmstart.m
## DESCRIPTION : REPL loop
## COPYRIGHT   : (C) 2014       Miguel de Benito
##               (C) 2018-2019  Lorenzo Lunelli
##               (C) 2020       Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

# prefix=
# - TEXMACS_HOME_PATH/plugins/octave/octave (higher priority)
# - TEXMACS_PATH/plugins/octave/octave
# The octave under TEXMACS_HOME_PATH will override the built-in one

relative_path= ["plugins", filesep, "octave", filesep, "octave", filesep];
prefix= [getenv("TEXMACS_HOME_PATH"), filesep, relative_path];
if (exist (prefix) == 7)
else
  prefix= [getenv("TEXMACS_PATH"), filesep, relative_path];
endif

if (strcmp (PS1(), "octave:\\#> "))
  PS1 (">> ");
endif

if (length (prefix) > 0)
  addpath (genpath (prefix))

  flush_verbatim (["GNU Octave (", version, ") Session in GNU TeXmacs\n"])
  flush_verbatim ("Welcome to star and fork it at https://github.com/texmacs/octave\n")
  flush_prompt (PS1 ())

  # Define some global variables.
  global TMSTRUCT= 0;
  global TMCOLORS= ["black"; "red"; "magenta"; "orange"; "green"; "blue";];
  global TMCOLIDX= rows (TMCOLORS);
  global TEXMACS_OCTAVE_PLUGIN_CONTROL= 0;
  global TM_OCTAVE_PLOT_DIGEST= 0;

  if (any (strcmp ("gnuplot", available_graphics_toolkits)))
    graphics_toolkit ("gnuplot"); ## then use it
  endif
  set (0, "defaultaxesfontsize", 18);
  set (0, "defaultlinelinewidth", 1.5);
endif

# Default to $HOME
cd ("~");

tmrepl ()
