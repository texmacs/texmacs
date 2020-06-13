
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

if (length (prefix) > 0)
  # Add the following directories to Octave's ``load path'':
  #   - ${TEXMACS_PATH}/plugins/octave/octave/protocol
  #   - ${TEXMCSS_PATH}/plugins/octave/octave/convert
  #   - ${TEXMACS_PATH}/plugins/octave/octave/tm
  #   - ${TEXMACS_PATH}/plugins/octave/octave/polynomial
  addpath ([prefix, "protocol"], "-end");
  addpath ([prefix, "convert"], "-end");
  addpath ([prefix, "tm"], "-end");
  addpath ([prefix, "polynomial"], "-end");

  flush_verbatim (["GNU Octave (", version, ") Session in GNU TeXmacs\n"])
  flush_verbatim ("Welcome to star and fork it at https://github.com/texmacs/octave\n")
  flush_prompt ("octave> ")

  # Define some global variables.
  global TMSTRUCT= 0;
  global TMCOLORS= ["black"; "red"; "magenta"; "orange"; "green"; "blue";];
  global TMCOLIDX= rows (TMCOLORS);
  global TEXMACS_OCTAVE_PLUGIN_CONTROL= 0;

  # choice of graphics toolkit
  # ========== 1) check if there is a file with a default choice (Lorenzo added 7-9-2019)
  global def_gtkit_file=[getenv("HOME"), filesep, "._octaveXdef_gtkit"];
  defgtkfalse= 1;
  if (fileattrib (def_gtkit_file)) # there is a file with default graphics toolkit
    fir= fopen (def_gtkit_file);
    def_gtkit= fgetl(fir);
    fclose (fir);
    if (any (strcmpi (def_gtkit,available_graphics_toolkits))) # if the required graphics toolkit is available
      graphics_toolkit (def_gtkit); ## then use it
      flush_verbatim (["-- TeXmacs OctaveX plugin: default graphics toolkit selected [",def_gtkit,"]"]);
      defgtkfalse= 0;
    else
      flush_verbatim (["-- TeXmacs OctaveX plugin: ERROR default graphics toolkit [",def_gtkit,"] not available"]);
    endif
  endif		
  # ========== 2) no default gtoolkit, try to select 'qt' (Lorenzo added 1-9-2019)
  if (defgtkfalse)
    if (any (strcmp ("qt", available_graphics_toolkits)))  
      graphics_toolkit ("qt"); ## then use it
    else
      flush_verbatim ("-- TeXmacs OctaveX plugin: Warning, qt graphics toolkit not available");
    endif
  endif
  # 3) ========== if gtoolkit is 'qt', set some variables (Lorenzo added 1-9-2019)
  if (strcmp ("qt", graphics_toolkit))
    TEXMACS_OCTAVE_PLUGIN_CONTROL= bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,0); # set bit 2 to 0: figures are hidden
    h= gcf ();
    set (h, "visible", "off");
  endif
  set (0, "defaultaxesfontsize", 18);
  set (0, "defaultlinelinewidth", 1.5);
endif

tmrepl ()
