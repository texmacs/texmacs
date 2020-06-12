
fsep=filesep();
relative_path= ["plugins",fsep,"octave",fsep,"octave",fsep];
prefix= [getenv("TEXMACS_HOME_PATH"),fsep,relative_path];
if (exist (prefix) == 7)
else
  prefix= [getenv("TEXMACS_PATH"),fsep,relative_path];
endif

if (length(prefix) > 0)
  # Add the following directories to Octave's ``load path'':
  #   - ${TEXMACS_PATH}/plugins/octave/octave/tm
  #   - ${TEXMACS_PATH}/plugins/octave/octave/plot
  #   - ${TEXMACS_PATH}/plugins/octave/octave/polynomial
  #   - ${TEXMCSS_PATH}/plugins/octave/octave/convert
  addpath ([prefix,"protocol"], "-end");
  addpath ([prefix,"convert"], "-end");
  addpath ([prefix,"tm"], "-end");
  addpath ([prefix,"polynomial"], "-end");
  # NB: filesep() is used above for portability.

  # Define some global variables.
  global TMSTRUCT=0;
  global TMCOLORS=["black"; "red"; "magenta"; "orange"; "green"; "blue";];
  global TMCOLIDX=rows(TMCOLORS);

  # choice of graphics toolkit
  # ========== 1) check if there is a file with a default choice (Lorenzo added 7-9-2019)
  global TEXMACS_OCTAVE_PLUGIN_CONTROL= 0;
  global def_gtkit_file=[getenv("HOME"),fsep,"._octaveXdef_gtkit"];
  defgtkfalse= 1;
  if (fileattrib (def_gtkit_file)) # there is a file with default graphics toolkit
    fir= fopen (def_gtkit_file);
    def_gtkit= fgetl(fir);
    fclose (fir);
    if (any (strcmpi (def_gtkit,available_graphics_toolkits))) # if the required graphics toolkit is available
      graphics_toolkit (def_gtkit); ## then use it
      disp (["-- TeXmacs OctaveX plugin: default graphics toolkit selected [",def_gtkit,"]"]);
      defgtkfalse= 0;
    else
      disp(["-- TeXmacs OctaveX plugin: ERROR default graphics toolkit [",def_gtkit,"] not available"]);
    endif
  endif		
  # ========== 2) no default gtoolkit, try to select 'qt' (Lorenzo added 1-9-2019)
  if (defgtkfalse)
    if (any (strcmp ("qt", available_graphics_toolkits)))  
      graphics_toolkit ("qt"); ## then use it
    else
      disp ("-- TeXmacs OctaveX plugin: Warning, qt graphics toolkit not available");
    endif
  endif
  # 3) ========== if gtoolkit is 'qt', set some variables (Lorenzo added 1-9-2019)
  if (strcmp("qt",graphics_toolkit))
    TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,0); # set bit 2 to 0: figures are hidden
    h=gcf;
    set (h,"visible","off");
  endif
  disp(["-- using ",graphics_toolkit," graphics toolkit"]);
  disp("-- tmmod(\'n\'|\'i\') to change plotting mode");
  disp("-- tmdisp(a) to display variable 'a'");
  set(0,"defaultaxesfontsize",18);
  set(0,"defaultlinelinewidth",1.5);
endif

tmrepl
