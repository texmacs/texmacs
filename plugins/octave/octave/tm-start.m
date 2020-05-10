
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
    if (exist ("OCTAVE_VERSION") == 5)
        # As of Octave version 3, "instead of setting the built-in variable
        # LOADPATH, you must use addpath [...] to manipulate the function
        # search path." [1]
        addpath ([prefix,"tm"],         "-end");
        addpath ([prefix,"plot"],       "-end");
        addpath ([prefix,"polynomial"], "-end");
        # NB: filesep() is used above for portability.
    else
        LOADPATH=[LOADPATH,                              
                  [prefix,"tm:"],      
                  [prefix,"plot:"],    
                  [prefix,"polynomial:"]];
    endif

    # Customize the command-line prompt.
    if (exist ("OCTAVE_VERSION") == 5)
        # As of Octave version 3, "all built-in variables have been converted
        # to functions." [1]
        PS1("\\002channel:prompt\\005octave> \\005");
        PS2("\\002channel:prompt\\005> \\005");
    else
        PS1="\\002channel:prompt\\005octave> \\005";
        PS2="\\002channel:prompt\\005> \\005";
    endif

    # Define some global variables.
    global TMSTRUCT=0;
    global TMCOLORS=["black"; "red"; "magenta"; "orange"; "green"; "blue";];
    global TMCOLIDX=rows(TMCOLORS);
endif

tmrepl
