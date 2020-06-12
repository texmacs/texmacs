## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
## Aug. 2018 --  TeXmacs plotting interface -- tested with octave 4.2.1, 4.4.0, 4.4.1,5.1.0
## This function set the environment variable that determines the octave plugin behavior (presently, only the interactive use of octave plots in TeXmacs)
## Author: Lorenzo Lunelli
## v 0.5
## Nov. 2019 v 0.6 - added figure window visibiity options

function info = tmmod (choice)
	# 1 Sept 2018:	bit 1 controls the plotting mode (0 normal, 1 interactive)
	# 1 Nov 2019:	bit 2 controls visibility of octave figure during drawing (0 hidden, 1 visible)
	global TEXMACS_OCTAVE_PLUGIN_CONTROL;  # 28-August 2018: now using global variables - 1 Sept 2018: changed variable name
	if (nargin>0)   # change the plugin mode
		if (strcmpi (choice,'i') || choice==1) # ----------------------------- INTERACTIVE MODE -------------------------
			TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,1); # set bit 2 to 1, figures are visible
			TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,1,1); # set bit 1 (the lowest) to 1
			message = "-- TeXmacs OctaveX plugin: set interactive plotting mode";
			message=char(message,"-- in some cases you need to use 'refresh()' to update the octave window");
			message=char(message,"-- use tmfig to load a figure in the document");
			message=char(message,"-- use tmmod('n'|'i')) to change plotting mode");
		elseif (strcmpi (choice,'n') || choice==0) # ----------------------------- NORMAL MODE --------------------------
			TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,1,0); # set bit 1 (the lowest) to 0
			if (any(strcmp("qt",graphics_toolkit())))  # Lorenzo added 1-9-2019, check if qt graphic toolkit is active
				TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,0); # set bit 2 to 0: figures are hidden
			endif
			message="-- TeXmacs OctaveX plugin: set normal plotting mode";
			message=char(message,"-- use tmmod('n'|'i') to change plotting mode");
		elseif (strcmpi (choice,'_v_')) # --------------- figures are visible --------------------------------------------
			TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,1); # set bit 2 to 1
			message="Octave figure visible";
		elseif (strcmpi (choice,'_h_')) # --------------- figures are hidden ---------------------------------------------
			TEXMACS_OCTAVE_PLUGIN_CONTROL=bitset(TEXMACS_OCTAVE_PLUGIN_CONTROL,2,0); # set bit 2 to 0
			message="Octave figure hidden";
		else
			message='please use a valid option';
		endif
	else   # check the plotting mode
		if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,1)==0)   # check bit 1 state (plotting mode)
			message="current plotting mode is normal";
		endif
		if (bitget(TEXMACS_OCTAVE_PLUGIN_CONTROL,1)==1)   # check bit 1 state (plotting mode)
			message="current plotting mode is interactive";
		endif
	endif
	info=message;
endfunction
