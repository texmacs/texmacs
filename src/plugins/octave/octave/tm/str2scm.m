## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert an Octave string to a Scheme string
## Created: Sept 2002

function tmp=str2scm(n)
	global TMCOLORS;
	global TMCOLIDX;
	tmp=["(with \"color\" \"", deblank(TMCOLORS(TMCOLIDX,:)), "\" \"",n,"\")"];
endfunction
