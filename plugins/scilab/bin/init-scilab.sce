//*****************************************************************************
// MODULE     : init-texmacs-atoms.sce
// DESCRIPTION: Overload Scilab display routines in order to return Scheme
//              trees to TeXmacs.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************

texmacs_path= getenv("TEXMACS_PATH");
exec (fullfile (getenv("TEXMACS_PATH"), 'plugins/scilab/bin/tmstree.sci'), -1);

function s= makeStreeNode (l, x)
  scm= "scheme: ";
  if type (l) <> 10 then
    error (msprintf (gettext ("%s: Wrong type of input argument(s): " + ..
                              "%s expected."), "make", "string"));
  end
  if argn (2) == 1 | x == [] then
    s= scm + "(" + l + ")";
  return
  end
  if type (l) <> 10 | type (x) <> 10 then
    error (msprintf (gettext ("%s: Wrong type of input argument(s): " + ..
                              "%s expected."), "make", "string"));
  end
  for i= 1:size (x,'*') do
    is_scheme= strindex (x(i), scm);
    if is_scheme (1) == 1 then
      x(i)= part (x(i), length (scm):length (x(i)));
    else
      x(i)= "'"" + x(i) + "'"";
    end
  end
  s= scm + "(" + l + " " + strcat (x, " ") + ")";
endfunction

funcprot(0)

function tmout(v)
  DATA_BEGIN=ascii(2);
  DATA_END=ascii(5);
  out= tmstree(v);
  out= makeStreeNode ("with", ["mode" "math" out]);
  out= makeStreeNode ("document", [out ""])
  mprintf(DATA_BEGIN + out + DATA_END);
endfunction


char_codes= ["s" "p" "b" "sp" "spb" "msp" "i" "c" "h" "fptr" "mc" "l" "hm" ..
             "ptr" "ce" "st" "r" "lss" "ip"]

for i= 1:size (char_codes, '*') do
  deff('[]=%' + char_codes (i) + '_p(a)', "tmout (a)");
end

funcprot(1)

banner
