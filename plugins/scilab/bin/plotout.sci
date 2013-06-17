//*****************************************************************************
// MODULE     : plotout.sci
// DESCRIPTION: Provide Scilab routine to send Scilab figure as a link to
//              Postscript picture.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************

function plotout ()
  global count
  if count == [] then count = 0, end

  count= count + 1;
  win= get ('current_figure');
  win= win.figure_id;
  filename= fullfile (SCIHOME, 'figure_' + string (win) + '.' + ..
                               string (count) + '.ps');

  xs2ps (win, filename);
  ps= mgetl (filename);
  ps= strsubst (ps, ascii (27), ascii (27) + ascii (27));
  ps= strsubst (ps, ascii (2), ascii (27) + ascii (2));
  ps= strsubst (ps, ascii (5), ascii (27) + ascii (5));
  tmsend ("ps: " + strcat (ps, ascii(10)));
endfunction
