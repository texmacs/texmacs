//*****************************************************************************
// MODULE     : rm_from_insert_menu.sci
// DESCRIPTION: Provide Scilab routines to unpopulates the Scilab session menu.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************


function rm_from_insert_menu (nam)
  // Unpopulates the Scilab session menu named bu nam.
  //
  // Parameters
  // nam : string, title of the submenu.
  //
  // Description
  // The function rm_from_insert_menu unpopulates the Scilab session menu .
  //
  // Examples
  // add_to_insert_menu ("Sparselib", libraryinfo ("sparselib"));
  // // Now the menu Scilab -> Insert contains an entry "sparselib";
  // rm_from_insert_menu ("sparselib");
  //
  // Authors
  // François Poulain

  if argn(2) ~= 1 then
    error (msprintf (gettext ('%s: Wrong number of input argument(s): %d'+..
                              'expected.\n'), 'rm_from_insert_menu', 1));
  elseif type (nam) <> 10 then
    error (msprintf (gettext ('%s: Wrong type for input argument #%d: A '+..
                              'string expected.\n'), 'rm_from_insert_menu', 1));
  elseif size (nam, '*') <> 1 then
    error (msprintf (gettext ('%s: Wrong size for input argument #%d: A '+..
                              'scalar expected.\n'), 'rm_from_insert_menu', 1));
  end
  cmd= makeStreeNode ("scilab-rm-from-insert-menu", [nam]);
  tmcmd (cmd);
endfunction
