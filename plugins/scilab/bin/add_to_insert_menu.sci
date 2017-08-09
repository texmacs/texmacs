//*****************************************************************************
// MODULE     : add_to_insert_menu.sci
// DESCRIPTION: Provide Scilab routines to populates the Scilab session menu
//              with code snippets to be inserted.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************


function add_to_insert_menu (nam, lst)
  // Populates the Scilab session menu with code snippets to be inserted.
  //
  // Parameters
  // nam : string, title of the submenu.
  // lst : string column vector: the text of submenu entries.
  //
  // Description
  // The function add_to_insert_menu populates the Scilab session menu with
  // code snippets to be inserted.
  //
  // Examples
  // add_to_insert_menu ("Sparselib", libraryinfo ("sparselib"));
  // // Now the menu Scilab -> Insert contains an entry "sparselib";
  //
  // Authors
  // François Poulain

  if argn(2) ~= 2 then
    error (msprintf (gettext ('%s: Wrong number of input argument(s): %d'+..
                              'expected.\n'), 'add_to_insert_menu', 2));
  elseif type (nam) <> 10 then
    error (msprintf (gettext ('%s: Wrong type for input argument #%d: A '+..
                              'string expected.\n'), 'add_to_insert_menu', 1));
  elseif type (lst) <> 10 then
    error (msprintf (gettext ('%s: Wrong type for input argument #%d: A '+..
                              'string expected.\n'), 'add_to_insert_menu', 2));
  elseif size (nam, '*') <> 1 then
    error (msprintf (gettext ('%s: Wrong size for input argument #%d: A '+..
                              'scalar expected.\n'), 'add_to_insert_menu', 1));
  end
  cmd= makeStreeNode ("scilab-add-to-insert-menu",..
                      [nam (makeStreeNode ("list", lst))]);
  tmcmd (cmd);
endfunction
