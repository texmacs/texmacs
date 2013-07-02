//*****************************************************************************
// MODULE     : populates_demo_menu.sci
// DESCRIPTION: Provide Scilab routines to populates the Scilab demo menu with
//              code demo to be launched.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************


function populates_demo_menu ()
  // Populates the Scilab session menu with code demo to be launched.
  //
  // Descripton
  // The function populates_demo_menu () populates the Scilab demo menu with
  // code demo to be launched.
  //
  // Examples
  // add_to_demo_menu ();
  //
  // See also
  // add_demo
  //
  // Authors
  // François Poulain

  if argn(2) ~= 0 then
    error (msprintf (gettext ('%s: Wrong number of input argument(s): %d'+..
                              'expected.\n'), 'populates_demo_menu', 0));
  end

  // Scilab demos are stored in the demolist variable
  global ('demolist');

function str= expand_paths (path)
    exec(path, -1);
    str= demolist2stree (subdemolist);
  endfunction

  function str= demolist2stree (lst)
    titles= lst (:, 1);
    paths= lst (:, 2);
    str= emptystr (titles);
    for i= 1:size(paths, 'r') do
        //if grep (paths(i), "dem.gateway.sce") == 1 & titles(i) <> 'Simulation' & titles(i) <> "ODE''S" then
        if grep (paths(i), "dem.gateway.sce") == 1 then
          path= expand_paths (paths(i));
        else
          path= paths(i);
        end
        disp([titles(i) path'])
        str(i)= makeStreeNode ("list", [titles(i) path']);
        disp("got:")
        disp(str(i))
    end
  endfunction

  for i= 1:size(demolist, 'r') do
    disp ("list:")
    disp (demolist(i,:))
    menu= demolist2stree (demolist(i,:));
    disp ("menu:")
    disp (menu)
    if size (menu, '*') > 1 then
      menu= makeStreeNode ("list", menu);
      disp("got:")
      disp(menu)
    end
    cmd= makeStreeNode ("scilab-add-to-demo-menu", menu);
    disp("got:")
    disp(cmd)
    tmcmd (cmd);
  end
endfunction
