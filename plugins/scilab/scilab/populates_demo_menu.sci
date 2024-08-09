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

lines (0)

///////////////////////// HACK - NEEDED FOR SCILAB -NWNI
if ~%gui then
  function t= usecanvas()
    t= %t;
  endfunction
end

global demolist
if demolist == [] then
  demolist= ..
  [
    "Introduction",SCI+"/modules/core/demos/intro/dem01.dem"; ..
    "Simulation",..
    SCI+"/modules/differential_equations/demos/simulation.dem.gateway.sce"; ..
    "Graphics",SCI+"/modules/graphics/demos/graphics.dem.gateway.sce"; ..
    "CACSD",SCI+"/modules/cacsd/demos/cacsd.dem.gateway.sce"; ..
    "GUI",SCI+"/modules/gui/demos/gui.dem.gateway.sce"; ..
    "Dynamic link",..
    SCI+"/modules/dynamic_link/demos/dynamic_link.dem.gateway.sce"; ..
    "Optimization and Simulation",..
    SCI+"/modules/optimization/demos/optimization.dem.gateway.sce"; ..
    "Polynomials",SCI+"/modules/polynomials/demos/polynomials.dem.gateway.sce"; ..
    "Signal Processing",..
    SCI+"/modules/signal_processing/demos/signal.dem.gateway.sce"; ..
    "Tcl/Tk",SCI+"/modules/tclsci/demos/tk/tcltk.dem.gateway.sce"; ..
    "Sound file handling",SCI+"/modules/sound/demos/sound.dem.sce"; ..
    "Random",SCI+"/modules/randlib/demos/random.dem.gateway.sce"; ..
    "Spreadsheet",SCI+"/modules/spreadsheet/demos/spreadsheet.dem.sce"; ..
    "PVM parallel Toolbox",SCI+"/modules/pvm/demos/pvm.dem"; ..
    "Xcos",SCI+"/modules/xcos/demos/xcos.dem.gateway.sce"
]
end
///////////////////////// END

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

  function str= expand_paths (path)
    if isfile (path) then
      exec(path, -1);
      str= demolist2stree (subdemolist);
    else
      str= '';
    end
  endfunction

  function str= demolist2stree (lst)
    titles= lst (:, 1);
    paths= lst (:, 2);
    str= emptystr (titles);
    for i= 1:size(paths, 'r') do
        if grep (paths(i), "dem.gateway.sce") == 1 then
          path= expand_paths (paths(i));
        else
          path= paths(i);
        end
        str(i)= makeStreeNode ("list", [titles(i) path']);
    end
  endfunction

  // Scilab demos are stored in the demolist variable
  global ('demolist');

  for i= 1:size(demolist, 'r') do
    menu= demolist2stree (demolist(i,:));
    menu= strsubst (menu, SCI, "SCI/");
    if size (menu, '*') > 1 then
      menu= makeStreeNode ("list", menu);
    end
    cmd= makeStreeNode ("scilab-add-to-demo-menu", menu);
    tmcmd (cmd);
  end
endfunction
