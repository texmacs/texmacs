
/******************************************************************************
* MODULE     : gui.hpp
* DESCRIPTION: Abstract interface to various GUIs.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This module contains all system-wide routines of GUIs.
* An abstract widget interface can be found in widget.hpp / message.hpp and
* an abstract interface for drawing primitives in renderer.hpp.
* The Widkit plug-in provides a default implementation for a widget library.
* When using Widkit, you should provide a simple window implementation,
* as specified in window.hpp.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GUI_H
#define GUI_H
#include "tree.hpp"
#include "bitmap_font.hpp"
#include "tm_timer.hpp"
#include "colors.hpp"

struct font;
class widget;

/******************************************************************************
* Main routines
******************************************************************************/

void gui_open (int& argc, char** argv);
  // start the gui
void gui_interpose (void (*) (void));
  // specify an interpose routine for the main loop
void gui_start_loop ();
  // start the main loop
void gui_close ();
  // cleanly close the gui
void gui_root_extents (SI& width, SI& height);
  // get the screen size
void gui_maximal_extents (SI& width, SI& height);
  // get the maximal size of a window (can be larger than the screen size)
void gui_refresh ();
  // update and redraw all windows (e.g. on change of output language)
string gui_version ();
  // retrieve the type of GUI that is being used

/******************************************************************************
* Font support
******************************************************************************/

void set_default_font (string name);
  // set the name of the default font
font get_default_font (bool tt= false, bool mini= false, bool bold= false);
  // get the default font, depending on desired characteristics:
  // tt for a monospaced font, mini for a smaller font and bold for a bold font
font get_default_styled_font (int style);
  // get the default font for a given style
  // (see widget.hpp for available styles)
  // NOTE: implemented in widget.cpp
void load_system_font (string family, int size, int dpi,
		       font_metric& fnm, font_glyphs& fng);
  // load the metric and glyphs of a system font
  // you are not obliged to provide any system fonts

/******************************************************************************
* Clipboard support
******************************************************************************/

bool set_selection (string cb, tree t,
                    string s, string sv, string sh, string format);
  // Copy a selection 't' of a given 'format' to the clipboard 'cb',
  // where 's' contains the string serialization of t according to the format
  // and possibly the variants 'sv' and 'sh' for verbatim and html
  // Returns true on success
bool get_selection (string cb, tree& t, string& s, string format);
  // Retrieve the selection 't' of a given 'format' from the clipboard 'cb',
  // where 's' is the string serialization of t according to the format
  // Returns true on success; sets t to (extern s) for external selections
void clear_selection (string cb);
  // Clear the selection on clipboard 'cb'

/******************************************************************************
* Miscellaneous
******************************************************************************/

#define INTERRUPT_EVENT   0
#define INTERRUPTED_EVENT 1
#define ANY_EVENT         2
#define DRAG_EVENT        3
#define MOTION_EVENT      4
#define MENU_EVENT        5

void beep ();
  // Issue a beep
void needs_update ();
  // Inform the gui that the editor needs to update itself
  // before repainting can start
bool check_event (int type);
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
void image_gc (string name= "*");
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
array<SI> get_widget_size (widget w);
  // Get size of a widget w
void show_help_balloon (widget balloon, SI x, SI y);
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
void show_wait_indicator (widget base, string message, string argument);
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
void external_event (string type, time_t t);
  // External events, such as pushing a button of a remote infrared commander
bool gui_interrupted (bool check = false);
  // Probe whether external events are waiting to be handled
  // Useful to stop lengthy repainting operations

extern bool use_unified_toolbar;
  // MacOS toolbar style option

#endif // defined GUI_H
