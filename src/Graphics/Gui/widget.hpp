
/******************************************************************************
* MODULE     : widget.hpp
* DESCRIPTION: Definition of abstract widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WIDGET_H
#define WIDGET_H
#include "list.hpp"
#include "tree.hpp"
#include "blackbox.hpp"
#include "command.hpp"
#include "tm_timer.hpp"
#include "renderer.hpp" // for PIXEL

class window_rep;
typedef window_rep* window;
class url;
class widget;
class slot;
class widget_connection;
template<class T> class promise;

/******************************************************************************
* The abstract widget class
******************************************************************************/

class widget_rep: public abstract_struct {
protected:
  list<widget_connection> in;
  list<widget_connection> out;

public:
  widget_rep ();
  virtual ~widget_rep ();
  inline virtual void* derived_this () {return (widget_rep*)this; }
  virtual tm_ostream& print (tm_ostream& out);

  virtual void send (slot s, blackbox val);
    // send a message val to the slot s
  virtual blackbox query (slot s, int type_id);
    // obtain information of a given type from the slot s
  virtual widget read (slot s, blackbox index);
    // abstract read access (of type s) of a subwidget at position index
  virtual void write (slot s, blackbox index, widget w);
    // abstract write access (of type s) of a subwidget at position index
  virtual void notify (slot s, blackbox new_val);
    // notification of a change on a slot s which contains a state variable
  virtual void connect (slot s, widget w2, slot s2);
    // connect a state slot s to another slot s2 of another widget w2
  virtual void deconnect (slot s, widget w2, slot s2);
    // deconnect a state slot s from another slot s2 of another widget w2

  friend class widget;
};

template<> void tm_delete<widget_rep> (widget_rep* ptr);

class widget {
public:
ABSTRACT_NULL(widget);
  inline bool operator == (widget w) { return rep == w.rep; }
  inline bool operator != (widget w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(widget);

inline tm_ostream&
operator << (tm_ostream& out, widget w) {
  if (is_nil (w)) return out << "nil";
  else return w->print (out);
}

extern bool use_side_tools;

/******************************************************************************
* Widget style parameters
******************************************************************************/

#define WIDGET_STYLE_MINI                1
  // use smaller text font inside widget
#define WIDGET_STYLE_MONOSPACED          2
  // use monospaced font inside widget
#define WIDGET_STYLE_GREY                4
  // use grey text font
#define WIDGET_STYLE_PRESSED             8
  // indicate that a button is currently pressed
#define WIDGET_STYLE_INERT              16
  // only render but don't associate any action to widget
#define WIDGET_STYLE_BUTTON             32
  // indicate that a button should explicitly rendered as a button
#define WIDGET_STYLE_CENTERED           64
  // use centered text
#define WIDGET_STYLE_BOLD              128
  // use bold text

/******************************************************************************
* Window widgets
******************************************************************************/

widget plain_window_widget (widget w, string s, command quit= command ());
  // creates a decorated window with name s and contents w
widget popup_window_widget (widget w, string s);
  // creates an undecorated window with name s and contents w
void   destroy_window_widget (widget w);
  // destroys a window as created by the above routines

/******************************************************************************
* Top-level widgets, typically given as an argument to plain_window_widget
* See also message.hpp for specific messages for these widgets
******************************************************************************/

widget texmacs_widget (int mask, command quit);
  // the main TeXmacs widget and a command which is called on exit
  // the mask variable indicates whether the menu, icon bars, status bar, etc.
  // are visible or not
widget file_chooser_widget (command cmd, string type, string prompt);
  // file chooser widget for files of a given 'type';
  // for files of type "image", the widget includes a previsualizer for images
  // 'prompt' contains a prompt if we intend to save the file
  // and the empty string otherwise
widget printer_widget (command cmd, url ps_pdf_file);
  // widget for printing a file, offering a way for selecting a page range,
  // changing the paper type and orientation, previewing, etc.;
  // the command cmd is called on exit
widget color_picker_widget (command cmd, bool bg, array<tree> proposals);
  // widgets for selecting a color, a pattern or a background image,
  // encoded by a tree. On input, we give a list of recently used proposals
  // on termination the command is called with the selected color as argument
  // the bg flag specifies whether we are picking a background color or fill
widget inputs_list_widget (command call_back, array<string> prompts);
  // a dialogue widget with Ok and Cancel buttons and a series of textual
  // input widgets with specified prompts
widget popup_widget (widget w);
  // a widget container which results w to be unmapped as soon as
  // the pointer quits the widget

/******************************************************************************
* Widgets for the construction of menus
******************************************************************************/

widget horizontal_menu (array<widget> a);
  // a horizontal menu made up of the widgets in a
widget vertical_menu (array<widget> a);
  // a vertical menu made up of the widgets in a
widget tile_menu (array<widget> a, int cols);
  // a menu rendered as a table of cols columns wide & made up of widgets in a
widget minibar_menu (array<widget> a);
  // a small minibar, which can for instance occur inside another iconbar
widget menu_separator (bool vertical);
  // a horizontal or vertical menu separator
widget menu_group (string name, int style);
  // a menu group of a given style; the name should be greyed and centered

widget pulldown_button (widget w, promise<widget> pw);
  // a button w with a lazy pulldown menu pw
widget pullright_button (widget w, promise<widget> pw);
  // a button w with a lazy pullright menu pw
widget menu_button (widget w, command cmd,
		    string pre= "", string ks= "", int style= 0);
  // a command button with an optional prefix (o, * or v) and
  // keyboard shortcut; if ok does not hold, then the button is greyed
  // for pressed styles, the button is displayed as a pressed button
widget balloon_widget (widget w, widget help);
  // given a button widget w, specify a help balloon which should be displayed
  // when the user leaves the mouse pointer on the button for a small while

widget text_widget (string s, int style, color col, bool tsp= true);
  // a text widget with a given style, color and transparency
widget xpm_widget (url file_name);
  // a widget with an X pixmap icon
widget input_text_widget (command call_back, string type, array<string> def,
			  int style= 0, string width= "1w");
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  // an optional width may be specified for the input field
  // the width is specified in TeXmacs length format with units em, px or w
widget enum_widget (command cb, array<string> vals, string val,
                    int st= 0, string w= "1w");
  // select a value from a list of possible values
widget choice_widget (command cb, array<string> vals, string val);
  // select a value from a long list of possible values
widget choice_widget (command cb, array<string> vals, array<string> mc);
  // select multiple values from a long list
widget choice_widget (command cb, array<string> vals, string val, string filt);
  // select a value from a long list with scrollbars and an input to filter
widget tree_view_widget (command cmd, tree data, tree data_roles);
  // A widget with a tree view which observes the data and updates automatically

/******************************************************************************
* Other widgets
******************************************************************************/

widget empty_widget ();
  // an empty widget of size zero
widget glue_widget (bool hx=true, bool vx=true, SI w=0, SI h=0);
  // an empty widget of minimal width w and height h and which is horizontally
  // resp. vertically extensible if hx resp. vx is true
widget glue_widget (tree col, bool hx=true, bool vx=true, SI w=0, SI h=0);
  // a colored variant of the above widget, with colors as in the color picker
widget horizontal_list (array<widget> a);
  // a horizontal list made up of the widgets in a
widget vertical_list (array<widget> a);
  // a vertical list made up of the widgets in a
widget aligned_widget (array<widget> lhs, array<widget> rhs,
                       SI hsep= 3*PIXEL, SI vsep= 3*PIXEL,
                       SI lpad= 0, SI rpad= 0);
  // a table with two columns, the first one being right aligned and
  // the second one being left aligned
widget tabs_widget (array<widget> tabs, array<widget> bodies);
  // a tab bar where one and only of the bodies can be selected
widget icon_tabs_widget (array<url> us, array<widget> ss, array<widget> bs);
  // a variant of tabs_widget with named icon tabs
widget wrapped_widget (widget w, command quit);
  // copy of w, but with a separate reference counter,
  // and with a command to be called upon destruction
widget user_canvas_widget (widget wid, int style= 0);
  // a widget whose contents can be scrolled
  // if the size of the inner contents exceed the specified size
widget resize_widget (widget w, int style, string w1, string h1,
                      string w2, string h2, string w3, string h3,
                      string hpos, string vpos);
  // resize the widget w to be of minimal size (w1, h1),
  // of default size (w2, h2), of maximal size (w3, h3),
  // and initial scrolling position (hpos, vpos)
widget hsplit_widget (widget l, widget r);
  // two horizontally juxtaposed widgets l and r with an ajustable border
widget vsplit_widget (widget t, widget b);
  // two vertically juxtaposed widgets t and b with an ajustable border
widget extend (widget w, array<widget> a);
  // extend the size of w to the maximum of the sizes of
  // the widgets in the list a
widget toggle_widget (command cmd, bool on= false, int style= 0);
  // an input toggle
widget wait_widget (SI width, SI height, string message);
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
widget ink_widget (command cb);
  // widget for inking a sketch. The input may later be passed to
  // an external program for handwriting recognition,
  // using the callback routine
widget refresh_widget (string tmwid, string kind= "any");
  // a widget which is automatically constructed from the a dynamic
  // scheme widget tmwid. When receiving the send_refresh event,
  // the contents should also be updated dynamically by reevaluating
  // the scheme widget (in case of matching kind)
widget refreshable_widget (object prom, string kind= "any");
  // a widget which is automatically constructed from the a dynamic
  // scheme widget promise. When receiving the send_refresh event,
  // the contents should also be updated dynamically by reevaluating
  // the scheme widget promise (in case of matching kind)

/******************************************************************************
* Besides the widget constructors, any GUI implementation should also provide
* a simple_widget_rep class with the following virtual methods:
******************************************************************************/

// bool simple_widget_rep::is_editor_widget ();
//   should return true for editor widgets only
// void simple_widget_rep::handle_get_size_hint (SI& w, SI& h);
//   propose a size for the widget
// void simple_widget_rep::handle_notify_resize (SI w, SI h);
//   issued when the size of the widget has changed
// void simple_widget_rep::handle_keypress (string key, time_t t);
//   issed when a key is pressed
// void simple_widget_rep::handle_keyboard_focus (bool new_focus, time_t t);
//   issued when the keyboard focus of the widget has changed
// void simple_widget_rep::handle_mouse
//        (string kind, SI x, SI y, int mods, time_t t);
//   a mouse event of a given kind at position (x, y) and time t
//   mods contains the active keyboard modifiers at time t
// void simple_widget_rep::handle_set_zoom_factor (double zoom);
//   set the zoom factor for painting
// void simple_widget_rep::handle_clear
//        (renderer ren, SI x1, SI y1, SI x2, SI y2);
//   clear the widget to the background color
//   this event may for instance occur when scrolling
// void simple_widget_rep::handle_repaint
//        (renderer ren, SI x1, SI y1, SI x2, SI y2);
//   repaint the region (x1, y1, x2, y2)

#endif // defined WIDGET_H
