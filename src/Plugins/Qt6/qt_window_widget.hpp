
/******************************************************************************
* MODULE     : qt_window_widget.hpp
* DESCRIPTION: QT window widget.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_WINDOW_WIDGET_HPP
#define QT_WINDOW_WIDGET_HPP

#include "qt_widget.hpp"

class QWidget;

/*! Models the simplest top-level window possible
 
 When any TeXmacs widget needs promotion into a TeXmacs window, the method
 qt_widget_rep::plain_window_widget() or any reimplementation thereof 
 encapsulates it into a qt_window_widget. This class handles the necessary 
 slots, in particular all those already handled by qt_widget_rep.
 
 This qt_widget takes ownership of the enclosed QWidget and marks it as a 
 "texmacs_window_widget" using QObject::property(). The value of the property is
 set to this qt_window_widget_rep.

 Later, the handling of some texmacs messages (SLOT_WINDOW in qt_simple_widget_rep 
 for instance) will require access to an instance of qt_window_widget which they
 retrieve using the static member widget_from_qwidget(), who in turn uses the
 mentioned property.
 
 qt_window_widget_rep is the *sole responsible* for the deletion of QWidgets.
 This makes sense in as much as all QWidgets are owned by some texmacs widget
 which is inside some texmacs window: the method plain_window_widget must ALWAYS 
 return a qt_window_widget_rep or we'll leak.
*/
class qt_window_widget_rep: public qt_widget_rep {
protected:
  int win_id;        //!< Unique integer identifier, returned by SLOT_IDENTIFIER.
  string orig_name;  //!< Unique name assigned to the window.
  command quit;      //!< Command to be executed when the window is closed.
  bool fake;         //!< Whether this truly is a window (or a docked widget).

public:
  
  qt_window_widget_rep (QWidget* _wid, string name, command q, bool fake=false);
  ~qt_window_widget_rep ();

  virtual inline string get_nickname () { return orig_name; }
  virtual widget popup_window_widget (string s);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  virtual void    notify (slot s, blackbox new_val);
	
  static widget_rep* widget_from_qwidget (QWidget* qwid);

  static bool has_resizable_children (QWidget* w, bool ret=false);
};

/*!
 */
class qt_popup_widget_rep: public qt_widget_rep {
public:
  command quit;
  
  qt_popup_widget_rep (widget wid, command q);
  ~qt_popup_widget_rep ();
  
  virtual widget popup_window_widget (string s);
  virtual widget tooltip_window_widget (string s);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
};

#endif // defined QT_WINDOW_WIDGET_HPP
