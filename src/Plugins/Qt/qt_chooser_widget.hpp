/******************************************************************************
 * MODULE     : qt_chooser_widget.hpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_CHOOSER_WIDGET_HPP
#define QT_CHOOSER_WIDGET_HPP

#include "qt_widget.hpp"
#include "qt_utilities.hpp"

class qt_chooser_widget_rep: public qt_widget_rep {
protected:      
  command cmd;
  string type;
  bool   save;
  string win_title;
  string directory;
  coord2 position;
  coord2 size;
  string file;
  
public:
  qt_chooser_widget_rep (command, string, bool);
  ~qt_chooser_widget_rep ();
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
  virtual widget plain_window_widget (string s);
  
  void perform_dialog();
};


#endif  // QT_CHOOSER_WIDGET_HPP
