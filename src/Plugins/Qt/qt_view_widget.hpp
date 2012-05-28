
/******************************************************************************
 * MODULE     : qt_view_widget.hpp
 * DESCRIPTION: A widget with a renderer.
 * COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_VIEW_WIDGET_HPP
#define QT_VIEW_WIDGET_HPP

#include "basic_renderer.hpp"
#include "qt_widget.hpp"

class QWidget;

/*! A widget with a renderer.
 
 TeXmacs input and output buffers are based upon this widget: qt_tm_widget_rep
 (for the main TeXmacs window), qt_tm_embedded_widget_rep (for simpler, 
 auxilliary texmacs input buffers without toolbars, etc.) and
 qt_simple_widget_rep inherit from this class. The latter is a bit different
 because it is subclassed many times across all of TeXmacs.
 
 Buffers are drawn in qt_view_widgets (...)
 
 
*/
class qt_view_widget_rep: public qt_widget_rep {
public:
  basic_renderer current_renderer;
  
public:
  qt_view_widget_rep (QWidget* _view, types _type=view_widget);
  ~qt_view_widget_rep ();

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  void set_current_renderer(basic_renderer _r) { current_renderer = _r;  }
  basic_renderer get_current_renderer() {  return current_renderer; }
};


#endif // QT_VIEW_WIDGET_HPP
