
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
#include "QTMWidget.hpp"

/*! A widget with a renderer.
 
 TeXmacs input and output buffers are based upon this widget: qt_tm_widget_rep
 (for the main TeXmacs window), qt_tm_embedded_widget_rep (for simpler, 
 auxilliary texmacs input buffers without toolbars, etc.) and
 qt_simple_widget_rep inherit from this class. The latter is a bit different
 because it is subclassed many times across all of TeXmacs, for instance to
 implement pure output widgets.
 
 Buffers are drawn in qt_view_widgets (...)
 
 MEMORY POLICY: 
 It's qt_window_widget_rep, NOT qt_view_widget_rep who owns the QWidget. This
 is to avoid crashing when closing other windows:
 The same QWidget is owned by qt_view_widget_rep and qt_window_widget_rep after
 a call to qt_view_widget_rep::plain_window_widget, so one of them has to 
 destroy it, but we also must destroy windows which are not qt_views, so it 
 seems to make more sense there. For example this is needed for dialogs
 constructed with scheme code. Destroying in qt_widget_rep is of course 
 impossible because of the same reason: two instances of its subclasses might 
 share a QWidget.

 FIXME?
   - Qt specifies that widgets with a parent are deleted by the parent.
   - Our policy is that qt_window_widget_rep owns the QWidget (so it is 
     responsible to delete it)
 Are these two requirements compatible?
*/
class qt_view_widget_rep: public qt_widget_rep {
public:
  basic_renderer current_renderer;
  
public:
  qt_view_widget_rep (QTMWidget* _view, types _type=view_widget);
  virtual ~qt_view_widget_rep () { }

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  void set_current_renderer(basic_renderer _r) { current_renderer = _r;  }
  basic_renderer get_current_renderer() {  return current_renderer; }

  QTMWidget*         canvas () { return static_cast<QTMWidget*> (qwid); }
  QTMScrollView* scrollarea () { return static_cast<QTMScrollView*> (qwid); }
};


#endif // QT_VIEW_WIDGET_HPP
