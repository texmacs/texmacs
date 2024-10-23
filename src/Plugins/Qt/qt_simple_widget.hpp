
/******************************************************************************
* MODULE     : qt_simple_widget.hpp
* DESCRIPTION: A widget containing a TeXmacs canvas.
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_SIMPLE_WIDGET_HPP
#define QT_SIMPLE_WIDGET_HPP

#include "hashset.hpp"
#include "basic_renderer.hpp"

#include "qt_widget.hpp"
#include "QTMScrollView.hpp"
#include "QTMWidget.hpp"

/*! A widget containing a TeXmacs canvas.
 
 This canvas can be used both for input or output of typesetted documents. 
 Editors (editor_rep), output-only widgets (box_widget_rep) and
 other classes are extensions to a "simple_widget", quite a misnomer...
 
 MEMORY POLICY: as usual, we give ownership of the QWidget to the caller of
 as_qwidget(), which in our case will be one of qt_tm_widget_rep or 
 qt_tm_embedded_rep. These will embed our QWidget in a QLayout who will reparent
 it to the QWidget using the layout (e.g. QTMWindow::centralWidget())
  */
class qt_simple_widget_rep: public qt_widget_rep {

    // We keep a pointer to ourselves to avoid accidental deletion by our own
    // QTMWidget, who keeps a smart pointer to us.
    // (not sure whether this works/is necessary, though)
  //qt_widget self;
  
  typedef struct t_slot_entry {
    int seq;
    slot_id id;
    blackbox val;
    t_slot_entry() : seq(-1), id (slot_id__LAST), val (blackbox()) { }
    t_slot_entry(const t_slot_entry& other)
    : seq (other.seq), id (other.id), val (other.val) { };
    bool operator< (const t_slot_entry& b) const { return this->seq < b.seq; }
  } t_slot_entry;
  
  t_slot_entry sent_slots[slot_id__LAST];
  
  int sequencer;
  
  
public:
  qt_simple_widget_rep ();
  ~qt_simple_widget_rep ();
  
  virtual bool is_editor_widget ();
  virtual bool is_embedded_widget ();
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data= array<double> ());
  virtual void handle_set_zoom_factor (double zoom);
  virtual void handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2);
  
    ////////////////////// Handling of TeXmacs' messages
  
  void save_send_slot (slot s, blackbox val);
  void reapply_sent_slots();
  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
    ////////////////////// Qt semantics of abstract texmacs widgets

  virtual QAction* as_qaction();
  virtual QWidget* as_qwidget();
  
    ////////////////////// Qt widget counterparts

  QTMWidget*         canvas () { return qobject_cast<QTMWidget*> (qwid); }
  QTMScrollView* scrollarea () { return qobject_cast<QTMScrollView*> (qwid); }

    ////////////////////// backing store management

  static void repaint_all (); // called by qt_gui_rep::update()

protected:
  
  static hashset<pointer> all_widgets;
  rectangles   invalid_regions;
  QPixmap*     backingPixmap;  
#if QT_VERSION >= 0x060000
  qreal oldDpr = 0;
#endif
  QPoint       backing_pos;


  void invalidate_rect (int x1, int y1, int x2, int y2);
  void invalidate_all ();
  bool is_invalid ();
  void repaint_invalid_regions ();
  basic_renderer get_renderer();
  
  
  friend class QTMWidget;
};

inline qt_simple_widget_rep* concrete_simple_widget (qt_widget w) {
  return static_cast<qt_simple_widget_rep*>(w.rep); 
}

inline qt_simple_widget_rep* concrete_simple_widget (widget w) {
  return static_cast<qt_simple_widget_rep*>(w.rep);
}

// Export for TeXmacs' use
typedef qt_simple_widget_rep simple_widget_rep;

#endif // defined QT_SIMPLE_WIDGET_HPP
