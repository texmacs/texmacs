
/******************************************************************************
* MODULE     : tm_window.hpp
* DESCRIPTION: TeXmacs main data structures (buffers, views and windows)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_WINDOW_H
#define TM_WINDOW_H
#include "tm_buffer.hpp"

window texmacs_window (wk_widget wid, tree geom);
window texmacs_window (widget wid, tree geom);

class tm_window_rep {
public:
  window    win;
  tm_widget wid;
  int       id;

  inline tm_window_rep (tm_widget wid2, tree geom):
    win (texmacs_window (abstract (wid2), geom)),
    wid (wid2), id (create_window_id ()) {}
  inline ~tm_window_rep () { destroy_window_id (id); }

  inline int serial () {
    return wid->serial; }
  inline void set_property (scheme_tree what, scheme_tree val) {
    wid->props (what)= val; }
  inline scheme_tree get_property (scheme_tree what) {
    return wid->props [what]; }

  inline void set_window_name (string s) {
    wid->set_window_name (s); }
  inline void set_popup_menu (wk_widget w, SI x, SI y) {
    wid->set_popup_menu (w, x, y); }

  inline wk_widget get_header () {
    return wk_widget (wid) ["header"]; }
  inline wk_widget get_canvas () {
    return wk_widget (wid) ["canvas"]; }
  inline void set_subwidget (wk_widget w, string which, wk_widget sw) {
    wid->set_subwidget (w, which, sw); }
  inline bool get_subwidget_flag (wk_widget w) {
    return wid->get_subwidget_flag (w); }
  inline void set_subwidget_flag (wk_widget w, bool on) {
    wid->set_subwidget_flag (w, on); }

  inline void interactive (string name, string type, array<string> def,
			   string& s, command cmd) {
    wid->interactive (name, type, def, s, cmd); }
  inline void interactive_return () {
    wid->interactive_return (); }
  inline void set_left_footer (string s) {
    wid->set_left_footer (s); }
  inline void set_right_footer (string s) {
    wid->set_right_footer (s); }
  inline int  get_footer_mode () {
    return wid->get_footer_mode (); }
  inline void set_footer_mode (int which) {
    wid->set_footer_mode (which); }
  inline bool get_footer_flag () {
    return wid->get_footer_flag (); }
  inline void set_footer_flag (bool on) {
    wid->set_footer_flag (on); }
  inline int  get_shrinking_factor () {
    return wid->get_shrinking_factor (); }
  inline void set_shrinking_factor (int sf) {
    wid->set_shrinking_factor (sf); }

  inline void menu_main (string menu) {
    wid->menu_main (menu); }
  inline void menu_icons (int which, string menu) {
    wid->menu_icons (which, menu); }
};

class tm_view_rep {
public:
  tm_buffer buf;
  editor    ed;
  tm_window win;
  inline tm_view_rep (tm_buffer buf2, editor ed2):
    buf (buf2), ed (ed2), win (NULL) {}
};

typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;
typedef tm_window_rep* tm_window;

#endif // defined TM_WINDOW_H
