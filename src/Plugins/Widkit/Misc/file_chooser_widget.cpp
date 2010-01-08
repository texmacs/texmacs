
/******************************************************************************
* MODULE     : file_chooser.cpp
* DESCRIPTION: A file_chooser widget with horizontal and vertical scrollbars.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/basic_widget.hpp"
#include "Widkit/attribute_widget.hpp"
#include "Widkit/layout.hpp"

#include "bitmap_font.hpp"
#include "font.hpp"
#include "window.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

#ifdef OS_WIN32
#include <X11/Xlib.h>
#endif

/******************************************************************************
* File chooser commands
******************************************************************************/

#define CHANGE_FILE      0
#define CHANGE_DIR       1
#define BUTTON_HOME      2
#define BUTTON_TEXTS     3
#define BUTTON_FILE_OK   4
#define BUTTON_DIR_OK    5
#define BUTTON_CANCEL    6
#define IMAGE_HSIZE      7
#define IMAGE_VSIZE      8
#define IMAGE_CLIP_X1    9
#define IMAGE_CLIP_Y1   10
#define IMAGE_CLIP_X2   11
#define IMAGE_CLIP_Y2   12
#define CHANGE_SUFFIXES 13

class file_chooser_command_rep: public command_rep {
  wk_widget_rep* fch;
  int                type;
public:
  file_chooser_command_rep (wk_widget w, int t): fch(w.rep), type(t) {}
  void apply ();
  tm_ostream& print (tm_ostream& out) {
    return out << "File chooser command (" << type << ")"; }
};

void
file_chooser_command_rep::apply () {
  wk_widget fch_wid (fch);
  switch (type) {
  case CHANGE_FILE:
    {
      string s;
      fch_wid[0]["file"]["input"] << get_string ("input", s);
      fch_wid << set_string ("return", scm_unquote (s));
      break;
    }
  case CHANGE_DIR:
    {
      string dir;
      fch_wid[0]["directory"]["input"] << get_string ("input", dir);
      if (dir == "#f") fch_wid << set_string ("return", "#f");
      else {
	dir= scm_unquote (dir);
	fch_wid << set_string ("directory", dir);
      }
      break;
    }
  case BUTTON_HOME:
    fch_wid << set_string ("directory", "~");
    break;
  case BUTTON_TEXTS:
    fch_wid << set_string ("directory",
			   as_string (url ("$TEXMACS_HOME_PATH", "texts")));
    break;
  case BUTTON_FILE_OK:
    {
      string s;
      fch_wid[0]["file"]["input"] << get_string ("input", s);
      fch_wid << set_string ("return", scm_unquote (s));
      break;
    }
  case BUTTON_DIR_OK:
    {
      string s;
      fch_wid[0]["directory"]["input"] << get_string ("input", s);
      fch_wid << set_string ("return", scm_unquote (s));
      break;
    }
  case BUTTON_CANCEL:
    fch_wid << set_string ("return", "#f");
    break;
  case CHANGE_SUFFIXES:
    {
      string sxs;
      fch_wid[0]["suffixes"]["input"] << get_string ("input", sxs);
      fch_wid << set_string ("suffixes", scm_unquote (sxs));
      break;
    }
  default:
    {
      string which, s;
      if (type == IMAGE_HSIZE) which= "hsize";
      else if (type == IMAGE_VSIZE) which= "vsize";
      else if (type == IMAGE_CLIP_X1) which= "clip-x1";
      else if (type == IMAGE_CLIP_Y1) which= "clip-y1";
      else if (type == IMAGE_CLIP_X2) which= "clip-x2";
      else which= "clip-y2";
      wk_widget inp= fch_wid[0]["image"]["parameters"][which]["input"];
      inp << get_string ("input", s);
      if (s == "#f") fch_wid << set_string ("return", "#f");
      else inp << set_string ("input", scm_unquote (s));
      break;
    }
  }
}

command
file_chooser_command (wk_widget fch, int type) {
  return tm_new<file_chooser_command_rep> (fch, type);
}

/******************************************************************************
* File_list widgets
******************************************************************************/

class file_list_widget_rep: public attribute_widget_rep {
  wk_widget_rep* fch;
  string             dir;
  array<bool>        lids;
  array<string>      names;
  array<string>      suffix;
  bool               dir_flag;
  int                hilight;

public:
  file_list_widget_rep (wk_widget ch, array<string> suffix, bool dir_flag);
  operator tree ();

  wk_widget get_canvas ();

  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
  void handle_set_string (set_string_event ev);
};

/******************************************************************************
* Implementation of file_list widgets
******************************************************************************/

file_list_widget_rep::file_list_widget_rep (
  wk_widget c, array<string> s, bool f):
    attribute_widget_rep (0), fch (c.rep),
    dir (""), suffix (s), dir_flag (f), hilight (-1) {}

file_list_widget_rep::operator tree () {
  return "file_list";
}

wk_widget
file_list_widget_rep::get_canvas () {
  string which (dir_flag? string ("directories"): string ("files"));
  wk_widget fch_wid (fch);
  return fch_wid[0]["list"][which];
}

static bool
has_suffix (string name, array<string> suffix) {
  int i;
  for (i=0; i<N(suffix); i++)
    if (ends (locase_all (name), suffix[i])) return true;
  return false;
}

static bool
list_in_directory (string dir, string name,
		   array<string> suffix, bool dir_flag)
{
  if (name == "") return false;
  if (name == "..") return dir_flag;
  if (name[0]=='.') return false;
  if (dir_flag) return is_directory (url_system (dir, name));
  else return is_regular (url_system (dir, name)) && has_suffix (name, suffix);
}

void
file_list_widget_rep::handle_get_size (get_size_event ev) {
  int i;
  metric ex;
  font fn= get_default_font ();
  ev->w= ev->h= 0;
  for (i=0; i<N(names); i++)
    if (lids[i]) {
      fn->var_get_extents (names[i], ex);
      ev->w  = max (ev->w, ((ex->x2- ex->x1+ 2)/3) + (6*PIXEL));
      ev->h += ((fn->y2- fn->y1+ 2)/3) + (4*PIXEL);
    }
  abs_round (ev->w, ev->h);
}

void
file_list_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  int i; 
  metric ex;
  ren->set_background (white);
  ren->clear (0, -h, w, 0);
  font fn= get_default_font ();
  ren->set_shrinking_factor (3);
  SI y= 0;
  for (i=0; i<N(names); i++)
    if (lids[i]) {
      ren->set_color (black);
      if (hilight == i) ren->set_color (red);
      fn->var_get_extents (names[i], ex);
      fn ->draw (ren, names[i], 9*PIXEL, y-fn->y2-6*PIXEL);
      y += fn->y1- fn->y2- 12*PIXEL;
    }
  ren->set_shrinking_factor (1);
}

void
file_list_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;

  if ((type == "release-left") || (type == "release-right")) {
    int i;
    SI y= 0, search= ev->y*3;
    metric ex;
    font fn= get_default_font ();
    for (i=0; i<N(names); i++)
      if (lids[i]) {
	fn->var_get_extents (names[i], ex);
	if ((search >= (y+ fn->y1- fn->y2- 12*PIXEL)) && (search < y)) break;
	y += fn->y1- fn->y2- 12*PIXEL;
      }
    if (i==N(names)) return;

    string s= names[i];
    wk_widget fch_wid (fch);
    if (hilight == i) {
      if (dir_flag) {
	string name= as_string (url_system (dir, s));
	fch_wid << set_string ("directory", name);
      }
      else fch_wid << set_string ("return", s);
    }
    else {
      hilight= i;
      if (!dir_flag) fch_wid << set_string ("file", s);
      this << emit_invalidate_all ();
    }
  }

  if ((type == "press-up") || (type == "press-down")) {
    SI x, y, dy= 100*PIXEL;
    if (type == "press-down") dy= -dy;
    get_canvas () << get_coord2 ("scroll position", x, y);
    y += dy;
    get_canvas () << set_scroll_pos (x, y);
  }
}

void
file_list_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "directory") {
    dir= ev->s;
    bool flag;
    names= read_directory (url_system (dir), flag);
    lids= array<bool>(N(names));
    for (int i=0; i<N(names); i++)
      lids[i]= list_in_directory (dir, names[i], suffix, dir_flag);
    SI w, h;
    this << get_size (w, h, 0);
    get_canvas () << set_extents (0, -h, w, 0);
    hilight=-1;
    if (attached ()) this << emit_invalidate_all ();
  }
  else attribute_widget_rep::handle_set_string (ev);
}

/******************************************************************************
* image widgets
******************************************************************************/

class image_widget_rep: public attribute_widget_rep {
  string file_name;
public:
  image_widget_rep ();
  operator tree ();
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_set_string (set_string_event ev);
};

/******************************************************************************
* Implementation of image widgets
******************************************************************************/

image_widget_rep::image_widget_rep ():
  attribute_widget_rep (0, south_west), file_name ("") {}

image_widget_rep::operator tree () {
  return "image";
}

void
image_widget_rep::handle_get_size (get_size_event ev) {
  ev->w= 221*PIXEL;
}

void
image_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  ren->set_background (white);
  ren->clear (0, 0, w, h);
  layout_dark_outline (ren, 0, 0, w, h);
  if (file_name != "") {
    SI iw, ih;
    image_size (url_system (file_name), iw, ih);
    
    SI ww= w-2*PIXEL, hh= h-2*PIXEL;
    if ((ww>0) && (hh>0) && (iw>0) && (ih>0)) {
      if (iw * hh > ih * ww)
	hh= (ww * ih) / iw;
      else ww= (hh * iw) / ih;
    }

    ren->image (url_system (file_name),
		ww, hh, PIXEL, PIXEL, 0.0, 0.0, 1.0, 1.0);
  }
}

void
image_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "name") {
    file_name= ev->s;
    if (attached ()) this << emit_invalidate_all ();
  }
  else attribute_widget_rep::handle_set_string (ev);
}

/******************************************************************************
* File_Chooser widgets
******************************************************************************/

class file_chooser_widget_rep: public attribute_widget_rep {
  command       cmd;
  string        type;
  array<string> suffix;
  string        magn;

public:
  file_chooser_widget_rep (command cmd, string type, string magn);
  operator tree ();

  wk_widget input_widget (string what, int type);
  wk_widget button_widget (string what, int type);

  void handle_get_size (get_size_event ev);
  void handle_set_string (set_string_event ev);
  void handle_get_string (get_string_event ev);
  void handle_destroy (destroy_event ev);
};

/******************************************************************************
* Drives under Windows
******************************************************************************/

#ifdef OS_WIN32
class drive_menu_command_rep: public command_rep {
  string driveLetter;
  file_chooser_widget_rep *fileChooser;

public:
  drive_menu_command_rep (file_chooser_widget_rep *fileChooser2,
			  string driveLetter2):
    fileChooser (fileChooser2), driveLetter(driveLetter2) {}
  void apply () {
    fileChooser << set_string("directory", driveLetter);
  }
};
#endif

/******************************************************************************
* Implementation of file_chooser widgets
******************************************************************************/

wk_widget
file_chooser_widget_rep::input_widget (string what, int type) {
  array<wk_widget> ww (2);
  array<string> nn (2);
  ww[0]= text_wk_widget (what, false, "english");
  ww[1]= input_text_wk_widget (file_chooser_command (this, type));
  nn[1]= "input";
  if (type == CHANGE_DIR) ww[1] << set_string ("type", "directory");
  return horizontal_list (ww, nn);
}

wk_widget
file_chooser_widget_rep::button_widget (string what, int type) {
  return command_button (text_wk_widget (what, false, "english"),
			 file_chooser_command (this, type), true);
}

file_chooser_widget_rep::file_chooser_widget_rep (
  command cmd2, string type2, string magn2):
  attribute_widget_rep (1), cmd (cmd2), type (type2), magn (magn2)
{
  ref_count++;

  tree t= stree_to_tree (call ("format-get-suffixes*", type));
  int i, n= N(t);
  for (i=0; i<n; i++)
    suffix << ("." * as_string (t[i]));
  if (n == 0) suffix << string ("");

  SI sep= 3*PIXEL;
  int cw2n= 5;
  if (type == "directory") cw2n= 3;
  array<wk_widget> cw2 (cw2n);
  array<string> cn2 (cw2n);
  cw2[0]= glue_wk_widget (false, true, sep);
  cw2[1]=
    canvas_widget (wk_widget (tm_new<file_list_widget_rep> (this, suffix, true)));
  cn2[1]= "directories";
  cw2[2]= glue_wk_widget (false, true, sep);
  if (type != "directory") {
    cw2[3]=
      canvas_widget (wk_widget (tm_new<file_list_widget_rep> (this,suffix,false)));
    cn2[3]= "files";
    cw2[4]= glue_wk_widget (false, true, sep-PIXEL);
  }

#ifdef OS_WIN32
  wk_widget drive_menu = vertical_menu (array<wk_widget> ());
  unsigned int driveMask = XGetDrivesMask();
  char driveString[4] = "A:\\";
  for (char x = 'A'; x <= 'Z'; x++)
    if(driveMask & (1 << (x - 'A'))) {
      driveString[0] = x;
      drive_menu << emit_insert (driveString,
	command_button (text_wk_widget (driveString),
			tm_new<drive_menu_command_rep> (this, driveString)));
    }
  array<wk_widget> drw (2);
  drw[0] = pullright_button (text_wk_widget ("Drive"), drive_menu);
  drw[1] = text_wk_widget("");
  // drw[1]= glue_wk_widget (false, true, sep);
#endif

  int BUTTON_OK= BUTTON_FILE_OK;
  if (type == "directory") BUTTON_OK= BUTTON_DIR_OK;

#ifdef OS_WIN32
  array<wk_widget> cw3 (11);
  cw3[0]= glue_wk_widget (false, false, sep);
  cw3[1]= pulldown_button (text_wk_widget ("Drive"), drive_menu, true);
  cw3[2]= glue_wk_widget (false, false, sep);
  cw3[3]= button_widget ("Home", BUTTON_HOME);
  cw3[4]= glue_wk_widget (false, false, sep);
  cw3[5]= button_widget ("Texts", BUTTON_TEXTS);
  cw3[6]= glue_wk_widget (true, false);
  cw3[7]= button_widget ("Ok", BUTTON_OK);
  cw3[8]= glue_wk_widget (false, false, sep);
  cw3[9]= button_widget ("Cancel", BUTTON_CANCEL);
  cw3[10]= glue_wk_widget (false, false, sep);
#else
  array<wk_widget> cw3 (9);
  cw3[0]= glue_wk_widget (false, false, sep);
  cw3[1]= button_widget ("Home", BUTTON_HOME);
  cw3[2]= glue_wk_widget (false, false, sep);
  cw3[3]= button_widget ("Texts", BUTTON_TEXTS);
  cw3[4]= glue_wk_widget (true, false);
  cw3[5]= button_widget ("Ok", BUTTON_OK);
  cw3[6]= glue_wk_widget (false, false, sep);
  cw3[7]= button_widget ("Cancel", BUTTON_CANCEL);
#ifdef OS_MACOS
  cw3[8]= glue_wk_widget (false, false, sep + 14*PIXEL);
#else
  cw3[8]= glue_wk_widget (false, false, sep);
#endif
#endif

  int cwn= 11;
  if (type == "image") cwn= 17;
  if (type == "directory") cwn= 7;
  array<wk_widget> cw (cwn);
  array<string> cn (cwn);
  cw[0]= glue_wk_widget (true, false, 0, sep);
  cw[1]= input_widget ("Directory:", CHANGE_DIR);
  cn[1]= "directory";
  cw[2]= glue_wk_widget (true, false, 0, sep);

  if (type == "directory") {
    cw[3]= horizontal_list (cw2, cn2);
    cn[3]= "list";
  }

  if (type != "directory") {
    cw[3]= input_widget ("File:", CHANGE_FILE);
    cn[3]= "file";
    cw[4]= glue_wk_widget (true, false, 0, sep);
    cw[5]= input_widget ("Suffixes:", CHANGE_SUFFIXES);
    cn[5]= "suffixes";
    cw[6]= glue_wk_widget (true, false, 0, sep);
    cw[7]= horizontal_list (cw2, cn2);
    cn[7]= "list";
  }

  if (type == "image") {
    array<wk_widget> imw (11);
    array<string> ims (11);
    imw[ 0]= input_widget ("width:", IMAGE_HSIZE);
    ims[ 0]= "hsize";
    imw[ 1]= glue_wk_widget (true, false, 0, sep);
    imw[ 2]= input_widget ("height:", IMAGE_VSIZE);
    ims[ 2]= "vsize";
    imw[ 3]= glue_wk_widget (true, false, 0, sep);
    imw[ 4]= input_widget ("left border:", IMAGE_CLIP_X1);
    ims[ 4]= "clip-x1";
    imw[ 5]= glue_wk_widget (true, false, 0, sep);
    imw[ 6]= input_widget ("lower border:", IMAGE_CLIP_Y1);
    ims[ 6]= "clip-y1";
    imw[ 7]= glue_wk_widget (true, false, 0, sep);
    imw[ 8]= input_widget ("right border:", IMAGE_CLIP_X2);
    ims[ 8]= "clip-x2";
    imw[ 9]= glue_wk_widget (true, false, 0, sep);
    imw[10]= input_widget ("upper border:", IMAGE_CLIP_Y2);
    ims[10]= "clip-y2";

    array<wk_widget> cw4 (5);
    array<string> cn4 (5);
    cw4[0] = glue_wk_widget (false, false, sep);
    cw4[1] = vertical_list (imw, ims);
    cn4[1] = "parameters";
    cw4[2] = glue_wk_widget (false, false, sep);
    cw4[3] = tm_new<image_widget_rep> ();
    cn4[3] = "image";
    cw4[4] = glue_wk_widget (false, false, sep);

    cw[ 8] = separator_wk_widget ();
    cw[ 9] = glue_wk_widget (true, false, 0, sep);
    cw[10] = horizontal_list (cw4, cn4);
    cn[10] = "image";
    cw[11] = glue_wk_widget (true, false, 0, sep);
    cw[12] = separator_wk_widget ();
    cw[13] = glue_wk_widget (true, false, 0, sep);
  }

  cw[cwn-3]= glue_wk_widget (true, false, 0, sep);
  cw[cwn-2]= horizontal_list (cw3);
  cn[cwn-2]= "buttons";
  cw[cwn-1]= glue_wk_widget (true, false, 0, sep);

  a[0]= vertical_list (cw, cn);

  if (type != "directory") {
    string s;
    for (i=0; i<N(suffix); ++i) {
      if (i) s << " ";
      s << suffix[i];
    }
    a[0]["suffixes"]["input"] << set_string ("input", s);
  }

  ref_count--;
}

file_chooser_widget_rep::operator tree () {
  return tree (TUPLE, "file_chooser", (tree) a[0]);
}

void
file_chooser_widget_rep::handle_get_size (get_size_event ev) {
  if (ev->mode < 1) {
    ev->w= 451*PIXEL;
    if (type == "image") ev->h= 500*PIXEL;
    else ev->h= 350*PIXEL;
  }
  else gui_maximal_extents (ev->w, ev->h);
}

void
file_chooser_widget_rep::handle_set_string (set_string_event ev) {
  if (ev->which == "directory") {
    string dir= as_string (url_pwd () * url_system (ev->s));
    a[0]["directory"]["input"] << set_string ("input", dir);
    a[0]["list"]["directories"] << set_string ("directory", dir);
    if (type != "directory") {
      // a[0]["file"]["input"] << set_string ("input", "");
      a[0]["list"]["files"] << set_string ("directory", dir);
    }
  }
  else if (ev->which == "file") {
    if (type == "directory") return;
    a[0]["file"]["input"] << set_string ("input", ev->s);
    if (type == "image") {
      string dir, name= ev->s;
      a[0]["directory"]["input"] << get_string ("input", dir);
      if (name != "") name= as_string (url_system (scm_unquote (dir), name));
      a[0]["image"]["image"] << set_string ("name", name);
      array<string> ps_suffix;
      ps_suffix << string (".ps") << string (".eps");
      wk_widget par_wid= a[0]["image"]["parameters"];
      if (has_suffix (name, ps_suffix)) {
	par_wid["hsize"]["input"] << set_string ("input", "");
	par_wid["vsize"]["input"] << set_string ("input", "");
      }
      else {
	par_wid["hsize"]["input"] << set_string ("input", magn);
	par_wid["vsize"]["input"] << set_string ("input", magn);
      }
    }
  }
  else if (ev->which == "return") {
    string s= ev->s;
    if (type == "directory") {
      a[0]["directory"]["input"] << set_string ("input", s);
      cmd ();
    }
    else {
      if (s != "#f" && !has_suffix (s, suffix))
	a[0]["file"]["input"] << set_string ("input", s * suffix[0]);
      else {
	a[0]["file"]["input"] << set_string ("input", s);
	cmd ();
      }
    }
  }
  else if (ev->which == "suffixes") {
    if (type == "directory") return;
    // surely the following can be done better:
    suffix->resize(0);
    int i=0, any=0;
    string s= scm_unquote (ev->s);
    a[0]["suffixes"]["input"] << set_string ("input", s);
    while (i<N(s)) {
      while (s[i]==' ') ++i;
      int j=i;
      while (j<N(s) && s[j]!=' ') ++j;
      if (j>i) {
        suffix << s(i,j);
        any=1;
      }
      i=j+1;
    }
    if (!any) suffix << string ("");
    // Force a refresh:
    string dir;
    a[0]["directory"]["input"] << get_string ("input", dir);
    a[0]["list"]["directories"] << set_string ("directory", scm_unquote (dir));
    a[0]["list"]["files"] << set_string ("directory", scm_unquote (dir));
  }
  else attribute_widget_rep::handle_set_string (ev);
}

void
file_chooser_widget_rep::handle_get_string (get_string_event ev) {
  if (ev->which == "input") {
    string dir, name;
    a[0]["directory"]["input"] << get_string ("input", dir);
    if (type == "directory") {
      a[0]["directory"]["input"] << get_string ("input", name);
      if (name == "#f") { ev->s= "#f"; return; }
      url u= url_system (scm_unquote (dir));
      ev->s= "(url-system " * scm_quote (as_string (u)) * ")";
    }
    else {
      a[0]["file"]["input"] << get_string ("input", name);
      if (name == "#f") { ev->s= "#f"; return; }
      url u= url_system (scm_unquote (dir)) * url_system (scm_unquote (name));
      ev->s= "(url-system " * scm_quote (as_string (u)) * ")";
    }
    if (type == "image") {
      string hsize, vsize, cx1, cy1, cx2, cy2;
      wk_widget par= a[0]["image"]["parameters"];
      par["hsize"]["input"] << get_string ("input", hsize);
      par["vsize"]["input"] << get_string ("input", vsize);
      par["clip-x1"]["input"] << get_string ("input", cx1);
      par["clip-y1"]["input"] << get_string ("input", cy1);
      par["clip-x2"]["input"] << get_string ("input", cx2);
      par["clip-y2"]["input"] << get_string ("input", cy2);
      ev->s=
	"(list " * ev->s * " " * hsize * " " * vsize * " "
	         * cx1 * " " * cy1 * " " * cx2 * " " * cy2 * ")";
    }
  }
  else attribute_widget_rep::handle_get_string (ev);
}

void
file_chooser_widget_rep::handle_destroy (destroy_event ev) {
  (void) ev;
  this << set_string ("return", "#f");
}

/******************************************************************************
* exported routines
******************************************************************************/

wk_widget
file_chooser_wk_widget (command cmd, string type, string magn) {
  return tm_new<file_chooser_widget_rep> (cmd, type, magn);
}
