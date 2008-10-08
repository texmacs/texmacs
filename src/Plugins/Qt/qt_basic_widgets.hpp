
/******************************************************************************
* MODULE     : qt_basic_widgets.h
* DESCRIPTION: Basic widgets
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

class qt_text_widget_rep : public qt_widget_rep {
public:
  string str, lan;
  color col;
  bool tsp;

  qt_text_widget_rep (string _s, color _col, bool _tsp, string _lan):
    str (_s), lan (_lan), col (_col), tsp (_tsp) {};  
  virtual QAction* as_qaction ();
};

class qt_image_widget_rep : public qt_widget_rep {
public:
  url image;
  
  qt_image_widget_rep (url _image): image(_image) {};
  virtual QAction *as_qaction();
};

class qt_balloon_widget_rep : public qt_widget_rep {
public:
  widget text, hint;
  
  qt_balloon_widget_rep (widget _text, widget _hint):
    text (_text), hint (_hint) {};
  virtual QAction* as_qaction ();
};


class qt_input_text_widget_rep : public qt_widget_rep {
public:
  command cmd;
  string type;
  array<string> def;
  string text;
  
  qt_input_text_widget_rep (command _cmd, string _type, array<string> _def):
    cmd (_cmd), type (_type), def (_def), text ("") {};
};
