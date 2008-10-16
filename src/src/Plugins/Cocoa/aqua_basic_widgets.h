
/******************************************************************************
* MODULE     : aqua_basic_widgets.h
* DESCRIPTION: Basic widgets
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

class aqua_text_widget_rep : public aqua_widget_rep {
public:
  string str, lan;
  color col;
  bool tsp;
  
  aqua_text_widget_rep(string _s, color _col, bool _tsp, string _lan) 
  : str(_s), col(_col), tsp(_tsp), lan(_lan) {};  

  virtual TMMenuItem *as_menuitem();

};

class aqua_image_widget_rep : public aqua_widget_rep {
public:
  url image;
  
  aqua_image_widget_rep(url _image) : image(_image) {};
  virtual TMMenuItem *as_menuitem();
};

class aqua_balloon_widget_rep : public aqua_widget_rep {
public:
  widget text, hint;
  
  aqua_balloon_widget_rep(widget _text, widget _hint) : text(_text), hint(_hint) {};
  virtual TMMenuItem *as_menuitem();
};


class aqua_input_text_widget_rep : public aqua_widget_rep {
public:
  command cmd;
  string type;
  array<string> def;
  string text;
  
  aqua_input_text_widget_rep(command _cmd, string _type, array<string> _def)
  : cmd(_cmd), type(_type), def(_def), text("") {};
};
