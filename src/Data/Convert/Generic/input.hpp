
/******************************************************************************
* MODULE     : input.hpp
* DESCRIPTION: Generic TeXmacs input
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef INPUT_H
#define INPUT_H
#include "path.hpp"

class texmacs_input;
struct texmacs_input_rep: concrete_struct {
  string type;                  // default value for channel below
  int    status;                // status of parser
  string buf;                   // input buffer
  string format;                // current input format
  int    mode;                  // corresponding input mode
  string channel;               // current output channel
  tree   stack;                 // stack for nested blocks
  hashmap<string,tree> docs;    // output for each channel

  texmacs_input_rep (string type);
  int  get_mode (string s);
  void begin_mode (string s);
  void begin_channel (string s);
  void end ();
  bool put (char c);
  void bof ();
  void eof ();
  void write (tree t);
  tree get (string channel);

  void flush (bool force= false);
  void verbatim_flush (bool force= false);
  void scheme_flush (bool force= false);
  void latex_flush (bool force= false);
  void html_flush (bool force= false);
  void ps_flush (bool force= false);
  void ispell_flush (bool force= false);
  void channel_flush (bool force= false);
  void command_flush (bool force= false);
  void xformat_flush (bool force= false);
};

class texmacs_input {
  CONCRETE(texmacs_input);
  texmacs_input (string type);
};
CONCRETE_CODE(texmacs_input);

#endif // defined INPUT_H
