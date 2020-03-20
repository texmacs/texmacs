
/******************************************************************************
* MODULE     : parser.hpp
* DESCRIPTION: parser headers
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PARSER_H
#define PARSER_H

#include "string.hpp"

class parser_rep {
public:
  bool parse (string s, int& pos) {
    if (!can_parse (s, pos)) {
      return false;
    }

    int opos= pos;
    do_parse (s, pos);

    if (pos > opos) {
      // Expressive logs for parser with start and end marked
      // Commented by default for performance
      // cout << get_parser_name() << " ended" << LF
      //      << s << LF
      //      << string(' ', opos) << "^" << LF
      //      << string(' ', pos) << "^" << LF;
      return true;
    } else {
      debug_packrat << "Illegal status for " << get_parser_name() << LF
                    << s << LF
                    << string(' ', opos) << "^" << LF
                    << string(' ', pos) << "^" << LF;
      return false;
    }
  }

protected:
  /**
   * @return the name of the parser
   */
  virtual string get_parser_name () { return ""; }
  /** Parse a string at the position and advance the position
   * @param s     the string to parse
   * @param[out]  the position to parse
   */
  virtual void do_parse (string s, int& pos) { (void) s; (void) pos; }

  /** Test if a string is parsable at the position
   * @param s     the string to parse
   * @param pos   the position to parse
   */
  virtual bool can_parse (string s, int pos) { return pos < N(s); }
};

#endif
