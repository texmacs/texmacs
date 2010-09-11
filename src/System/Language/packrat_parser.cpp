
/******************************************************************************
* MODULE     : packrat_parser.cpp
* DESCRIPTION: efficient packrat parsing
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "packrat_parser.hpp"
#include "analyze.hpp"

/******************************************************************************
* Constructor
******************************************************************************/

packrat_parser_rep::packrat_parser_rep (packrat_grammar gr):
  grammar (gr->grammar),
  productions (gr->productions),
  current_tree (packrat_uninit),
  current_string (""),
  current_start (-1),
  current_end (-1),
  current_input (),
  current_cache (PACKRAT_UNDEFINED),
  current_production (packrat_uninit) {}

packrat_parser
make_packrat_parser (string lan, string in) {
  static string         last_lan= "";
  static string         last_in = "";
  static packrat_parser last_par;
  if (lan != last_lan || in != last_in) {
    packrat_grammar gr= make_packrat_grammar (lan);
    last_lan= lan;
    last_in = in;
    last_par= packrat_parser (gr, in);
  }
  return last_par;
}

packrat_parser
make_packrat_parser (string lan, tree in) {
  static string         last_lan= "";
  static tree           last_in = "";
  static packrat_parser last_par;
  if (lan != last_lan || in != last_in) {
    packrat_grammar gr= make_packrat_grammar (lan);
    last_lan= lan;
    last_in = in;
    last_par= packrat_parser (gr, in);
  }
  return last_par;
}

/******************************************************************************
* Setting up the input
******************************************************************************/

void
packrat_parser_rep::set_input (string s) {
  current_string= s;
  current_input = encode_tokens (s);
}

void
packrat_parser_rep::set_input (tree t) {
  current_string= "";
  add_input (t, path ());
  current_input= encode_tokens (current_string);
}

void
packrat_parser_rep::add_input (tree t, path p) {
  current_start (p)= N(current_string);
  if (is_atomic (t)) current_string << t->label;
  else if (is_func (t, CONCAT) || is_func (t, DOCUMENT)) {
    for (int i=0; i<N(t); i++) {
      add_input (t[i], p * i);
      if (is_func (t, DOCUMENT)) current_string << "\n";
    }
  }
  else {
    current_string << "<\\" << as_string (L(t)) << ">";
    for (int i=0; i<N(t); i++) {
      if (i != 0) current_string << "<|>";
      add_input (t[i], p * i);
    }
    current_string << "</>";
  }
  current_end (p)= N(current_string);
}

/******************************************************************************
* Decoding the end-position after parsing
******************************************************************************/

int
packrat_parser_rep::decode_string_position (C pos) {
  if (pos == PACKRAT_FAILED) return -1;
  int i=0, k=0;
  while (i<N(current_string) && k<pos) {
    tm_char_forwards (current_string, i);
    k++;
  }
  return i;
}

path
packrat_parser_rep::decode_tree_position (C pos) {
  int i= decode_string_position (pos);
  if (i < 0) return path (i);
  return get_path (current_tree, path (), i);
}

path
packrat_parser_rep::get_path (tree t, path p, int pos) {
  //cout << "Search " << pos << " in " << t << ", " << p << "\n";
  //cout << "Range " << current_start[p] << " -- " << current_end[p] << "\n";
  if (is_atomic (t)) return p * (pos - current_start[p]);
  else {
    for (int i=0; i<N(t); i++)
      if (pos >= current_start[p*i] && pos <= current_end[p*i])
	return get_path (t[i], p * i, pos);
    if (pos <= current_start[p]) return p * 0;
    if (pos >= current_end[p]) return p * 1;
    return p * 0;
  }
}

/******************************************************************************
* Packrat parsing
******************************************************************************/

C
packrat_parser_rep::parse (C sym, C pos) {
  D key= (((D) sym) << 32) + ((D) (sym^pos));
  C im = current_cache [key];
  if (im != PACKRAT_UNDEFINED) {
    //cout << "Cached " << sym << " at " << pos << " -> " << im << LF;
    return im;
  }
  current_cache (key)= PACKRAT_FAILED;
  //cout << "Parse " << sym << " at " << pos << INDENT << LF;
  if (sym >= PACKRAT_SYMBOLS) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      im= PACKRAT_FAILED;
      for (int i=1; i<N(inst); i++) {
	im= parse (inst[i], pos);
	if (im != PACKRAT_FAILED) break;
      }
      break;
    case PACKRAT_CONCAT:
      im= pos;
      for (int i=1; i<N(inst); i++) {
	im= parse (inst[i], im);
	if (im == PACKRAT_FAILED) break;
      }
      break;
    case PACKRAT_WHILE:
      im= pos;
      while (true) {
	C next= parse (inst[1], im);
	if (next == PACKRAT_FAILED || (next >= 0 && next <= im)) break;
	im= next;
      }
      break;
    case PACKRAT_REPEAT:
      im= parse (inst[1], pos);
      if (im != PACKRAT_FAILED)
	while (true) {
	  C next= parse (inst[1], im);
	  if (next == PACKRAT_FAILED || (next >= 0 && next <= im)) break;
	  im= next;
	}
      break;
    case PACKRAT_RANGE:
      if (pos < N (current_input) &&
	  current_input [pos] >= inst[1] &&
	  current_input [pos] <= inst[2])
	im= pos + 1;
      else im= PACKRAT_FAILED;
      break;
    case PACKRAT_NOT:
      if (parse (inst[1], pos) == PACKRAT_FAILED) im= pos;
      else im= PACKRAT_FAILED;
    case PACKRAT_UNKNOWN:
      im= PACKRAT_FAILED;
    default:
      im= parse (inst[0], pos);
      break;
    }
  }
  else {
    if (pos < N (current_input) && current_input[pos] == sym) im= pos + 1;
    else im= PACKRAT_FAILED;
  }
  current_cache (key)= im;
  //cout << UNINDENT << "Parsed " << sym << " at " << pos << " -> " << im << LF;
  //cout << "cache= " << current_cache << LF;
  return im;
}

/******************************************************************************
* User interface
******************************************************************************/

int
packrat_parse (string lan, string sym, string in) {
  packrat_parser par= make_packrat_parser (lan, in);
  C pos= par->parse (encode_symbol (compound ("symbol", sym)), 0);
  return par->decode_string_position (pos);
}

path
packrat_parse (string lan, string sym, tree in) {
  packrat_parser par= make_packrat_parser (lan, in);
  C pos= par->parse (encode_symbol (compound ("symbol", sym)), 0);
  return par->decode_tree_position (pos);
}
