
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

void
packrat_parser_rep::set_input (tree t) {
  current_string= "";
  current_tree  = t;
  add_input (t, path ());
  //cout << "Input= " << current_string << "\n";
  current_input= encode_tokens (current_string);
}

/******************************************************************************
* Encoding and decoding of cursor positions in the input
******************************************************************************/

C
packrat_parser_rep::encode_string_position (int i) {
  if (i < 0) return PACKRAT_FAILED;
  int j=0;
  C k=0;
  while (j<i && j<N(current_string)) {
    tm_char_forwards (current_string, j);
    k++;
  }
  return k;
}

int
packrat_parser_rep::encode_path (tree t, path p, path pos) {
  //cout << "Search " << pos << " in " << t << ", " << p << "\n";
  //cout << "Range " << current_start[p] << " -- " << current_end[p] << "\n";
  if (is_nil (pos) || pos->item < 0 || pos->item > right_index (t))
    return -1;
  else if (is_atomic (t))
    return current_start[p] + pos->item;
  else {
    if (pos == path (0)) return current_start[p];
    if (pos == path (1)) return current_end[p];
    return encode_path (t[pos->item], p * pos->item, pos->next);
  }
}

C
packrat_parser_rep::encode_tree_position (path p) {
  if (is_nil (p) || p->item < 0) return PACKRAT_FAILED;
  int i= encode_path (current_tree, path (), p);
  return encode_string_position (i);
}

int
packrat_parser_rep::decode_string_position (C pos) {
  //cout << "Decode " << pos << "\n";
  if (pos == PACKRAT_FAILED) return -1;
  int i=0;
  C k=0;
  while (i<N(current_string) && k<pos) {
    tm_char_forwards (current_string, i);
    k++;
  }
  return i;
}

path
packrat_parser_rep::decode_path (tree t, path p, int pos) {
  //cout << "Search " << pos << " in " << t << ", " << p << "\n";
  //cout << "Range " << current_start[p] << " -- " << current_end[p] << "\n";
  if (is_atomic (t)) return p * (pos - current_start[p]);
  else {
    for (int i=0; i<N(t); i++)
      if (pos >= current_start[p*i] && pos <= current_end[p*i])
	return decode_path (t[i], p * i, pos);
    if (pos <= current_start[p]) return p * 0;
    if (pos >= current_end[p]) return p * 1;
    return p * 0;
  }
}

path
packrat_parser_rep::decode_tree_position (C pos) {
  int i= decode_string_position (pos);
  if (i < 0) return path (i);
  return decode_path (current_tree, path (), i);
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
      break;
    case PACKRAT_UNKNOWN:
      im= PACKRAT_FAILED;
      break;
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
* Finding all enclosing structures at a given position
******************************************************************************/

void
packrat_parser_rep::context
  (C sym, C pos, C where, array<C>& kind, array<C>& begin, array<C>& end)
{
  C next= parse (sym, pos);
  if (next < 0 || pos >= where || next <= where) return;

  int n= N(kind);
  if (n >= 1 && begin[n-1] == pos && end[n-1] == next) kind[n-1]= sym;
  else {
    kind  << sym;
    begin << pos;
    end   << next;
  }

  if (sym >= PACKRAT_SYMBOLS) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED)
	  context (inst[i], pos, where, kind, begin, end);
      break;
    case PACKRAT_CONCAT:
      for (int i=1; i<N(inst); i++) {
	next= parse (inst[i], pos);
	if (next == PACKRAT_FAILED) break;
	if (pos < where || where < next)
	  context (inst[i], pos, where, kind, begin, end);
	if (next >= where) break;
	pos= next;
      }
      break;
    case PACKRAT_WHILE:
    case PACKRAT_REPEAT:
      while (true) {
	C next= parse (inst[1], pos);
	if (next == PACKRAT_FAILED) break;
	if (pos < where || where < next)
	  context (inst[1], pos, where, kind, begin, end);
	if (next >= where) break;
	pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
    case PACKRAT_UNKNOWN:
      break;
    default:
      context (inst[0], pos, where, kind, begin, end);
      break;
    }
  }
}

void
packrat_parser_rep::compress
  (array<C>& kind, array<C>& begin, array<C>& end)
{
  array<C> new_kind, new_begin, new_end;
  for (int i=0; i<N(kind); i++) {
    int n= N(new_kind);
    if (is_compound (packrat_decode[kind[i]], "symbol", 1)) {
      if (N(new_kind) == 0 ||
	  new_kind [n-1] != kind[i] ||
	  (new_begin[n-1] != begin[i] && new_end[n-1] != end[i])) {
	new_kind  << kind[i];
	new_begin << begin[i];
	new_end   << end[i];
      }
    }
  }
  kind = new_kind;
  begin= new_begin;
  end  = new_end;
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

object
packrat_context (string lan, string s, string in, int in_pos) {
  packrat_parser par= make_packrat_parser (lan, in);
  C sym= encode_symbol (compound ("symbol", s));
  C pos= par->encode_string_position (in_pos);
  array<C> kind, begin, end;
  par->context (sym, 0, pos, kind, begin, end);
  par->compress (kind, begin, end);
  object ret= null_object ();
  for (int i=0; i<N(kind); i++) {
    object x1 (symbol_object (packrat_decode[kind[i]][0]->label));
    object x2 (par->decode_string_position (begin[i]));
    object x3 (par->decode_string_position (end[i]));
    ret= cons (list_object (x1, x2, x3), ret);
  }
  return ret;
}

object
packrat_context (string lan, string s, tree in, path in_pos) {
  //cout << "Context " << in << " at " << in_pos
  //<< " (" << lan << ", " << s << ")" << LF;
  packrat_parser par= make_packrat_parser (lan, in);
  C sym= encode_symbol (compound ("symbol", s));
  C pos= par->encode_tree_position (in_pos);
  if (pos == PACKRAT_FAILED) return object (false);
  array<C> kind, begin, end;
  par->context (sym, 0, pos, kind, begin, end);
  par->compress (kind, begin, end);
  object ret= null_object ();
  for (int i=0; i<N(kind); i++) {
    object x1 (symbol_object (packrat_decode[kind[i]][0]->label));
    object x2 (par->decode_tree_position (begin[i]));
    object x3 (par->decode_tree_position (end[i]));
    ret= cons (list_object (x1, x2, x3), ret);
  }
  return ret;
}
