
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
  properties (gr->properties),
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
    packrat_grammar gr= find_packrat_grammar (lan);
    last_lan= lan;
    last_in = copy (in);
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
    packrat_grammar gr= find_packrat_grammar (lan);
    last_lan= lan;
    last_in = copy (in);
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
  //cout << "Input " << current_string << "\n";
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
  if (is_nil (pos)) return -1;
  else if (is_atomic (t)) {
    if (pos->item < 0 || pos->item > N(t->label)) return -1;
    return current_start[p] + pos->item;
  }
  else {
    if (pos == path (0)) return current_start[p];
    if (pos == path (1)) return current_end[p];
    if (pos->item < 0 || pos->item > N(t) || is_nil (pos->next)) return -1;
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

bool
starts (tree t, string s) {
  return is_atomic (t) && starts (t->label, s);
}

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
  //cout << "Parse " << packrat_decode[sym] << " at " << pos << INDENT << LF;
  if (sym >= PACKRAT_TM_OPEN) {
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
    case PACKRAT_TM_OPEN:
      if (pos < N (current_input) &&
	  starts (packrat_decode[current_input[pos]], "<\\"))
	im= pos + 1;
      else im= PACKRAT_FAILED;
      break;
    case PACKRAT_TM_ANY:
      im= parse (PACKRAT_TM_OPEN, pos);
      if (im == PACKRAT_FAILED)
	im= parse (PACKRAT_TM_LEAF, pos);
      else {
	im= parse (PACKRAT_TM_ARGS, im);
	if (im != PACKRAT_FAILED)
	  im= parse (encode_token ("</>"), im);
      }
      break;
    case PACKRAT_TM_ARGS:
      im= parse (PACKRAT_TM_ANY, pos);
      while (im < N (current_input))
	if (current_input[im] != encode_token ("<|>")) break;
	else im= parse (PACKRAT_TM_ANY, im + 1);
      break;
    case PACKRAT_TM_LEAF:
      im= pos;
      while (im < N (current_input)) {
	tree t= packrat_decode[current_input[im]];
	if (starts (t, "<\\") || t == "<|>" || t == "</>") break;
	else im++;
      }
      break;
    case PACKRAT_TM_FAIL:
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
  //cout << UNINDENT << "Parsed " << packrat_decode[sym]
  //<< " at " << pos << " -> " << im << LF;
  //cout << UNINDENT << "Parsed " << sym << " at " << pos << " -> " << im << LF;
  //cout << "cache= " << current_cache << LF;
  return im;
}

/******************************************************************************
* Finding all enclosing structures at a given position
******************************************************************************/

bool
packrat_parser_rep::is_selectable (C sym) {
  tree t= packrat_decode[sym];
  if (!is_compound (t, "symbol", 1)) return false;
  string s= t[0]->label;
  return !ends (s, "-head") && !ends (s, "-tail");
}

void
packrat_parser_rep::context
  (C sym, C pos, C w1, C w2, array<C>& kind, array<C>& begin, array<C>& end)
{
  C next= parse (sym, pos);
  if (next < 0 || pos > w1 || next < w2) return;

  int n= N(kind);
  if (n >= 1 && begin[n-1] == pos && end[n-1] == next) {
    if (is_selectable (sym) || !is_selectable (kind[n-1]))
      kind[n-1]= sym;
  }
  else {
    kind  << sym;
    begin << pos;
    end   << next;
  }

  if (sym >= PACKRAT_TM_OPEN) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED)
	  context (inst[i], pos, w1, w2, kind, begin, end);
      break;
    case PACKRAT_CONCAT:
      for (int i=1; i<N(inst); i++) {
	next= parse (inst[i], pos);
	if (next == PACKRAT_FAILED) break;
	if (pos <= w1 && w2 <= next)
	  context (inst[i], pos, w1, w2, kind, begin, end);
	if (next > w2) break;
	pos= next;
      }
      break;
    case PACKRAT_WHILE:
    case PACKRAT_REPEAT:
      while (true) {
	C next= parse (inst[1], pos);
	if (next == PACKRAT_FAILED) break;
	if (pos <= w1 && w2 <= next)
	  context (inst[1], pos, w1, w2, kind, begin, end);
	if (next > w2) break;
	pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
    case PACKRAT_TM_OPEN:
    case PACKRAT_TM_ANY:
    case PACKRAT_TM_ARGS:
    case PACKRAT_TM_LEAF:
    case PACKRAT_TM_FAIL:
      break;
    default:
      context (inst[0], pos, w1, w2, kind, begin, end);
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
    if (is_selectable (kind[i]))
      if (N(new_kind) == 0 ||
	  new_kind [n-1] != kind[i] ||
	  (new_begin[n-1] != begin[i] && new_end[n-1] != end[i])) {
	new_kind  << kind[i];
	new_begin << begin[i];
	new_end   << end[i];
      }
  }
  kind = new_kind;
  begin= new_begin;
  end  = new_end;
}

/******************************************************************************
* Syntax highlighting
******************************************************************************/

void
packrat_parser_rep::highlight (tree t, path p1, path p2, string col) {
  if (p1 == p2);
  else if (is_atomic (t)) {
    string s= t->label;
    ASSERT (is_atom (p1) && is_atom (p2), "invalid selection");
    ASSERT (0 <= p1->item && p1->item <= p2->item && p2->item <= N(s),
	    "invalid selection");
    cout << "highlight " << col << ": " << s (p1->item, p2->item) << "\n";
  }
  else if (N(t) == 0);
  else {
    ASSERT (!is_nil (p1) && !is_nil (p2) && p1->item <= p2->item,
	    "invalid selection");
    if (p1 == path (0)) p1= path (0, 0);
    if (p2 == path (1)) p2= path (N(t) - 1, right_index (t[N(t) -1]));
    for (int i= max (0, p1->item); i <= min (p2->item, N(t)-1); i++) {
      path q1= (i == p1->item? p1->next: path (0));
      path q2= (i == p2->item? p2->next: path (right_index (t[i])));
      highlight (t[i], q1, q2, col);
    }
  }
}

void
packrat_parser_rep::highlight (C sym, C pos) {
  C next= parse (sym, pos);
  if (next < 0) return;
  tree symt= packrat_decode[sym];
  if (is_compound (symt, "symbol", 1) && is_atomic (symt[0])) {
    tree key= tuple (symt[0]->label, "highlight");
    if (properties->contains (key)) {
      string col= properties [key];
      path start= decode_tree_position (pos);
      path end= decode_tree_position (next);
      highlight (current_tree, start, end, col);
    }
  }

  if (sym >= PACKRAT_TM_OPEN) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED)
	  highlight (inst[i], pos);
      break;
    case PACKRAT_CONCAT:
      for (int i=1; i<N(inst); i++) {
	next= parse (inst[i], pos);
	highlight (inst[i], pos);
	pos= next;
      }
      break;
    case PACKRAT_WHILE:
    case PACKRAT_REPEAT:
      while (true) {
	C next= parse (inst[1], pos);
	if (next == PACKRAT_FAILED) break;
	highlight (inst[1], pos);
	pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
    case PACKRAT_TM_OPEN:
    case PACKRAT_TM_ANY:
    case PACKRAT_TM_ARGS:
    case PACKRAT_TM_LEAF:
    case PACKRAT_TM_FAIL:
      break;
    default:
      highlight (inst[0], pos);
      break;
    }
  }
}

/******************************************************************************
* User interface
******************************************************************************/

path
packrat_parse (string lan, string sym, tree in) {
  packrat_parser par= make_packrat_parser (lan, in);
  C pos= par->parse (encode_symbol (compound ("symbol", sym)), 0);
  return par->decode_tree_position (pos);
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
  par->context (sym, 0, pos-1, pos+1, kind, begin, end);
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

bool
packrat_select (string lan, string s, tree in,
		path& p1, path& p2, int mode)
{
  //cout << "Enlarge " << p1 << " -- " << p2 << " in " << in
  //<< " (" << lan << ", " << s << ")" << LF;
  packrat_parser par= make_packrat_parser (lan, in);
  C sym = encode_symbol (compound ("symbol", s));
  C pos1= par->encode_tree_position (p1);
  C pos2= par->encode_tree_position (p2);
  //cout << "Encoded " << pos1 << " -- " << pos2
  //<< " in " << par->current_string << LF;
  if (par->parse (sym, 0) != N(par->current_input)) return false;
  if (pos1 == PACKRAT_FAILED || pos2 == PACKRAT_FAILED) return false;
  array<C> kind, begin, end;
  C pos0= pos1;
  if ((mode == 1 && pos1 == pos2) || mode == 2) pos0= max (pos1 - 1, 0);
  par->context (sym, 0, pos0, pos2, kind, begin, end);
  par->compress (kind, begin, end);
  /*
  for (int i=0; i<N(kind); i++)
    cout << i << ":\t"
	 << par->decode_tree_position (begin[i]) << "\t"
	 << par->decode_tree_position (end[i]) << "\t"
	 << packrat_decode[kind[i]] << LF;
  */
  int n= N(kind);
  if (n == 0) return false;
  if (mode == 1) {
    if (pos1 == begin[n-1] && pos2 == end[n-1]) n--;
    if (n == 0) return false;
  }
  else if (mode == 2 && n > 1) n--;
  p1= par->decode_tree_position (begin[n-1]);
  p2= par->decode_tree_position (end[n-1]);
  //cout << "Selected " << packrat_decode[kind[n-1]] << LF;
  return true;
}

void
packrat_highlight (string lan, string s, tree in) {
  packrat_parser par= make_packrat_parser (lan, in);
  C sym = encode_symbol (compound ("symbol", s));
  if (par->parse (sym, 0) == N(par->current_input))
    par->highlight (sym, 0);
}
