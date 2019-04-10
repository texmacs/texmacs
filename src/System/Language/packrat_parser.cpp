
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
#include "drd_std.hpp"
#include "language.hpp" //(en|de)code_color

extern tree the_et;
bool packrat_invalid_colors= false;

/******************************************************************************
* Constructor
******************************************************************************/

packrat_parser_rep::packrat_parser_rep (packrat_grammar gr):
  lan_name(gr->lan_name),
  grammar (gr->grammar),
  productions (gr->productions),
  properties (gr->properties),
  current_tree (packrat_uninit),
  current_string (""),
  current_start (-1),
  current_end (-1),
  current_path_pos (-1),
  current_pos_path (-1),
  current_cursor (-1),
  current_input (),
  current_cache (PACKRAT_UNDEFINED),
  current_production (packrat_uninit) {}

packrat_parser
make_packrat_parser (string lan, tree in) {
  static string         last_lan   = "";
  static tree           last_in    = "";
  static packrat_parser last_par;
  if (lan != last_lan || in != last_in) {
    packrat_grammar gr= find_packrat_grammar (lan);
    last_lan   = lan;
    last_in    = copy (in);
    last_par   = packrat_parser (gr, last_in);
  }
  return last_par;
}

packrat_parser
make_packrat_parser (string lan, tree in, path in_pos) {
  static string         last_lan   = "";
  static tree           last_in    = "";
  static path           last_in_pos= path ();
  static packrat_parser last_par;
  if (lan != last_lan || in != last_in || in_pos != last_in_pos) {
    packrat_grammar gr= find_packrat_grammar (lan);
    last_lan   = lan;
    last_in    = copy (in);
    last_in_pos= copy (last_in_pos);
    last_par   = packrat_parser (gr, last_in, in_pos);
  }
  return last_par;
}

/******************************************************************************
* Setting up the input
******************************************************************************/

void
packrat_parser_rep::set_input (tree t) {
  current_string= "";
  current_tree  = t;
  serialize (t, path ());
  if (DEBUG_FLATTEN)
    debug_packrat << "Input " << current_string << "\n";
  current_input= encode_tokens (current_string);
}

void
packrat_parser_rep::set_cursor (path p) {
  if (is_nil (p)) current_cursor= -1;
  else current_cursor= encode_tree_position (p);
  //cout << current_input << ", " << current_cursor << "\n";
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
  if (is_nil (pos) || !current_start->contains (p)) return -1;
  else if (is_atomic (t)) {
    if (current_path_pos->contains (p * pos))
      return current_path_pos[p * pos];
    else if (pos->item < 0 || pos->item > N(t->label)) return -1;
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
  if (is_atomic (t)) {
    if (current_pos_path->contains (pos))
      return current_pos_path[pos];
    else return p * (pos - current_start[p]);
  }
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
  if (DEBUG_PACKRAT)
    debug_packrat << "Parse " << packrat_decode[sym]
                  << " at " << pos << INDENT << LF;
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
    case PACKRAT_EXCEPT:
      im= parse (inst[1], pos);
      if (im != PACKRAT_FAILED)
	if (parse (inst[2], pos) != PACKRAT_FAILED)
	  im= PACKRAT_FAILED;
      break;
    case PACKRAT_TM_OPEN:
      if (pos < N (current_input) &&
	  starts (packrat_decode[current_input[pos]], "<\\"))
	im= pos + 1;
      else im= PACKRAT_FAILED;
      break;
    case PACKRAT_TM_ANY:
      im= pos;
      while (true) {
	C old= im;
	im= parse (PACKRAT_TM_OPEN, old);
	if (im == PACKRAT_FAILED)
	  im= parse (PACKRAT_TM_LEAF, old);
	else {
	  im= parse (PACKRAT_TM_ARGS, im);
	  if (im != PACKRAT_FAILED)
	    im= parse (encode_token ("</>"), im);
	}
	if (old == im) break;
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
    case PACKRAT_TM_CHAR:
      if (pos >= N (current_input)) im= PACKRAT_FAILED;
      else {
	tree t= packrat_decode[current_input[pos]];
	if (starts (t, "<\\") || t == "<|>" || t == "</>") im= PACKRAT_FAILED;
	else im= pos + 1;
      }
      break;
    case PACKRAT_TM_CURSOR:
      if (pos == current_cursor) im= pos;
      else im= PACKRAT_FAILED;
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
  if (DEBUG_PACKRAT)
    debug_packrat << UNINDENT << "Parsed " << packrat_decode[sym]
                  << " at " << pos << " -> " << im << LF;
  return im;
}

/******************************************************************************
* Inspecting the parse tree
******************************************************************************/

void
packrat_parser_rep::inspect (C sym, C pos, array<C>& syms, array<C>& poss) {
  syms= array<C> ();
  poss= array<C> ();
  C next= parse (sym, pos);
  if (next == PACKRAT_FAILED) return;
  if (sym >= PACKRAT_TM_OPEN) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED) {
	  inspect (inst[i], pos, syms, poss);
	  break;
	}
      break;
    case PACKRAT_CONCAT:
      for (int i=1; i<N(inst); i++) {
	next= parse (inst[i], pos);
	if (next == PACKRAT_FAILED) break;
        syms << inst[i];
        poss << pos;
	pos= next;
      }
      break;
    case PACKRAT_WHILE:
    case PACKRAT_REPEAT:
      while (true) {
        C next= parse (inst[1], pos);
        if (next == PACKRAT_FAILED) break;
        syms << inst[1];
        poss << pos;
        pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
      break;
    case PACKRAT_EXCEPT:
      inspect (inst[1], pos, syms, poss);
      break;
    case PACKRAT_TM_OPEN:
    case PACKRAT_TM_ANY:
    case PACKRAT_TM_ARGS:
    case PACKRAT_TM_LEAF:
    case PACKRAT_TM_CHAR:
    case PACKRAT_TM_CURSOR:
    case PACKRAT_TM_FAIL:
      break;
    default:
      inspect (inst[0], pos, syms, poss);
      break;
    }
  }
}

bool
packrat_parser_rep::is_left_recursive (C sym) {
  if (sym < PACKRAT_TM_OPEN) return false;
  array<C> inst= grammar [sym];
  if (inst[0] != PACKRAT_CONCAT || N(inst) != 3) return false;
  if (inst[1] < PACKRAT_TM_OPEN) return false;
  tree t= packrat_decode[inst[1]];
  return is_compound (t, "symbol", 1) && ends (t[0]->label, "-head");
}

bool
packrat_parser_rep::is_associative (C sym) {
  static C prop= encode_symbol (compound ("property", "associativity"));
  D key = (((D) prop) << 32) + ((D) (sym ^ prop));
  if (!properties->contains (key)) return false;
  return properties[key] == "associative";
}

bool
packrat_parser_rep::is_anti_associative (C sym) {
  static C prop= encode_symbol (compound ("property", "associativity"));
  D key = (((D) prop) << 32) + ((D) (sym ^ prop));
  if (!properties->contains (key)) return false;
  return properties[key] == "anti-associative";
}

bool
packrat_parser_rep::is_list_like (C sym) {
  (void) sym;
  return false;
}

bool
packrat_parser_rep::is_selectable (C sym) {
  tree t= packrat_decode[sym];
  if (is_compound (t, "partial", 1)) return true;
  if (!is_compound (t, "symbol", 1)) return false;
  string s= t[0]->label;
  return !ends (s, "-head") && !ends (s, "-tail");
}

/******************************************************************************
* Finding all enclosing structures at a given position
******************************************************************************/

void
packrat_parser_rep::context
  (C sym, C pos, C w1, C w2, int mode,
   array<C>& kind, array<C>& begin, array<C>& end)
{
  C next= parse (sym, pos);
  if (next < 0 || pos > w1 || next < w2) return;

  if (mode == 2 && (pos == w1 || next == w2)) {
    static C prop= encode_symbol (compound ("property", "operator"));
    D key = (((D) prop) << 32) + ((D) (sym ^ prop));
    if (properties->contains (key)) return;
  }

  if (true) {
    static C sel_prop= encode_symbol (compound ("property", "selectable"));
    static C foc_prop= encode_symbol (compound ("property", "focus"));
    D sel_key = (((D) sel_prop) << 32) + ((D) (sym ^ sel_prop));
    D foc_key = (((D) foc_prop) << 32) + ((D) (sym ^ foc_prop));
    if (properties->contains (sel_key) &&
        properties[sel_key] == "inside");
    else if (properties->contains (foc_key) &&
             properties[foc_key] == "disallow" &&
             mode == 2);
    else {
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
    }
  }

  if (mode >= 0) {
    static C prop= encode_symbol (compound ("property", "atomic"));
    D key = (((D) prop) << 32) + ((D) (sym ^ prop));
    if (properties->contains (key)) return;
  }

  if (is_left_recursive (sym) && mode == 0) {
    array<C> inst= grammar [sym];
    C before= pos;
    C middle= parse (inst[1], before);
    if (middle == PACKRAT_FAILED) return;
    C after = parse (inst[2], middle);
    if (after == PACKRAT_FAILED) return;
    array<C> csym;
    array<C> cpos;
    inspect (inst[2], middle, csym, cpos);
    csym= append (inst[1], csym);
    cpos= append (before, cpos);
    cpos << after;
    int i1, i2;
    for (i1=0; i1<N(csym); i1++)
      if (cpos[i1+1] > w1) break;
    for (i2=i1; i2<N(csym); i2++)
      if (cpos[i2+1] >= w2) break;
    if (i1 == i2) {
      int i, n= N(kind);
      context (csym[i1], cpos[i1], w1, w2, mode, kind, begin, end);
      for (i=n; i<N(kind); i++)
        if (is_selectable (kind[i]))
          return;
      kind  -> resize (n);
      begin -> resize (n);
      end   -> resize (n);
    }
    C alt_start= -1;
    while (i1 > 0) {
      array<C> ccsym;
      array<C> ccpos;
      inspect (csym[i1], cpos[i1], ccsym, ccpos);
      if (N(ccsym)>1 && is_associative (ccsym[0])) {
        if (w1 >= ccpos[1]) alt_start= ccpos[1];
        break;
      }
      if (N(ccsym)>0 && is_anti_associative (ccsym[0])) break;
      i1--;
    }
    tree sel= compound ("partial", packrat_decode[sym]);
    kind  << encode_symbol (sel);
    begin << (alt_start<0? cpos[i1]: alt_start);
    end   << cpos[i2+1];
    return;
  }

  if (sym >= PACKRAT_TM_OPEN) {
    array<C> inst= grammar [sym];
    //cout << "Context " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED) {
	  context (inst[i], pos, w1, w2, mode, kind, begin, end);
	  break;
	}
      break;
    case PACKRAT_CONCAT:
      for (int i=1; i<N(inst); i++) {
	next= parse (inst[i], pos);
	if (next == PACKRAT_FAILED) break;
	if (pos <= w1 && w2 <= next)
	  context (inst[i], pos, w1, w2, mode, kind, begin, end);
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
	  context (inst[1], pos, w1, w2, mode, kind, begin, end);
	if (next > w2) break;
	pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
      break;
    case PACKRAT_EXCEPT:
      context (inst[1], pos, w1, w2, mode, kind, begin, end);
      break;
    case PACKRAT_TM_OPEN:
    case PACKRAT_TM_ANY:
    case PACKRAT_TM_ARGS:
    case PACKRAT_TM_LEAF:
    case PACKRAT_TM_CHAR:
    case PACKRAT_TM_CURSOR:
    case PACKRAT_TM_FAIL:
      break;
    default:
      context (inst[0], pos, w1, w2, mode, kind, begin, end);
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
packrat_parser_rep::highlight (tree t, path tp, path p1, path p2, int col) {
  if (p1 == p2);
  else if (is_atomic (t)) {
    string s= t->label;
    ASSERT (is_atom (p1) && is_atom (p2), "invalid selection");
    ASSERT (0 <= p1->item && p1->item <= p2->item && p2->item <= N(s),
	    "invalid selection");
    attach_highlight (t, current_hl_lan, col, p1->item, p2->item);
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
      highlight (t[i], tp * i, q1, q2, col);
    }
  }
}

void
packrat_parser_rep::highlight (C sym, C pos) {
  C next= parse (sym, pos);
  if (next < 0) return;
  if (sym >= PACKRAT_SYMBOLS) {
    static C prop= encode_symbol (compound ("property", "highlight"));
    D key = (((D) prop) << 32) + ((D) (sym ^ prop));
    if (properties->contains (key)) {
      int  col  = encode_color (properties [key]);
      path start= decode_tree_position (pos);
      path end  = decode_tree_position (next);
      highlight (current_tree, path (), start, end, col);
      static C prop= encode_symbol (compound ("property", "transparent"));
      D key = (((D) prop) << 32) + ((D) (sym ^ prop));
      if (!properties->contains (key)) return;
    }
  }

  if (sym >= PACKRAT_TM_OPEN) {
    array<C> inst= grammar [sym];
    //cout << "Parse " << inst << " at " << pos << LF;
    switch (inst[0]) {
    case PACKRAT_OR:
      for (int i=1; i<N(inst); i++)
	if (parse (inst[i], pos) != PACKRAT_FAILED) {
	  highlight (inst[i], pos);
	  break;
	}
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
	if (next == pos) break;
	pos= next;
      }
      break;
    case PACKRAT_RANGE:
    case PACKRAT_NOT:
      break;
    case PACKRAT_EXCEPT:
      highlight (inst[1], pos);
      break;      
    case PACKRAT_TM_OPEN:
    case PACKRAT_TM_ANY:
    case PACKRAT_TM_ARGS:
    case PACKRAT_TM_LEAF:
    case PACKRAT_TM_CHAR:
    case PACKRAT_TM_CURSOR:
    case PACKRAT_TM_FAIL:
      break;
    default:
      highlight (inst[0], pos);
      break;
    }
  }
}

/******************************************************************************
* Memoized and accelerated highlighting
******************************************************************************/

static bool
empty_line (tree t) {
  if (!is_atomic (t)) return false;
  string s= t->label;
  for (int i=0; i<N(s); i++)
    if (s[i] != ' ') return false;
  return true;
}

static bool
consistent_portion (tree t, int begin, int end) {
  int level= 0;
  for (int i=begin; i<end; i++)
    if (is_atomic (t[i])) {
      string s= t[i]->label;
      for (int j=0; j<N(s); j++)
	switch (s[j]) {
	case '(': level++; break;
	case ')': if (level <= 0) return false; level--; break;
	case '[': level++; break;
	case ']': if (level <= 0) return false; level--; break;
	case '{': level++; break;
	case '}': if (level <= 0) return false; level--; break;
	default : break;
	}
    }
  return level == 0;
}

static void
consistent_enlargement (tree t, int& begin, int& end) {
  while (begin > 0 || end < N(t)) {
    while (begin > 0    && !empty_line (t[begin-1])) begin--;
    while (end   < N(t) && !empty_line (t[end    ])) end++;
    if (consistent_portion (t, begin, end)) return;
    //cout << "Inconsistent " << begin << " -- " << end << "\n";
    begin= max (0   , begin - max (end - begin, 1));
    end  = min (N(t), end   + max (end - begin, 1));
    //cout << "  Try " << begin << " -- " << end << "\n";
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

bool
packrat_correct (string lan, string sym, tree in) {
  packrat_parser par= make_packrat_parser (lan, in);
  C pos= par->parse (encode_symbol (compound ("symbol", sym)), 0);
  return pos == N(par->current_input);
}

bool
packrat_available_path (string lan, tree in, path in_p) {
  packrat_parser par= make_packrat_parser (lan, in);
  return par->current_start->contains (in_p);
}

object
packrat_context (string lan, string s, tree in, path in_pos) {
  //cout << "Context " << in << " at " << in_pos
  //     << " (" << lan << ", " << s << ")" << LF;
  packrat_parser par= make_packrat_parser (lan, in);
  C sym= encode_symbol (compound ("symbol", s));
  if (par->parse (sym, 0) != N(par->current_input))
    par= make_packrat_parser (lan, in, in_pos);
  C pos= par->encode_tree_position (in_pos);
  if (pos == PACKRAT_FAILED) return object (false);
  array<C> kind, begin, end;
  par->context (sym, 0, pos-1, pos+1, 0, kind, begin, end);
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
packrat_select (string lan, string s, tree in, path in_pos,
		path& p1, path& p2, int mode)
{
  // mode= 0: genuine semantic selection
  // mode= 1: strictly larger selection for select_enlarge
  // mode= 2: determine environment rectangles
  if (path_less (p2, p1))
    return packrat_select (lan, s, in, in_pos, p2, p1, mode);
  //cout << "Enlarge " << p1 << " -- " << p2 << " in " << in
  //<< " (" << lan << ", " << s << ")" << LF;
  packrat_parser par= make_packrat_parser (lan, in);
  C sym = encode_symbol (compound ("symbol", s));
  if (par->parse (sym, 0) != N(par->current_input))
    par= make_packrat_parser (lan, in, in_pos);
  C pos1= par->encode_tree_position (p1);
  C pos2= par->encode_tree_position (p2);
  //cout << "Encoded " << pos1 << " -- " << pos2
  //     << " in " << par->current_string << LF;
  if (par->parse (sym, 0) != N(par->current_input)) return false;
  if (pos1 == PACKRAT_FAILED || pos2 == PACKRAT_FAILED) return false;
  array<C> kind, begin, end;
  C pos0= pos1;
  if ((mode == 1 && pos1 == pos2) || mode == 2) pos0= max (pos1 - 1, 0);
  par->context (sym, 0, pos0, pos2, mode, kind, begin, end);
  //for (int i=0; i<N(kind); i++)
  //  cout << i << ":\t"
  //       << par->decode_tree_position (begin[i]) << "\t"
  //       << par->decode_tree_position (end[i]) << "\t"
  //       << packrat_decode[kind[i]] << LF;
  par->compress (kind, begin, end);
  int n= N(kind);
  if (n == 0) return false;
  if (mode == 1) {
    if (pos1 == begin[n-1] && pos2 == end[n-1]) n--;
    if (n == 0) return false;
  }
  p1= par->decode_tree_position (begin[n-1]);
  p2= par->decode_tree_position (end[n-1]);
  //cout << "Selected " << packrat_decode[kind[n-1]] << LF;
  return true;
}

void
packrat_highlight_subtree (string lan, string s, tree in) {
  //cout << "Highlight " << lan << ", " << s << " in " << in << "\n";
  int hl_lan= packrat_abbreviation (lan, s);
  if (hl_lan == 0) return;
  packrat_parser par= make_packrat_parser (lan, in);
  C sym = encode_symbol (compound ("symbol", s));
  if (par->parse (sym, 0) == N(par->current_input)) {
    par->current_hl_lan= hl_lan;
    par->highlight (sym, 0);
  }
}

void
packrat_highlight (string lan, string s, tree in) {
  int hl_lan= packrat_abbreviation (lan, s);
  if (hl_lan == 0) return;
  //cout << "Highlight " << in << "\n";
  if (is_func (in, DOCUMENT)) {
    int i, begin, end;
    for (begin=0; begin<N(in); begin++)
      if (!has_highlight (in[begin], hl_lan))
	break;
    for (end=N(in)-1; end>begin; end--)
      if (!has_highlight (in[end-1], hl_lan))
	break;
    consistent_enlargement (in, begin, end);    
    for (i=begin; i<end; i++)
      detach_highlight (in[i], hl_lan);
    attach_highlight (in, hl_lan);
    packrat_highlight_subtree (lan, s, in (begin, end));
  }
  else {
    if (is_compound (in))
      for (int i=0; i<N(in); i++)
	detach_highlight (in[i], hl_lan);
    attach_highlight (in, hl_lan);
    packrat_highlight_subtree (lan, s, in);
  }
}
