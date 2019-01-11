
/******************************************************************************
* MODULE     : packrat_serializer.cpp
* DESCRIPTION: serializing trees as strings
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "packrat_parser.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Useful subroutines
******************************************************************************/

static path
as_path (tree t) {
  ASSERT (is_tuple (t), "invalid path");
  path p;
  int i, n= N(t);
  for (i=n-1; i>=0; i--)
    p= path (as_int (t[i]), p);
  return p;
}

/******************************************************************************
* Serialization
******************************************************************************/

void
packrat_parser_rep::serialize_compound (tree t, path p) {
  tree r= the_drd->get_syntax (t, p);
  if (r != UNINIT)
    serialize (r, path (-1));
  else if (is_func (t, QUASI, 2)) {
    tree tt= t[0];
    path pp= as_path (t[1]);
    serialize (tt, pp);
  }
  else {
    current_string << "<\\" << as_string (L(t)) << ">";
    for (int i=0; i<N(t); i++) {
      if (i != 0) current_string << "<|>";
      serialize (t[i], p * i);
    }
    current_string << "</>";
  }
}

void
packrat_parser_rep::serialize (tree t, path p) {
  if (is_nil (p) || p->item != -1)
    current_start (p)= N(current_string);
  if (is_atomic (t)) {
    int begin= N(current_string);
    int pos=0;
    string s= t->label;
    while (true) {  
      if (N(current_string) != begin + pos)
        if (is_nil (p) || p->item != -1) {
          //cout << p * pos << " <-> " << N(current_string) << LF;
          //cout << "  " << s (0, pos) << LF;
          //cout << "  " << current_string (begin, N(current_string)) << LF;
          current_path_pos (p * pos)= N(current_string);
          current_pos_path (N(current_string))= p * pos;
        }
      if (pos >= N(s)) break;
      int start= pos;
      tm_char_forwards (s, pos);
      if (pos == start+1)
	current_string << s[start];
      else {
        // FIXME: where did we use this kind of syntactical substitutions?
        tree r (UNINIT);
	string ss= s (start, pos);
        if (N(ss) != 1 && existing_tree_label (ss))
          r= the_drd->get_syntax (as_tree_label (ss));
        //if (r != UNINIT) cout << "Rewrite " << ss << " -> " << r << "\n";
        if (r == UNINIT) current_string << ss;
        else serialize (r, path (-1));
      }
    }
  }
  else switch (L(t)) {
    case RAW_DATA:
      current_string << "<\\rawdata></>";
      break;
    case DOCUMENT:
    case PARA:
      for (int i=0; i<N(t); i++) {
        serialize (t[i], p * i);
        current_string << "\n";
      }
      break;
    case SURROUND:
      serialize (t[0], p * 0);
      serialize (t[2], p * 2);
      serialize (t[1], p * 1);
      break;
    case CONCAT:
      for (int i=0; i<N(t); i++)
        serialize (t[i], p * i);
      break;
    case RIGID:
    case HGROUP:
    case VGROUP:
      serialize (t[0], p * 0);
      break;
    case HIDDEN:
      break;
    case FREEZE:
    case UNFREEZE:
      serialize (t[0], p * 0);
      break;
    case HSPACE:
    case VAR_VSPACE:
    case VSPACE:
    case SPACE:
    case HTAB:
      break;
    case MOVE:
    case SHIFT:
    case RESIZE:
    case CLIPPED:
      serialize (t[0], p * 0);
      break;

    case WITH_LIMITS:
    case LINE_BREAK:
    case NEW_LINE:
    case NEXT_LINE:
    case NO_BREAK:
    case YES_INDENT:
    case NO_INDENT:
    case VAR_YES_INDENT:
    case VAR_NO_INDENT:
    case VAR_PAGE_BREAK:
    case PAGE_BREAK:
    case VAR_NO_PAGE_BREAK:
    case NO_PAGE_BREAK:
    case VAR_NO_BREAK_HERE:
    case NO_BREAK_HERE:
    case NO_BREAK_START:
    case NO_BREAK_END:
    case VAR_NEW_PAGE:
    case NEW_PAGE:
    case VAR_NEW_DPAGE:
    case NEW_DPAGE:
      break;

    case WIDE:
    case NEG:
      serialize (t[0], p * 0);
      break;
    case SYNTAX:
      serialize (t[1], path (-1));
      break;
    case TFORMAT:
    case TWITH:
    case CWITH:
    case CELL:
      serialize (t[N(t)-1], p * (N(t)-1));
      break;

    case WITH:
      if (t[0] == "mode" && t[1] != "math");
      else serialize (t[N(t)-1], p * (N(t)-1));
      break;
    case VALUE: {
      tree r= the_drd->get_syntax (t);
      //if (r != UNINIT) cout << "Rewrite " << t << " -> " << r << "\n";
      if (r != UNINIT) serialize (r, path (-1));
      else serialize_compound (t, p);
      break;
    }

    case STYLE_WITH:
    case VAR_STYLE_WITH:
    case LOCUS:
      serialize (t[N(t)-1], p * (N(t)-1));
      break;
    case ID:
    case HARD_ID:
    case LINK:
    case URL:
    case SCRIPT:
    case OBSERVER:
    case FIND_ACCESSIBLE:
      break;
    case HLINK:
    case ACTION:
      serialize (t[0], p * 0);
      break;
    case SET_BINDING:
    case GET_BINDING:
    case LABEL:
    case REFERENCE:
    case PAGEREF:
    case GET_ATTACHMENT:
    case WRITE:
    case TOC_NOTIFY:
      break;

    case SPECIFIC:
      serialize (t[1], p * 1);
      break;
    case FLAG:
      break;
    case HYPHENATE_AS:
      serialize (t[1], p * 1);
      break;

    default:
      serialize_compound (t, p);
      break;
    }
  if (is_nil (p) || p->item != -1)
    current_end (p)= N(current_string);
}
