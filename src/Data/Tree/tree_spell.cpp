
/******************************************************************************
* MODULE     : tree_spell.cpp
* DESCRIPTION: Spell checking inside trees
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_search.hpp"
#include "analyze.hpp"
#include "boot.hpp"
#include "drd_mode.hpp"
#include "drd_std.hpp"
#include "language.hpp"
#include "vars.hpp"

int  spell_max_hits= 1000000;
void spell (range_set& sel, tree t, tree what, path p);

/******************************************************************************
* Useful subroutines
******************************************************************************/

static void
merge (range_set& sel, range_set ssel) {
  for (int i=0; i+1<N(ssel); i+=2)
    if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], ssel[i]))
      sel << ssel[i] << ssel[i+1];
}

static bool
is_accessible_for_spell (tree t, int i) {
  if (is_accessible_child (t, i)) return true;
  if (is_func (t, HIDDEN)) return true;
  if (get_access_mode () != DRD_ACCESS_SOURCE) return false;
  if (is_func (t, RAW_DATA)) return false;
  return i >= 0 && i < N(t);
}

/******************************************************************************
* Spelling
******************************************************************************/

bool
spell_string (tree lan, string s) {
  if (is_atomic (lan)) return check_word (lan->label, s);
  else return true;
}

void
spell_string (tree lan, range_set& sel, string s, path p) {
  int pos= 0;
  while (pos < N(s) && s[pos] == ' ') pos++;
  while (pos < N(s)) {
    while (pos < N(s) && s[pos] == ' ') pos++;
    int start= pos;
    while (pos < N(s) && s[pos] != ' ') pos++;
    int end= pos;
    if (!spell_string (lan, s (start, end))) {
      int start2= start, end2= end;
      while (start < end && !is_iso_alpha (s[start]))
        tm_char_forwards (s, start);
      while (start < end && !is_iso_alpha (s[end-1]))
        tm_char_backwards (s, end);
      if ((start == start2 && end == end2) ||
          !spell_string (lan, s (start, end)))
        while (start < end) {
          int begin= start;
          while (start < end && is_iso_alpha (s[start])) start++;
          if ((begin == start2 && start == end2) ||
              !spell_string (lan, s (begin, start)))
            merge (sel, simple_range (p * begin, p * start));
          while (start < end && !is_iso_alpha (s[start]))
            tm_char_forwards (s, start);
        }
    }
  }
}

void
spell (tree mode, tree lan, range_set& sel, tree t, path p) {
  if (N(sel) > spell_max_hits) return;
  if (is_atomic (t)) {
    if (mode == "text") spell_string (lan, sel, t->label, p); }
  else
    for (int i=0; i<N(t); i++)
      if (is_accessible_for_spell (t, i)) {
        tree smode= the_drd->get_env_child (t, i, MODE, mode);
        tree slan = the_drd->get_env_child (t, i, LANGUAGE, lan);
        spell (smode, slan, sel, t[i], p * i);
      }
}

void
spell (tree mode, tree lan, range_set& sel, tree t, path p, path pos) {
  if (is_atomic (t)) spell (mode, lan, sel, t, p);
  else {
    if (is_nil (pos)) spell (mode, lan, sel, t, p);
    else {
      int hits= 0;
      array<range_set> sub (N(t));
      if (pos->item >= 0 && pos->item < N(t))
        if (is_accessible_for_spell (t, pos->item)) {
          int i= pos->item;
          tree smode= the_drd->get_env_child (t, i, MODE, mode);
          tree slan = the_drd->get_env_child (t, i, LANGUAGE, lan);
          spell (smode, slan, sub[i], t[i], p * i);
          hits += N(sub[i]);
        }
      for (int d=1; d<N(t); d++)
        for (int e=0; e<=1; e++) {
          if (hits > spell_max_hits) break;
          int i= (e==0? pos->item + d: pos->item - d);
          if (i >= 0 && i < N(t))
            if (is_accessible_for_spell (t, i)) {
              tree smode= the_drd->get_env_child (t, i, MODE, mode);
              tree slan = the_drd->get_env_child (t, i, LANGUAGE, lan);
              spell (smode, slan, sub[i], t[i], p * i);
              hits += N(sub[i]);
            }
        }
      for (int i=0; i<N(t); i++) sel << sub[i];
    }
  }
}

/******************************************************************************
* Front end
******************************************************************************/

range_set
spell (string lan, tree t, path p, int limit) {
  spell_max_hits= limit;
  range_set sel;
  //cout << "Spell " << what << "\n";
  spell ("text", lan, sel, t, p);
  //cout << "Selected " << sel << "\n";
  spell_max_hits= 1000000;
  return sel;
}

range_set
spell (string lan, tree t, path p, path pos, int limit) {
  spell_max_hits= limit;
  range_set sel;
  //cout << "Spell " << what << "\n";
  spell ("text", lan, sel, t, p, pos);
  //cout << "Selected " << sel << "\n";
  spell_max_hits= 1000000;
  return sel;
}
