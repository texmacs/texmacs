
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
#include "hashset.hpp"
#include "universal.hpp"

int  spell_max_hits= 1000000;
void spell (range_set& sel, tree t, tree what, path p);
hashset<tree_label> spell_ignore;

/******************************************************************************
* Useful subroutines
******************************************************************************/

static void
spell_initialize () {
  if (N(spell_ignore) > 0) return;
  spell_ignore->insert (make_tree_label ("abbr"));
  spell_ignore->insert (make_tree_label ("name"));
  spell_ignore->insert (make_tree_label ("bib-list"));
  spell_ignore->insert (make_tree_label ("explain-macro"));
}

static void
merge (range_set& sel, range_set ssel) {
  for (int i=0; i+1<N(ssel); i+=2)
    if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], ssel[i]))
      sel << ssel[i] << ssel[i+1];
}

static bool
is_accessible_for_spell (tree t, int i) {
  if (spell_ignore->contains (L(t))) return false;
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
spell_string (tree lan, range_set& sel, string s,
              path p, int pos1, int pos2) {
  int pos= pos1;
  while (pos < pos2 && s[pos] == ' ') pos++;
  while (pos < pos2) {
    while (pos < pos2 && s[pos] == ' ') pos++;
    int start= pos;
    while (pos < pos2 && s[pos] != ' ') pos++;
    int end= pos;
    if (!spell_string (lan, s (start, end))) {
      int start2= start, end2= end;
      if (start < end && (is_numeric (s[start]) || is_numeric (s[end-1]))) {
        // NOTE: always accept postal codes; could be a user preference
        bool ok= true;
        for (int i= start; i<end; ) {
          int save= i;
          tm_char_forwards (s, i);
          string ss= s (save, i);
          ok= ok && ss == uni_upcase_char (ss);
        }
        if (ok) start= end;
      }
      while (start < end) {
        int save= start;
        tm_char_forwards (s, start);
        if (uni_is_letter (s (save, start))) { start= save; break; }
      }
      while (start < end) {
        int save= end;
        tm_char_backwards (s, end);
        if (uni_is_letter (s (end, save))) { end= save; break; }
      }
      if ((start == start2 && end == end2) ||
          (start < end && !spell_string (lan, s (start, end))))
        while (start < end) {
          int begin= start;
          while (start < end) {
            int save= start;
            tm_char_forwards (s, start);
            if (!uni_is_letter (s (save, start))) { start= save; break; }
          }
          if ((begin == start2 && start == end2) ||
              !spell_string (lan, s (begin, start)))
            merge (sel, simple_range (p * begin, p * start));
          while (start < end) {
            int save= start;
            tm_char_forwards (s, start);
            if (uni_is_letter (s (save, start))) { start= save; break; }
          }
        }
    }
  }
}

void
spell (tree mode, tree lan, range_set& sel, tree t, path p) {
  if (N(sel) > spell_max_hits) return;
  if (is_atomic (t)) {
    if (mode == "text")
      spell_string (lan, sel, t->label, p, 0, N(t->label)); }
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

void
spell (tree mode, tree lan, range_set& sel, tree t,
       path p, path pos1, path pos2) {
  cout << "Spell " << p << ", " << (is_atomic (t)? -1: N(t))
       << "; " << pos1 << " -- " << pos2 << LF;
  if (is_nil (pos1) || is_nil (pos2));
  else if (is_atomic (t))
    spell_string (lan, sel, t->label,
                  p, max (pos1->item, 0), min (pos2->item, N(t->label)));
  else if (pos1 == path (0) && pos2 == path (1))
    spell (mode, lan, sel, t, p);
  else if (!is_atom (pos1) && !is_atom (pos2) && pos1->item == pos2->item)
    spell (mode, lan, sel, t[pos1->item],
           p * pos1->item, pos1->next, pos2->next);
  else {
    int i1= 0, i2= N(t);
    if (!is_atom (pos1)) i1= max (pos1->item, i1);
    if (!is_atom (pos2)) i2= min (pos2->item + 1, i2);
    int hits= 0;
    array<range_set> sub (N(t));
    for (int i=i1; i<i2; i++) {
      if (hits > spell_max_hits) break;
      if (is_accessible_for_spell (t, i)) {
        tree smode= the_drd->get_env_child (t, i, MODE, mode);
        tree slan = the_drd->get_env_child (t, i, LANGUAGE, lan);
        if (i == i1 && !is_atom (pos1) && i1 + 1 < i2)
          spell (smode, slan, sub[i], t[i],
                 p * i, pos1->next, path (right_index (t[i])));
        else if (i == i2 - 1 && !is_atom (pos2) && i1 + 1 < i2)
          spell (smode, slan, sub[i], t[i],
                 p * i, path (0), pos2->next);
        else
          spell (smode, slan, sub[i], t[i], p * i);
        hits += N(sub[i]);
      }
    }
    for (int i=i1; i<i2; i++) sel << sub[i];
  }
}

/******************************************************************************
* Front end
******************************************************************************/

range_set
spell (string lan, tree t, path p, int limit) {
  spell_initialize ();
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
  spell_initialize ();
  spell_max_hits= limit;
  range_set sel;
  //cout << "Spell " << what << "\n";
  spell ("text", lan, sel, t, p, pos);
  //cout << "Selected " << sel << "\n";
  spell_max_hits= 1000000;
  return sel;
}

range_set
spell (string lan, tree t, path p, path pos1, path pos2, int limit) {
  spell_initialize ();
  spell_max_hits= limit;
  range_set sel;
  //cout << "Spell " << what << "\n";
  spell ("text", lan, sel, t, p, pos1, pos2);
  //cout << "Selected " << sel << "\n";
  spell_max_hits= 1000000;
  return sel;
}
