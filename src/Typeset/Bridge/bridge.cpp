
/******************************************************************************
* MODULE     : bridge.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "Boxes/construct.hpp"

bridge bridge_document (typesetter, tree, path);
bridge bridge_surround (typesetter, tree, path);
bridge bridge_hidden (typesetter, tree, path);
bridge bridge_formatting (typesetter, tree, path, string);
bridge bridge_with (typesetter, tree, path);
bridge bridge_rewrite (typesetter, tree, path);
bridge bridge_argument (typesetter, tree, path);
bridge bridge_default (typesetter, tree, path);
bridge bridge_compound (typesetter, tree, path);
bridge bridge_mark (typesetter, tree, path);
bridge bridge_expand_as (typesetter, tree, path);
bridge bridge_eval (typesetter, tree, path);
bridge bridge_auto (typesetter, tree, path, tree, bool);
bridge bridge_locus (typesetter, tree, path);
bridge bridge_ornament (typesetter, tree, path);
bridge bridge_art_box (typesetter, tree, path);
bridge bridge_canvas (typesetter, tree, path);

bridge nil_bridge;

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

bridge_rep::bridge_rep (typesetter ttt2, tree st2, path ip2):
  ttt (ttt2), env (ttt->env), st (st2), ip (ip2),
  status (CORRUPTED), changes (UNINIT), stack_cache_ok (false), version (0),
  chunk_cache (NULL) {}

static tree inactive_auto
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x"), "recurse*"));
static tree error_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "error*"));
static tree inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "once*"));
static tree var_inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "recurse*"));

bridge
make_inactive_bridge (typesetter ttt, tree st, path ip) {
  if (is_document (st))
    return bridge_document (ttt, st, ip);
  else return bridge_auto (ttt, st, ip, inactive_auto, false);
}

bridge
make_bridge (typesetter ttt, tree st, path ip) {
  // cout << "Make bridge " << st << ", " << ip << LF;
  // cout << "Preamble mode= " << ttt->env->preamble << LF;
  if (ttt->env->preamble)
    return make_inactive_bridge (ttt, st, ip);
  switch (L(st)) {
  case _ERROR:
    return bridge_auto (ttt, st, ip, error_m, true);
  case DOCUMENT:
    return bridge_document (ttt, st, ip);
  case SURROUND:
    return bridge_surround (ttt, st, ip);
  case HIDDEN:
    return bridge_hidden (ttt, st, ip);
  case DATOMS:
    return bridge_formatting (ttt, st, ip, ATOM_DECORATIONS);
  case DLINES:
    return bridge_formatting (ttt, st, ip, LINE_DECORATIONS);
  case DPAGES:
    return bridge_formatting (ttt, st, ip, PAGE_DECORATIONS);
  case TFORMAT:
    return bridge_formatting (ttt, st, ip, CELL_FORMAT);
  case WITH:
    return bridge_with (ttt, st, ip);
  case COMPOUND:
    return bridge_compound (ttt, st, ip);
  case ARG:
    return bridge_argument (ttt, st, ip);
  case MAP_ARGS:
    // FIXME: we might want to merge bridge_rewrite and bridge_eval
    // 'map_args' should really be implemented using bridge_rewrite,
    // but bridge_eval leads to better locality of updates for 'screens'
    return bridge_eval (ttt, st, ip);
  case MARK:
  case VAR_MARK:
    return bridge_mark (ttt, st, ip);
  case EXPAND_AS:
    return bridge_expand_as (ttt, st, ip);
  case EVAL:
  case QUASI:
    return bridge_eval (ttt, st, ip);
  case EXTERN:
  case VAR_INCLUDE:
  case WITH_PACKAGE:
    return bridge_rewrite (ttt, st, ip);
  case INCLUDE:
    return bridge_compound (ttt, st, ip);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return bridge_compound (ttt, st, ip);
  case INACTIVE:
    return bridge_auto (ttt, st, ip, inactive_m, true);
  case VAR_INACTIVE:
    return bridge_auto (ttt, st, ip, var_inactive_m, true);
  case REWRITE_INACTIVE:
    return bridge_rewrite (ttt, st, ip);
  case LOCUS:
    return bridge_locus (ttt, st, ip);
  case HLINK:
  case ACTION:
    return bridge_compound (ttt, st, ip);
  case ANIM_STATIC:
  case ANIM_DYNAMIC:
    return bridge_eval (ttt, st, ip);
  case CANVAS:
    return bridge_canvas (ttt, st, ip);
  case ORNAMENT:
    return bridge_ornament (ttt, st, ip);
  case ART_BOX:
    return bridge_art_box (ttt, st, ip);
  default:
    if (L(st) < START_EXTENSIONS) return bridge_default (ttt, st, ip);
    else return bridge_compound (ttt, st, ip);
  }
}

void
replace_bridge (bridge& br, tree st, path ip) {
  bridge new_br= make_bridge (br->ttt, st, ip);
  new_br->changes= br->changes;
  br= new_br;
}

void
replace_bridge (bridge& br, path p, tree oldt, tree newt, path ip) {
  if (oldt == newt) return;
  if (is_atomic (newt) || L(oldt) != L(newt) || N(oldt) != N(newt)) {
    if (is_nil (p)) replace_bridge (br, newt, ip);
    else br->notify_assign (p, newt);
  }
  else
    for (int i=0; i<N(newt); i++)
      replace_bridge (br, p * i, oldt[i], newt[i], ip);
}

bool
bridge::operator == (bridge item2) {
  return rep == item2.rep;
}

bool
bridge::operator != (bridge item2) {
  return rep != item2.rep;
}

tm_ostream&
operator << (tm_ostream& out, bridge br) {
  return out << "bridge [" << br->st << ", " << br->ip << "]";
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) {
    ASSERT (is_atomic (u), "two atoms expected");
    t= t->label (0, l) * u->label * t->label (l, N(t->label));
  }
  else t= (t (0, l) * u) * t (l, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_remove (path p, int nr) {
  // cout << "Insert " << p << ", " << nr << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) t= t->label (0, l) * t->label (l+nr, N(t->label));
  else t= t (0, l) * t (l+nr, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_split (path p) {
  // cout << "Split " << p << " in " << st << "\n";
  path q  = path_up (p, 2);
  int  pos= last_item (path_up (p));
  int  l  = last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos])) {
    string s1= t[pos]->label (0, l), s2= t[pos]->label (l, N (t[pos]->label));
    notify_insert (q * pos, tree (L(t), s1));
    notify_assign (q * (pos+1), s2);
  }
  else {
    tree t1= t[pos] (0, l), t2= t[pos] (l, N(t[pos]));
    notify_insert (q * pos, tree (L(t), t1));
    notify_assign (q * (pos+1), t2);
  }
}

void
bridge_rep::notify_join (path p) {
  // cout << "Join " << p << " in " << st << "\n";
  path q  = path_up (p);
  int  pos= last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos]) && is_atomic (t[pos+1])) {
    string j= t[pos]->label * t[pos+1]->label;
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
  else {
    tree j= t[pos] * t[pos+1];
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
}

/******************************************************************************
* Getting environment variables and typesetting
******************************************************************************/

void
bridge_rep::my_clean_links () {
  link_env= link_repository (true);
}

void
bridge_rep::my_exec_until (path p) {
  env->exec_until (st, p);
}

bool
bridge_rep::my_typeset_will_be_complete () {
  return (status & VALID_MASK) == CORRUPTED;
}

void
bridge_rep::my_typeset (int desired_status) {
  if ((desired_status & WANTED_MASK) == WANTED_PARAGRAPH)
    ttt->insert_paragraph (st, ip);
  if ((desired_status & WANTED_MASK) == WANTED_PARUNIT)
    ttt->insert_parunit (st, ip);
}

void
bridge_rep::exec_until (path p, bool skip_flag) {
  // This virtual routine is redefined in bridge_auto in order to
  // treat cursor positions on the border in a special way depending
  // on skip_flag

  (void) skip_flag;
  // cout << "Exec until " << p << " in " << st << "\n";
  if ((status & VALID_MASK) != PROCESSED) {
    // cout << "  Re-execute until\n";
    env->exec_until (st, p);
  }
  else if (p == path (right_index (st))) {
    // cout << "  Patch env\n";
    env->patch_env (changes);
  }
  else if (p != path (0)) {
    // cout << "  My execute until\n";
    my_exec_until (p);
  }
  // cout << "  Done\n";
}

extern tree the_et;

/******************************************************************************
* Merging runs of lines outside paper mode
*
* Outside paper mode, the lines of a bridge are merged into a single stack
* box before being handed to the pager, unless some of them carry floats or
* several columns.  In the latter case all lines used to be passed on
* individually, so that for a long document with a few figures the pager
* reprocessed every line of the document at each keystroke.  Instead, runs
* of ordinary lines are merged into chunks, exactly as the whole list would
* have been merged in the absence of floats, while the special items are
* kept as they are.  Chunk boundaries only depend on the lines themselves,
* so that an edit only changes the chunk in which it occurs, and chunks are
* cached from one typesetting pass to the next.
******************************************************************************/

#include <stdint.h>

#define CHUNK_MAX 256

struct line_chunk {
  array<page_item> src;  // the merged lines
  page_item        out;  // the resulting item
};

struct chunk_cache_rep {
  // chunks of the current and of the previous pass, by their first box
  hashmap<pointer,line_chunk> cur, prev;
};

bridge_rep::~bridge_rep () {
  if (chunk_cache != NULL) tm_delete (chunk_cache);
}

static inline bool
chunkable (page_item& it) {
  return it->type == PAGE_LINE_ITEM && N(it->fl) == 0 && it->nr_cols <= 1;
}

static inline bool
chunk_boundary (page_item& it) {
  // content defined boundaries (about one every 64 lines)
  uint64_t h= (uint64_t) (uintptr_t) it->b.operator-> ();
  h= (h >> 4) * 0x9E3779B97F4A7C15ULL;
  return (h >> 58) == 0;
}

static inline bool
same_space (space s1, space s2) {
  return s1->min == s2->min && s1->def == s2->def && s1->max == s2->max;
}

static bool
same_lines (array<page_item>& src, array<page_item>& l, int i1, int i2) {
  if (N(src) != i2 - i1) return false;
  for (int k= i1; k < i2; k++) {
    page_item& a= src[k-i1];
    page_item& b= l[k];
    if (a->b != b->b || a->penalty != b->penalty ||
        !same_space (a->spc, b->spc))
      return false;
  }
  return true;
}

static page_item
merge_line_run (path ip, array<page_item>& l, int i1, int i2) {
  array<box> bs;
  array<SI>  spc;
  for (int k= i1; k < i2; k++) {
    bs  << l[k]->b;
    spc << l[k]->spc->def;
  }
  box lb= stack_box (path (ip), bs, spc);
  lb= move_box (path (ip), lb, 0, bs[0]->y2);
  page_item it (lb);
  it->spc= l[i2-1]->spc;
  it->penalty= l[i2-1]->penalty;
  return it;
}

static array<page_item>
chunk_lines (bridge_rep* br, array<page_item> l) {
  if (br->chunk_cache == NULL) br->chunk_cache= tm_new<chunk_cache_rep> ();
  chunk_cache_rep* cc= br->chunk_cache;
  cc->prev= cc->cur;
  cc->cur = hashmap<pointer,line_chunk> ();
  array<page_item> out;
  int i= 0, n= N(l);
  // the first and the last item are never merged: the pager uses their
  // extents for the corrections at the top and the bottom of pages
  while (i < n) {
    if (i == 0 || i == n-1 || !chunkable (l[i])) { out << l[i]; i++; continue; }
    int j= i;
    while (j < n-1 && chunkable (l[j]) && j - i < CHUNK_MAX) {
      j++;
      if (chunk_boundary (l[j-1])) break;
    }
    if (j - i < 2) { out << l[i]; i= j; continue; }
    pointer key= (pointer) l[i]->b.operator-> ();
    if (cc->prev->contains (key) && same_lines (cc->prev (key).src, l, i, j)) {
      out << cc->prev (key).out;
      cc->cur (key)= cc->prev (key);
    }
    else {
      line_chunk ch;
      ch.src= range (l, i, j);
      ch.out= merge_line_run (br->ip, l, i, j);
      out << ch.out;
      cc->cur (key)= ch;
    }
    i= j;
  }
  return out;
}

void
bridge_rep::typeset (int desired_status) {
  // FIXME: this dirty hack ensures a perfect coherence between
  // the bridge and the edit tree at the typesetting stage.
  // This should not be necessary, but we use because the ip_observers
  // may become wrong otherwise.
  if (is_accessible (ip))
    st= subtree (the_et, reverse (ip));
  if (!is_accessible (ip)) {
    path ip2= obtain_ip (st);
    if (ip2 != path (DETACHED))
      ip= ip2;
  }

  //cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
  if ((status==desired_status) && (N(ttt->old_patch)==0)) {
    //cout << "cached" << LF;
    env->monitored_patch_env (changes);
    // cout << "changes       = " << changes << LF;
  }
  else {
    // cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
    //cout << "recomputing" << LF;
    hashmap<string,tree> prev_back (UNINIT);
    my_clean_links ();
    link_repository old_link_env= env->link_env;
    env->link_env= link_env;
    ttt->local_start (l, sb);
    env->local_start (prev_back);
    if (env->hl_lan != 0) env->lan->highlight (st);
    my_typeset (desired_status);
    env->local_update (ttt->old_patch, changes);
    env->local_end (prev_back);
    ttt->local_end (l, sb);
    env->link_env= old_link_env;
    status= desired_status;
    stack_cache_ok= false;
    version++;
    // cout << "old_patch     = " << ttt->old_patch << LF;
    // cout << "changes       = " << changes << LF;
    // cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;
  }
  //cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;

  // ttt->insert_stack (l, sb);
  //if (N(l) == 0); else
  if (ttt->paper || (N(l) <= 1)) ttt->insert_stack (l, sb);
  else if (stack_cache_ok && strong_equal (ip, stack_cache_ip))
    ttt->insert_stack (stack_cache, sb);
  else {
    bool flag= false;
    int i, n= N(l);
    for (i=0; i<n; i++)
      flag= flag || (N (l[i]->fl) != 0) || (l[i]->nr_cols > 1);
    if (flag) {
      stack_cache= chunk_lines (this, l);
      stack_cache_ip= ip;
      stack_cache_ok= true;
      ttt->insert_stack (stack_cache, sb);
    }
    else {
      int first=-1, last=-1;
      array<box> bs;
      array<SI>  spc;
      array<page_item> special_l;
      for (i=0; i<n; i++)
	if (l[i]->type != PAGE_CONTROL_ITEM) {
	  if (first == -1 && l[i]->type == PAGE_LINE_ITEM) first= N(bs);
	  bs  << l[i]->b;
	  spc << l[i]->spc->def;
	  last= i;
	}
        else if (is_tuple (l[i]->t, "env_page") &&
                 (l[i]->t[1] == PAGE_THIS_TOP ||
                  l[i]->t[1] == PAGE_THIS_BOT ||
                  l[i]->t[1] == PAGE_THIS_BG_COLOR))
          special_l << l[i];
      box lb= stack_box (path (ip), bs, spc);
      if (first != -1) lb= move_box (path (ip), lb, 0, bs[first]->y2);
      array<page_item> new_l (1);
      new_l[0]= page_item (lb);
      new_l[0]->spc= l[last]->spc;
      new_l << special_l;
      stack_cache= new_l;
      stack_cache_ip= ip;
      stack_cache_ok= true;
      ttt->insert_stack (new_l, sb);
    }
  }

  //cout << "l   = " << l << LF;
  //cout << "sb  = " << sb << LF;
  //cout << "l   = " << ttt->l << LF;
  //cout << "a   = " << ttt->a << LF;
  //cout << "b   = " << ttt->b << LF;
}
