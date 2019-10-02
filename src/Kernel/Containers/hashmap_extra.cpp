
/******************************************************************************
* MODULE     : hashmap_extra.cpp
* DESCRIPTION: extra routines for hashmap<string,tree>
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHMAP_EXTRA_CC
#define HASHMAP_EXTRA_CC
#include "hashmap.hpp"
#define TMPL template<class T, class U>
#define H hashentry<T,U>

TMPL void
hashmap_rep<T,U>::write_back (T x, hashmap<T,U> base) {
  int hv= hash (x);
  list<hashentry<T,U> > l (a [hv & (n-1)]);
  while (!is_nil (l)) {
    if (l->item.code == hv && l->item.key == x)
      return;
    l= l->next;
  }
  if (size >= n*max) resize (n<<1);
  list<hashentry<T,U> >& rl= a[hv & (n-1)];
  rl= list<hashentry<T,U> > (H (hv, x, init), rl);
  size ++;

  list<hashentry<T,U> > bl (base->a [hv & (base->n-1)]);
  while (!is_nil (bl)) {
    if (bl->item.code == hv && bl->item.key == x) {
      rl->item.im= bl->item.im;
      return;
    }
    bl= bl->next;
  }
  rl->item.im= base->init;
}

TMPL void
hashmap_rep<T,U>::pre_patch (hashmap<T,U> patch, hashmap<T,U> base) {
  int i= 0, n= patch->n;
  for (; i<n; i++) {
    list<hashentry<T,U> > l= patch->a[i];
    for (; !is_nil (l); l= l->next) {
      T x= l->item.key;
      U y= contains (x)? bracket_ro (x): l->item.im;
      if (base[x] == y) reset (x);
      else bracket_rw (x)= y;
    }
  }
}

TMPL void
hashmap_rep<T,U>::post_patch (hashmap<T,U> patch, hashmap<T,U> base) {
  int i= 0, n= patch->n;
  for (; i<n; i++) {
    list<hashentry<T,U> > l= patch->a[i];
    for (; !is_nil (l); l= l->next) {
      T x= l->item.key;
      U y= l->item.im;
      if (base[x] == y) reset (x);
      else bracket_rw (x)= y;
    }
  }
}

TMPL list<hashentry<T,U> >
copy_list (list<hashentry<T,U> > l) {
  if (is_nil (l)) return l;
  else return list<hashentry<T,U> >
	        (hashentry<T,U> (l->item.code, l->item.key, l->item.im),
		 copy_list (l->next));
}

TMPL hashmap<T,U>
copy (hashmap<T,U> h) {
  int i, n= h->n;
  hashmap<T,U> h2 (h->init, n, h->max);
  h2->size= h->size;
  for (i=0; i<n; i++)
    h2->a[i]= copy_list (h->a[i]);
  return h2;
}

TMPL hashmap<T,U>
changes (hashmap<T,U> patch, hashmap<T,U> base) {
  int i;
  hashmap<T,U> h (base->init);
  for (i=0; i<patch->n; i++) {
    list<hashentry<T,U> > l (patch->a[i]);
    while (!is_nil (l)) {
      if (l->item.im != base [l->item.key])
	h (l->item.key)= l->item.im;
      l=l->next;
    }
  }
  return h;
}

TMPL hashmap<T,U>
invert (hashmap<T,U> patch, hashmap<T,U> base) {
  int i;
  hashmap<T,U> h (base->init);
  for (i=0; i<patch->n; i++) {
    list<hashentry<T,U> > l (patch->a[i]);
    while (!is_nil (l)) {
      if (l->item.im != base [l->item.key])
	h (l->item.key)= base [l->item.key];
      l=l->next;
    }
  }
  return h;
}

TMPL hashmap<T,U>::hashmap (U init, tree t):
  rep (tm_new<hashmap_rep<T,U> > (init, 1, 1))
{
  int i, n= arity (t);
  for (i=0; i<n; i++)
    if (is_func (t[i], ASSOCIATE, 2))
      rep->bracket_rw (get_label (t[i][0]))= copy (t[i][1]);
}

#undef H
#undef TMPL
#endif // defined HASHMAP_EXTRA_CC
