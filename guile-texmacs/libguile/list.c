/* Copyright (C) 1995,1996,1997,2000,2001,2003,2004,2008
 * Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/list.h"
#include "libguile/eval.h"

#include <stdarg.h>


/* creating lists */

#define SCM_I_CONS(cell, x, y)			\
do {						\
  cell = scm_cell ((scm_t_bits)x, (scm_t_bits)y);			\
} while (0)

SCM
scm_list_1 (SCM e1)
{
  SCM c1;
  SCM_I_CONS (c1, e1, SCM_EOL);
  return c1;
}

SCM
scm_list_2 (SCM e1, SCM e2)
{
  SCM c1, c2;
  SCM_I_CONS (c2, e2, SCM_EOL);
  SCM_I_CONS (c1, e1, c2);
  return c1;
}

SCM
scm_list_3 (SCM e1, SCM e2, SCM e3)
{
  SCM c1, c2, c3;
  SCM_I_CONS (c3, e3, SCM_EOL);
  SCM_I_CONS (c2, e2, c3);
  SCM_I_CONS (c1, e1, c2);
  return c1;
}

SCM
scm_list_4 (SCM e1, SCM e2, SCM e3, SCM e4)
{
  return scm_cons2 (e1, e2, scm_list_2 (e3, e4));
}

SCM
scm_list_5 (SCM e1, SCM e2, SCM e3, SCM e4, SCM e5)
{
  return scm_cons2 (e1, e2, scm_list_3 (e3, e4, e5));
}

SCM
scm_list_n (SCM elt, ...)
{
  va_list foo;
  SCM answer = SCM_EOL;
  SCM *pos = &answer;

  va_start (foo, elt);
  while (! SCM_UNBNDP (elt))
    {
#if (SCM_DEBUG_CELL_ACCESSES == 1)
      if (SCM_NIMP (elt))
	SCM_VALIDATE_CELL(elt, 0);
#endif      
      *pos = scm_cons (elt, SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      elt = va_arg (foo, SCM);
    }
  va_end (foo);
  return answer;
}


SCM_DEFINE (scm_make_list, "make-list", 1, 1, 0,
            (SCM n, SCM init),
	    "Create a list containing of @var{n} elements, where each\n"
	    "element is initialized to @var{init}.  @var{init} defaults to\n"
	    "the empty list @code{()} if not given.")
#define FUNC_NAME s_scm_make_list
{
  unsigned nn = scm_to_uint (n);
  unsigned i;
  SCM ret = SCM_EOL;

  if (SCM_UNBNDP (init))
    init = SCM_EOL;

  for (i = 0; i < nn; i++)
    ret = scm_cons (init, ret);
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_star, "cons*", 1, 0, 1,
            (SCM arg, SCM rest),
	    "Like @code{list}, but the last arg provides the tail of the\n"
	    "constructed list, returning @code{(cons @var{arg1} (cons\n"
	    "@var{arg2} (cons @dots{} @var{argn})))}.  Requires at least one\n"
	    "argument.  If given one argument, that argument is returned as\n"
	    "result.  This function is called @code{list*} in some other\n"
	    "Schemes and in Common LISP.")
#define FUNC_NAME s_scm_cons_star
{
  SCM ret = SCM_EOL;
  SCM *p = &ret;

  SCM_VALIDATE_REST_ARGUMENT (rest);

  for ( ; scm_is_pair (rest); rest = SCM_CDR (rest))
    {
      *p = scm_cons (arg, SCM_EOL);
      p = SCM_CDRLOC (*p);
      arg = SCM_CAR (rest);
    }

  *p = arg;
  return ret;
}
#undef FUNC_NAME



/* general questions about lists --- null?, list?, length, etc.  */

SCM_DEFINE (scm_null_p, "null?", 1, 0, 0, 
           (SCM x),
	    "Return @code{#t} iff @var{x} is the empty list, else @code{#f}.")
#define FUNC_NAME s_scm_null_p
{
  return scm_from_bool (SCM_NULL_OR_NIL_P (x));
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_p, "list?", 1, 0, 0, 
           (SCM x),
	    "Return @code{#t} iff @var{x} is a proper list, else @code{#f}.")
#define FUNC_NAME s_scm_list_p
{
  return scm_from_bool (scm_ilength (x) >= 0);
}
#undef FUNC_NAME


/* Return the length of SX, or -1 if it's not a proper list.
   This uses the "tortoise and hare" algorithm to detect "infinitely
   long" lists (i.e. lists with cycles in their cdrs), and returns -1
   if it does find one.  */
long
scm_ilength(SCM sx)
{
  long i = 0;
  SCM tortoise = sx;
  SCM hare = sx;

  do {
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (!scm_is_pair (hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    if (SCM_NULL_OR_NIL_P(hare)) return i;
    if (!scm_is_pair (hare)) return -1;
    hare = SCM_CDR(hare);
    i++;
    /* For every two steps the hare takes, the tortoise takes one.  */
    tortoise = SCM_CDR(tortoise);
  }
  while (!scm_is_eq (hare, tortoise));

  /* If the tortoise ever catches the hare, then the list must contain
     a cycle.  */
  return -1;
}


SCM_DEFINE (scm_length, "length", 1, 0, 0, 
           (SCM lst),
	    "Return the number of elements in list @var{lst}.")
#define FUNC_NAME s_scm_length
{
  long i;
  SCM_VALIDATE_LIST_COPYLEN (1, lst, i);
  return scm_from_long (i);
}
#undef FUNC_NAME



/* appending lists */

SCM_DEFINE (scm_append, "append", 0, 0, 1, 
            (SCM args),
	    "Return a list consisting of the elements the lists passed as\n"
	    "arguments.\n"
	    "@lisp\n"
	    "(append '(x) '(y))          @result{}  (x y)\n"
	    "(append '(a) '(b c d))      @result{}  (a b c d)\n"
	    "(append '(a (b)) '((c)))    @result{}  (a (b) (c))\n"
	    "@end lisp\n"
	    "The resulting list is always newly allocated, except that it\n"
	    "shares structure with the last list argument.  The last\n"
	    "argument may actually be any object; an improper list results\n"
	    "if the last argument is not a proper list.\n"
	    "@lisp\n"
	    "(append '(a b) '(c . d))    @result{}  (a b c . d)\n"
	    "(append '() 'a)             @result{}  a\n"
	    "@end lisp")
#define FUNC_NAME s_scm_append
{
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (scm_is_null (args)) {
    return SCM_EOL;
  } else {
    SCM res = SCM_EOL;
    SCM *lloc = &res;
    SCM arg = SCM_CAR (args);
    int argnum = 1;
    args = SCM_CDR (args);
    while (!scm_is_null (args)) {
      while (scm_is_pair (arg)) {
	*lloc = scm_cons (SCM_CAR (arg), SCM_EOL);
	lloc = SCM_CDRLOC (*lloc);
	arg = SCM_CDR (arg);
      }
      SCM_VALIDATE_NULL_OR_NIL (argnum, arg);
      arg = SCM_CAR (args);
      args = SCM_CDR (args);
      argnum++;
    };
    *lloc = arg;
    return res;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_append_x, "append!", 0, 0, 1, 
            (SCM lists),
	    "A destructive version of @code{append} (@pxref{Pairs and\n"
	    "Lists,,,r5rs, The Revised^5 Report on Scheme}).  The cdr field\n"
	    "of each list's final pair is changed to point to the head of\n"
	    "the next list, so no consing is performed.  Return\n"
	    "the mutated list.")
#define FUNC_NAME s_scm_append_x
{
  SCM ret, *loc;
  SCM_VALIDATE_REST_ARGUMENT (lists);

  if (scm_is_null (lists))
    return SCM_EOL;

  loc = &ret;
  for (;;)
    {
      SCM arg = SCM_CAR (lists);
      *loc = arg;

      lists = SCM_CDR (lists);
      if (scm_is_null (lists))
        return ret;

      if (!SCM_NULL_OR_NIL_P (arg))
        {
          SCM_VALIDATE_CONS (SCM_ARG1, arg);
          loc = SCM_CDRLOC (scm_last_pair (arg));
        }
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_last_pair, "last-pair", 1, 0, 0, 
           (SCM lst),
	    "Return the last pair in @var{lst}, signalling an error if\n"
	    "@var{lst} is circular.")
#define FUNC_NAME s_scm_last_pair
{
  SCM tortoise = lst;
  SCM hare = lst;

  if (SCM_NULL_OR_NIL_P (lst))
    return lst;

  SCM_VALIDATE_CONS (SCM_ARG1, lst);
  do {
    SCM ahead = SCM_CDR(hare);
    if (!scm_is_pair (ahead)) return hare;
    hare = ahead;
    ahead = SCM_CDR(hare);
    if (!scm_is_pair (ahead)) return hare;
    hare = ahead;
    tortoise = SCM_CDR(tortoise);
  }
  while (!scm_is_eq (hare, tortoise));
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", scm_list_1 (lst));
}
#undef FUNC_NAME


/* reversing lists */

SCM_DEFINE (scm_reverse, "reverse", 1, 0, 0,
            (SCM lst),
	    "Return a new list that contains the elements of @var{lst} but\n"
	    "in reverse order.")
#define FUNC_NAME s_scm_reverse
{
  SCM result = SCM_EOL;
  SCM tortoise = lst;
  SCM hare = lst;

  do {
      if (SCM_NULL_OR_NIL_P(hare)) return result;
      SCM_ASSERT(scm_is_pair(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      if (SCM_NULL_OR_NIL_P(hare)) return result;
      SCM_ASSERT(scm_is_pair(hare), lst, 1, FUNC_NAME);
      result = scm_cons (SCM_CAR (hare), result);
      hare = SCM_CDR (hare);
      tortoise = SCM_CDR (tortoise);
    }
  while (!scm_is_eq (hare, tortoise));
  SCM_MISC_ERROR ("Circular structure in position 1: ~S", scm_list_1 (lst));
}
#undef FUNC_NAME

SCM_DEFINE (scm_reverse_x, "reverse!", 1, 1, 0,
            (SCM lst, SCM new_tail),
	    "A destructive version of @code{reverse} (@pxref{Pairs and Lists,,,r5rs,\n"
	    "The Revised^5 Report on Scheme}).  The cdr of each cell in @var{lst} is\n"
	    "modified to point to the previous list element.  Return the\n"
	    "reversed list.\n\n"
	    "Caveat: because the list is modified in place, the tail of the original\n"
	    "list now becomes its head, and the head of the original list now becomes\n"
	    "the tail.  Therefore, the @var{lst} symbol to which the head of the\n"
	    "original list was bound now points to the tail.  To ensure that the head\n"
	    "of the modified list is not lost, it is wise to save the return value of\n"
	    "@code{reverse!}")
#define FUNC_NAME s_scm_reverse_x
{
  SCM_VALIDATE_LIST (1, lst);
  if (SCM_UNBNDP (new_tail))
    new_tail = SCM_EOL;
  else
    SCM_VALIDATE_LIST (2, new_tail);

  while (!SCM_NULL_OR_NIL_P (lst))
    {
      SCM old_tail = SCM_CDR (lst);
      SCM_SETCDR (lst, new_tail);
      new_tail = lst;
      lst = old_tail;
    }
  return new_tail;
}
#undef FUNC_NAME



/* indexing lists by element number */

SCM_DEFINE (scm_list_ref, "list-ref", 2, 0, 0,
	    (SCM list, SCM k),
	    "Return the @var{k}th element from @var{list}.")
#define FUNC_NAME s_scm_list_ref
{
  SCM lst = list;
  unsigned long int i;
  i = scm_to_ulong (k);
  while (scm_is_pair (lst)) {
    if (i == 0)
      return SCM_CAR (lst);
    else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_set_x, "list-set!", 3, 0, 0,
	    (SCM list, SCM k, SCM val),
	    "Set the @var{k}th element of @var{list} to @var{val}.")
#define FUNC_NAME s_scm_list_set_x
{
  SCM lst = list;
  unsigned long int i = scm_to_ulong (k);
  while (scm_is_pair (lst)) {
    if (i == 0) {
      SCM_SETCAR (lst, val);
      return val;
    } else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_tail);

SCM_DEFINE (scm_list_tail, "list-tail", 2, 0, 0,
           (SCM lst, SCM k),
	    "@deffnx {Scheme Procedure} list-cdr-ref lst k\n"
	    "Return the \"tail\" of @var{lst} beginning with its @var{k}th element.\n"
	    "The first element of the list is considered to be element 0.\n\n"
	    "@code{list-tail} and @code{list-cdr-ref} are identical.  It may help to\n"
	    "think of @code{list-cdr-ref} as accessing the @var{k}th cdr of the list,\n"
	    "or returning the results of cdring @var{k} times down @var{lst}.")
#define FUNC_NAME s_scm_list_tail
{
  size_t i = scm_to_size_t (k);
  while (i-- > 0) {
    SCM_VALIDATE_CONS (1, lst);
    lst = SCM_CDR(lst);
  }
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_cdr_set_x, "list-cdr-set!", 3, 0, 0,
           (SCM list, SCM k, SCM val),
	    "Set the @var{k}th cdr of @var{list} to @var{val}.")
#define FUNC_NAME s_scm_list_cdr_set_x
{
  SCM lst = list;
  size_t i = scm_to_size_t (k);
  while (scm_is_pair (lst)) {
    if (i == 0) {
      SCM_SETCDR (lst, val);
      return val;
    } else {
      --i;
      lst = SCM_CDR (lst);
    }
  };
  if (SCM_NULL_OR_NIL_P (lst))
    SCM_OUT_OF_RANGE (2, k);
  else
    SCM_WRONG_TYPE_ARG (1, list);
}
#undef FUNC_NAME



/* copying lists, perhaps partially */

SCM_DEFINE (scm_list_head, "list-head", 2, 0, 0,
           (SCM lst, SCM k),
	    "Copy the first @var{k} elements from @var{lst} into a new list, and\n"
	    "return it.")
#define FUNC_NAME s_scm_list_head
{
  SCM answer;
  SCM * pos;
  size_t i = scm_to_size_t (k);

  answer = SCM_EOL;
  pos = &answer;
  while (i-- > 0)
    {
      SCM_VALIDATE_CONS (1, lst);
      *pos = scm_cons (SCM_CAR (lst), SCM_EOL);
      pos = SCM_CDRLOC (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}
#undef FUNC_NAME


/* Copy a list which is known to be finite.  The last pair may or may not have
 * a '() in its cdr.  That is, improper lists are accepted.  */
SCM
scm_i_finite_list_copy (SCM list)
{
  if (!scm_is_pair (list))
    {
      return list;
    }
  else
    {
      SCM tail;
      const SCM result = tail = scm_list_1 (SCM_CAR (list));
      list = SCM_CDR (list);
      while (scm_is_pair (list))
        {
          const SCM new_tail = scm_list_1 (SCM_CAR (list));
          SCM_SETCDR (tail, new_tail);
          tail = new_tail;
          list = SCM_CDR (list);
        }
      SCM_SETCDR (tail, list);

      return result;
    }
}


SCM_DEFINE (scm_list_copy, "list-copy", 1, 0, 0, 
            (SCM lst),
	    "Return a (newly-created) copy of @var{lst}.")
#define FUNC_NAME s_scm_list_copy
{
  SCM newlst;
  SCM * fill_here;
  SCM from_here;

  SCM_VALIDATE_LIST (1, lst);

  newlst = SCM_EOL;
  fill_here = &newlst;
  from_here = lst;

  while (scm_is_pair (from_here))
    {
      SCM c;
      c = scm_cons (SCM_CAR (from_here), SCM_CDR (from_here));
      *fill_here = c;
      fill_here = SCM_CDRLOC (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}
#undef FUNC_NAME


SCM_PROC (s_list, "list", 0, 0, 1, scm_list_copy);
SCM_SNARF_DOCS (primitive, scm_list_copy, "list", (SCM objs), 0, 0, 1,
                "Return a list containing @var{objs}, the arguments to\n"
                "@code{list}.")

/* This used to be the code for "list", but it's wrong when used via apply
   (it should copy the list).  It seems pretty unlikely anyone would have
   been using this from C code, since it's a no-op, but keep it for strict
   binary compatibility.  */
SCM
scm_list (SCM objs)
{
  return objs;
}



/* membership tests (memq, memv, etc.) */ 

/* The function scm_c_memq returns the first sublist of list whose car is
 * 'eq?' obj, where the sublists of list are the non-empty lists returned by
 * (list-tail list k) for k less than the length of list.  If obj does not
 * occur in list, then #f (not the empty list) is returned.
 * List must be a proper list, otherwise scm_c_memq may crash or loop
 * endlessly.
 */
SCM
scm_c_memq (SCM obj, SCM list)
{
  for (; !SCM_NULL_OR_NIL_P (list); list = SCM_CDR (list))
    {
      if (scm_is_eq (SCM_CAR (list), obj))
	return list;
    }
  return SCM_BOOL_F;
}


SCM_DEFINE (scm_memq, "memq", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is @code{eq?}\n"
	    "to @var{x} where the sublists of @var{lst} are the non-empty\n"
	    "lists returned by @code{(list-tail @var{lst} @var{k})} for\n"
	    "@var{k} less than the length of @var{lst}.  If @var{x} does not\n"
	    "occur in @var{lst}, then @code{#f} (not the empty list) is\n"
	    "returned.")
#define FUNC_NAME s_scm_memq
{
  SCM_VALIDATE_LIST (2, lst);
  return scm_c_memq (x, lst);
}
#undef FUNC_NAME


SCM_DEFINE (scm_memv, "memv", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is @code{eqv?}\n"
	    "to @var{x} where the sublists of @var{lst} are the non-empty\n"
	    "lists returned by @code{(list-tail @var{lst} @var{k})} for\n"
	    "@var{k} less than the length of @var{lst}.  If @var{x} does not\n"
	    "occur in @var{lst}, then @code{#f} (not the empty list) is\n"
	    "returned.")
#define FUNC_NAME s_scm_memv
{
  SCM_VALIDATE_LIST (2, lst);
  for (; !SCM_NULL_OR_NIL_P (lst); lst = SCM_CDR (lst))
    {
      if (! scm_is_false (scm_eqv_p (SCM_CAR (lst), x)))
	return lst;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_member, "member", 2, 0, 0,
           (SCM x, SCM lst),
	    "Return the first sublist of @var{lst} whose car is\n"
	    "@code{equal?} to @var{x} where the sublists of @var{lst} are\n"
	    "the non-empty lists returned by @code{(list-tail @var{lst}\n"
	    "@var{k})} for @var{k} less than the length of @var{lst}.  If\n"
	    "@var{x} does not occur in @var{lst}, then @code{#f} (not the\n"
	    "empty list) is returned.")
#define FUNC_NAME s_scm_member
{
  SCM_VALIDATE_LIST (2, lst);
  for (; !SCM_NULL_OR_NIL_P (lst); lst = SCM_CDR (lst))
    {
      if (! scm_is_false (scm_equal_p (SCM_CAR (lst), x)))
	return lst;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


/* deleting elements from a list (delq, etc.) */

SCM_DEFINE (scm_delq_x, "delq!", 2, 0, 0,
           (SCM item, SCM lst),
	    "@deffnx {Scheme Procedure} delv! item lst\n"
	    "@deffnx {Scheme Procedure} delete! item lst\n"
	    "These procedures are destructive versions of @code{delq}, @code{delv}\n"
	    "and @code{delete}: they modify the existing @var{lst}\n"
	    "rather than creating a new list.  Caveat evaluator: Like other\n"
	    "destructive list functions, these functions cannot modify the binding of\n"
	    "@var{lst}, and so cannot be used to delete the first element of\n"
	    "@var{lst} destructively.")
#define FUNC_NAME s_scm_delq_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (scm_is_eq (SCM_CAR (walk), item))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv_x, "delv!", 2, 0, 0,
	    (SCM item, SCM lst),
	    "Destructively remove all elements from @var{lst} that are\n"
	    "@code{eqv?} to @var{item}.")
#define FUNC_NAME s_scm_delv_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (! scm_is_false (scm_eqv_p (SCM_CAR (walk), item)))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME



SCM_DEFINE (scm_delete_x, "delete!", 2, 0, 0,
	    (SCM item, SCM lst),
	    "Destructively remove all elements from @var{lst} that are\n"
	    "@code{equal?} to @var{item}.")
#define FUNC_NAME s_scm_delete_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (! scm_is_false (scm_equal_p (SCM_CAR (walk), item)))
	*prev = SCM_CDR (walk);
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME





SCM_DEFINE (scm_delq, "delq", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{eq?} to @var{item} removed.  This procedure mirrors\n"
	    "@code{memq}: @code{delq} compares elements of @var{lst} against\n"
	    "@var{item} with @code{eq?}.")
#define FUNC_NAME s_scm_delq
{
  SCM copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delv, "delv", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{eqv?}  to @var{item} removed.  This procedure mirrors\n"
	    "@code{memv}: @code{delv} compares elements of @var{lst} against\n"
	    "@var{item} with @code{eqv?}.")
#define FUNC_NAME s_scm_delv
{
  SCM copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}
#undef FUNC_NAME

SCM_DEFINE (scm_delete, "delete", 2, 0, 0,
            (SCM item, SCM lst),
	    "Return a newly-created copy of @var{lst} with elements\n"
	    "@code{equal?}  to @var{item} removed.  This procedure mirrors\n"
	    "@code{member}: @code{delete} compares elements of @var{lst}\n"
	    "against @var{item} with @code{equal?}.")
#define FUNC_NAME s_scm_delete
{
  SCM copy = scm_list_copy (lst);
  return scm_delete_x (item, copy);
}
#undef FUNC_NAME


SCM_DEFINE (scm_delq1_x, "delq1!", 2, 0, 0,
           (SCM item, SCM lst),
	    "Like @code{delq!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{eq?}.  See also @code{delv1!} and @code{delete1!}.")
#define FUNC_NAME s_scm_delq1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (scm_is_eq (SCM_CAR (walk), item))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delv1_x, "delv1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like @code{delv!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{eqv?}.  See also @code{delq1!} and @code{delete1!}.")
#define FUNC_NAME s_scm_delv1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (! scm_is_false (scm_eqv_p (SCM_CAR (walk), item)))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }
    
  return lst;
}
#undef FUNC_NAME


SCM_DEFINE (scm_delete1_x, "delete1!", 2, 0, 0,
            (SCM item, SCM lst),
	    "Like @code{delete!}, but only deletes the first occurrence of\n"
	    "@var{item} from @var{lst}.  Tests for equality using\n"
	    "@code{equal?}.  See also @code{delq1!} and @code{delv1!}.")
#define FUNC_NAME s_scm_delete1_x
{
  SCM walk;
  SCM *prev;

  for (prev = &lst, walk = lst;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (! scm_is_false (scm_equal_p (SCM_CAR (walk), item)))
	{
	  *prev = SCM_CDR (walk);
	  break;
	}
      else
	prev = SCM_CDRLOC (walk);
    }

  return lst;
}
#undef FUNC_NAME

SCM_DEFINE (scm_filter, "filter", 2, 0, 0,
	    (SCM pred, SCM list),
	    "Return all the elements of 2nd arg @var{list} that satisfy predicate @var{pred}.\n"
	    "The list is not disordered -- elements that appear in the result list occur\n"
	    "in the same order as they occur in the argument list. The returned list may\n"
	    "share a common tail with the argument list. The dynamic order in which the\n"
	    "various applications of pred are made is not specified.\n\n"
	    "@lisp\n"
	    "(filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_filter
{
  scm_t_trampoline_1 call = scm_trampoline_1 (pred);
  SCM walk;
  SCM *prev;
  SCM res = SCM_EOL;
  SCM_ASSERT (call, pred, 1, FUNC_NAME);
  SCM_VALIDATE_LIST (2, list);
  
  for (prev = &res, walk = list;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (scm_is_true (call (pred, SCM_CAR (walk))))
	{
	  *prev = scm_cons (SCM_CAR (walk), SCM_EOL);
	  prev = SCM_CDRLOC (*prev);
	}
    }

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_filter_x, "filter!", 2, 0, 0,
	    (SCM pred, SCM list),
	    "Linear-update variant of @code{filter}.")
#define FUNC_NAME s_scm_filter_x
{
  scm_t_trampoline_1 call = scm_trampoline_1 (pred);
  SCM walk;
  SCM *prev;
  SCM_ASSERT (call, pred, 1, FUNC_NAME);
  SCM_VALIDATE_LIST (2, list);
  
  for (prev = &list, walk = list;
       scm_is_pair (walk);
       walk = SCM_CDR (walk))
    {
      if (scm_is_true (call (pred, SCM_CAR (walk))))
	prev = SCM_CDRLOC (walk);
      else
	*prev = SCM_CDR (walk);
    }

  return list;
}
#undef FUNC_NAME


void
scm_init_list ()
{
#include "libguile/list.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
