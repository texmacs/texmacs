
/******************************************************************************
* MODULE     : patch.cpp
* DESCRIPTION: Routines on patches
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "patch.hpp"

int insert_length (tree t);

/******************************************************************************
* Concrete patches
******************************************************************************/

class modification_patch_rep: public patch_rep {
  modification mod;
  modification inv;
public:
  modification_patch_rep (modification mod2, modification inv2):
    mod (mod2), inv (inv2) {}
  int get_type () { return PATCH_MODIFICATION; }
  modification get_modification () { return mod; }
  modification get_inverse () { return inv; }
};

class compound_patch_rep: public patch_rep {
  array<patch> a;
public:
  compound_patch_rep (array<patch> a2): a (a2) {}
  int get_type () { return PATCH_COMPOUND; }
  int get_arity () { return N(a); }
  patch get_child (int i) { return a[i]; }
};

class branch_patch_rep: public patch_rep {
  array<patch> a;
public:
  branch_patch_rep (array<patch> a2): a (a2) {}
  int get_type () { return PATCH_BRANCH; }
  int get_arity () { return N(a); }
  patch get_child (int i) { return a[i]; }
};

class birth_patch_rep: public patch_rep {
  double author;
  bool birth;
public:
  birth_patch_rep (double a2, bool b2): author (a2), birth (b2) {}
  int get_type () { return PATCH_BIRTH; }
  double get_author () { return author; }
  bool get_birth () { return birth; }
};

class author_patch_rep: public patch_rep {
  double author;
  patch p;
public:
  author_patch_rep (double a2, patch p2): author (a2), p (p2) {}
  int get_type () { return PATCH_AUTHOR; }
  int get_arity () { return 1; }
  patch get_child (int i) {
    ASSERT (i == 0, "out of range");
    return p; }
  double get_author () { return author; }
};

patch::patch (modification mod, modification inv):
  rep (tm_new<modification_patch_rep> (mod, inv)) { rep->ref_count= 1; }
patch::patch (array<patch> a):
  rep (tm_new<compound_patch_rep> (a)) { rep->ref_count= 1; }
patch::patch (bool branch, array<patch> a):
  rep (branch?
       ((patch_rep*) tm_new<branch_patch_rep> (a)):
       ((patch_rep*) tm_new<compound_patch_rep> (a))) { rep->ref_count= 1; }
patch::patch (patch p1, patch p2):
  rep (tm_new<compound_patch_rep> (array<patch>(p1,p2))) { rep->ref_count= 1; }
patch::patch (double author, bool create):
  rep (tm_new<birth_patch_rep> (author, create)) { rep->ref_count= 1; }
patch::patch (double author, patch p):
  rep (tm_new<author_patch_rep> (author, p)) { rep->ref_count= 1; }

/******************************************************************************
* Internal subroutines
******************************************************************************/

array<patch>
singleton (patch p) {
  array<patch> a (1);
  a[0]= p;
  return a;
}

static array<patch>
get_array (patch p) {
  int i, n= N(p);
  array<patch> a (n);
  for (i=0; i<N(p); i++) a[i]= p[i];
  return a;
}

int
nr_children (patch p) {
  if (get_type (p) != PATCH_COMPOUND) return 1;
  else return N(p);
}

patch
child (patch p, int i) {
  ASSERT (0 <= i && i < nr_children (p), "index out of range");
  if (get_type (p) != PATCH_COMPOUND) return p;
  else return p[i];
}

array<patch>
children (patch p) {
  if (get_type (p) != PATCH_COMPOUND) return singleton (p);
  else return get_array (p);
}

array<patch>
children (patch p, int i, int j) {
  ASSERT (0 <= i && i <= nr_children (p), "index out of range");
  ASSERT (i <= j && j <= nr_children (p), "index out of range");
  return range (children (p), i, j);
}

int
nr_branches (patch p) {
  if (get_type (p) != PATCH_BRANCH) return 1;
  else return N(p);
}

patch
branch (patch p, int i) {
  ASSERT (0 <= i && i < nr_branches (p), "index out of range");
  if (get_type (p) != PATCH_BRANCH) return p;
  else return p[i];
}

array<patch>
branches (patch p) {
  if (get_type (p) != PATCH_BRANCH) return singleton (p);
  else return get_array (p);
}

array<patch>
branches (patch p, int i, int j) {
  ASSERT (0 <= i && i <= nr_branches (p), "index out of range");
  ASSERT (i <= j && j <= nr_branches (p), "index out of range");
  return range (branches (p), i, j);
}

/******************************************************************************
* Author management
******************************************************************************/

static double current_author= 0.0;

double
new_author () {
  static double next_author= 0.0;
  next_author += 1.0;
  return next_author;
}

double
new_marker () {
  static double next_marker= 0.5;
  next_marker += 1.0;
  return next_marker;
}

void
set_author (double a) {
  current_author= a;
}

double
get_author () {
  return current_author;
}

/******************************************************************************
* Common routines
******************************************************************************/

bool
operator == (patch p1, patch p2) {
  if (get_type (p1) != get_type (p2)) return false;
  switch (get_type (p1)) {
  case PATCH_MODIFICATION:
    return get_modification (p1) == get_modification (p2) &&
           get_inverse (p1) == get_inverse (p2);
  case PATCH_COMPOUND:
  case PATCH_BRANCH:
    if (N(p1) != N(p2)) return false;
    for (int i=0; i<N(p1); i++)
      if (p1[i] != p2[i]) return false;
    return true;
  case PATCH_BIRTH:
    return get_birth (p1) == get_birth (p2) &&
           get_author (p1) == get_author (p2);
  case PATCH_AUTHOR:
    return get_author (p1) == get_author (p2) && p1[0] == p2[0];
  default:
    FAILED ("unsupported patch type");
  }
  return false;
}

bool
operator != (patch p1, patch p2) {
  return !(p1 == p2);
}

tm_ostream&
operator << (tm_ostream& out, patch p) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    out << get_modification (p) << " -- " << get_inverse (p);
    break;
  case PATCH_COMPOUND:
    if (N(p) == 0) out << "No children";
    else {
      out << "Composite" << INDENT;
      for (int i=0; i<N(p); i++)
	out << LF << p[i];
      out << UNINDENT;
    }
    break;
  case PATCH_BRANCH:
    if (N(p) == 0) out << "No branches";
    else for (int i=0; i<N(p); i++) {
      if (i != 0) out << LF;
      out << "Branch " << i << INDENT << LF << p[i] << UNINDENT;
    }
    break;
  case PATCH_BIRTH:
    if (get_birth (p)) out << "Birth ";
    else out << "Death ";
    out << get_author (p);
    break;
  case PATCH_AUTHOR:
    out << "Author " << get_author (p) << INDENT << LF;
    out << p[0];
    out << UNINDENT;
    break;
  default:
    FAILED ("unsupported patch type");
  }
  return out;
}

patch
copy (patch p) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return patch (copy (get_modification (p)), copy (get_inverse (p)));
  case PATCH_COMPOUND:
  case PATCH_BRANCH:
    {
      int i, n= N(p);
      array<patch> r (n);
      for (i=0; i<N(p); i++) r[i]= copy (p[i]);
      return patch (get_type (p) == PATCH_BRANCH, r);
    }
  case PATCH_BIRTH:
    return p;
  case PATCH_AUTHOR:
    return patch (get_author (p), copy (p[0]));
  default:
    FAILED ("unsupported patch type");
  }
  return p;
}

/******************************************************************************
* Patch application
******************************************************************************/

bool
is_applicable (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return is_applicable (t, get_modification (p));
  case PATCH_COMPOUND:
    for (int i=0; i<N(p); i++) {
      if (!is_applicable (p[i], t)) return false;
      t= clean_apply (p[i], t);
    }
    return true;
  case PATCH_BRANCH:
  case PATCH_AUTHOR:
    for (int i=0; i<N(p); i++)
      if (!is_applicable (p[i], t))
	return false;
    return true;
  case PATCH_BIRTH:
    return true;
  default:
    FAILED ("unsupported patch type");
    return false;
  }
}

tree
clean_apply (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return clean_apply (t, get_modification (p));
  case PATCH_BRANCH:
    ASSERT (N(p) <= 1, "ambiguous application");
  case PATCH_COMPOUND:
  case PATCH_AUTHOR:
    for (int i=0; i<N(p); i++)
      t= clean_apply (p[i], t);
    return t;
  case PATCH_BIRTH:
    return t;
  default:
    FAILED ("unsupported patch type");
    return t;
  }
}

void
apply (patch p, tree& t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    apply (t, get_modification (p));
    break;
  case PATCH_BRANCH:
    ASSERT (N(p) <= 1, "ambiguous application");
  case PATCH_COMPOUND:
  case PATCH_AUTHOR:
    for (int i=0; i<N(p); i++)
      apply (p[i], t);
    break;
  case PATCH_BIRTH:
    break;
  default:
    FAILED ("unsupported patch type");
  }
}

/******************************************************************************
* Patch inversion
******************************************************************************/

patch
invert (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return patch (get_inverse (p), get_modification (p));
  case PATCH_BRANCH:
    ASSERT (N(p) <= 1, "ambiguous application");
  case PATCH_COMPOUND:
    {
      int i, n=N(p);
      array<patch> r(n);
      for (i=0; i<n; i++) {
	r[n-1-i]= invert (p[i], t);
	t= clean_apply (p[i], t);
      }
      return patch (get_type (p) == PATCH_BRANCH, r);
    }
  case PATCH_BIRTH:
    return patch (get_author (p), !get_birth (p));
  case PATCH_AUTHOR:
    return patch (get_author (p), invert (p[0], t));
  default:
    FAILED ("unsupported patch type");
    return patch ();
  }
}

bool
possible_inverse (modification m1, modification m2) {
  if (root (m1) != root (m2)) return false;
  switch (m1->k) {
  case MOD_ASSIGN:
    return m2->k == MOD_ASSIGN;
  case MOD_INSERT:
    return m2->k == MOD_REMOVE && 
           argument (m2) == insert_length (m1->t);
  case MOD_REMOVE:
    return m2->k == MOD_INSERT && 
           insert_length (m2->t) == argument (m1);
  case MOD_SPLIT:
    return m2->k == MOD_JOIN && 
           index (m2) == index (m1);
  case MOD_JOIN:
    return m2->k == MOD_SPLIT && 
           index (m2) == index (m1);
  case MOD_ASSIGN_NODE:
    return m2->k == MOD_ASSIGN_NODE;
  case MOD_INSERT_NODE:
    return m2->k == MOD_REMOVE_NODE && 
           index (m2) == argument (m1);
  case MOD_REMOVE_NODE:
    return m2->k == MOD_INSERT_NODE && 
           argument (m2) == index (m1);
  case MOD_SET_CURSOR:
    return m1 == m2;
  default:
    FAILED ("invalid situation");
    return false;
  }
}

/******************************************************************************
* Commutation of patches
******************************************************************************/

static bool
swap_basic (patch& p1, patch& p2) {
  patch aux= p1;
  p1= p2;
  p2= aux;
  return true;
}

bool
swap (patch& p1, patch& p2, double a1, double a2) {
  if (is_nil (p1) || is_nil (p2)) return false;
  if (get_type (p1) == PATCH_BRANCH)
    return false;
  if (get_type (p2) == PATCH_BRANCH)
    return false;
  if (get_type (p1) == PATCH_COMPOUND) {
    int n= N(p1);
    array<patch> a (n);
    for (int i=0; i<n; i++) a[i]= p1[i];
    for (int i=n-1; i>=0; i--) {
      if (!swap (a[i], p2, a1, a2)) return false;
      swap_basic (a[i], p2);
    }
    p1= p2;
    p2= patch (a);
    return true;
  }
  if (get_type (p2) == PATCH_COMPOUND) {
    int n= N(p2);
    array<patch> a (n);
    for (int i=0; i<n; i++) a[i]= p2[i];
    for (int i=0; i<n; i++) {
      if (!swap (p1, a[i], a1, a2)) return false;
      swap_basic (p1, a[i]);
    }
    p2= p1;
    p1= patch (a);
    return true;
  }
  if (get_type (p1) == PATCH_AUTHOR) {
    patch s= p1[0];
    bool r= swap (s, p2, get_author (p1), a2);
    p2= patch (get_author (p1), p2);
    p1= s;
    return r;
  }
  if (get_type (p2) == PATCH_AUTHOR) {
    patch s= p2[0];
    bool r= swap (p1, s, a1, get_author (p2));
    p1= patch (get_author (p2), p1);
    p2= s;
    return r;
  }
  if (get_type (p1) == PATCH_BIRTH) {
    if (get_author (p1) == a2) return false;
    return swap_basic (p1, p2);
  }
  if (get_type (p2) == PATCH_BIRTH) {
    if (get_author (p2) == a1) return false;
    return swap_basic (p1, p2);
  }
  if (get_type (p1) == PATCH_MODIFICATION &&
      get_type (p2) == PATCH_MODIFICATION)
    {
      modification m1= get_modification (p1);
      modification m2= get_modification (p2);
      modification i1= get_inverse (p1);
      modification i2= get_inverse (p2);
      bool r= swap (m1, m2);
      bool v= swap (i2, i1);
      p1= patch (m1, i1);
      p2= patch (m2, i2);
      return r && v && possible_inverse (m1, i1) && possible_inverse (m2, i2);
    }
  FAILED ("invalid situation");
  return false;
}

bool
swap (patch& p1, patch& p2) {
  // Assuming that p1;p2 (the patch p1 followed by p2) is well-defined,
  // determine patches p1* and p2* such that p2*;p1* is equivalent
  // to p1;p2.  If such patches exist, then set p1 := p2* and
  // p2 := p1* and return true.  Otherwise, return false
  return swap (p1, p2, 0, 0);
}

bool
commute (patch p1, patch p2) {
  // Assuming that p1;p2 (the patch p1 followed by p2) is well-defined,
  // determine whether there exist patches p1* and p2* such that
  // p2*;p1* is equivalent to p1;p2
  patch s1= p1;
  patch s2= p2;
  return swap (s1, s2);
}

bool
can_pull (patch p1, patch p2) {
  return commute (p2, p1);
}

patch
pull (patch p1, patch p2) {
  // Assuming that p2;p1 (the patch p2 followed by p1) is well-defined,
  // return the patch p1* such that p1;p2 is equivalent to p2*;p1*
  // for a suitable patch p1* (assuming that p1* and p2* exist).
  patch s1= p2;
  patch s2= p1;
  ASSERT (swap (s1, s2), "patch cannot be pulled");
  return s1;
}

patch
co_pull (patch p1, patch p2) {
  // Same as pull, but return p2* instead of p1*
  patch s1= p2;
  patch s2= p1;
  ASSERT (swap (s1, s2), "patch cannot be pulled");
  return s2;
}

/******************************************************************************
* Joining simple patches if possible
******************************************************************************/

bool
is_set_cursor (patch p) {
  return
    is_modification (p) && 
    get_modification (p)->k == MOD_SET_CURSOR;
}

bool
join (patch& p1, patch p2, tree t) {
  //cout << "Join " << p1 << LF << "with " << p2 << LF;
  if (get_type (p1) == PATCH_AUTHOR &&
      get_type (p2) == PATCH_AUTHOR &&
      get_author (p1) == get_author (p2))
    {
      double author= get_author (p1);
      patch q1= p1[0];
      patch q2= p2[0];
      bool r= join (q1, q2, t);
      if (r) p1= patch (author, q1);
      return r;
    }
  if (get_type (p1) == PATCH_MODIFICATION &&
      get_type (p2) == PATCH_MODIFICATION)
    {
      modification m1= get_modification (p1);
      modification m2= get_modification (p2);
      modification i2= get_inverse (p2);
      modification i1= get_inverse (p1);
      bool r= join (m1, m2, t);
      bool v= join (i2, i1, clean_apply (p2, clean_apply (p1, t)));
      if (r && v) p1= patch (m1, i2);
      return r && v;
    }
  if (get_type (p1) == PATCH_COMPOUND &&
      nr_children (p1) > 0 &&
      nr_children (remove_set_cursor (p1)) == 1)
    {
      int nr= nr_children (p1);
      patch p1b= remove_set_cursor (p1);
      if (nr_children (p1b) == 1) {
        bool rf= join (p1b, p2, t);
        if (rf) {
          if (nr >= 2 && is_set_cursor (child (p1, 0))) {
            array<patch> a= range (children (p1), 0, 1);
            array<patch> b= children (p1b);
            p1= patch (append (a, b));
          }
          else p1= p1b;
        }
        return rf;
      }
    }
  if (get_type (p2) == PATCH_COMPOUND &&
      nr_children (p2) > 0 &&
      nr_children (remove_set_cursor (p2)) == 1)
    {
      int nr= nr_children (p2);
      patch p2b= remove_set_cursor (p2);
      if (nr_children (p2b) == 1) {
        bool rf= join (p1, p2b, t);
        if (rf && nr >= 2 && is_set_cursor (child (p2, nr-1))) {
          array<patch> a= children (p1);
          array<patch> b= range (children (p2), nr-1, nr);
          p1= patch (append (a, b));
        }
        return rf;
      }
    }
  return false;
}

/******************************************************************************
* Other routines
******************************************************************************/

void
insert (array<patch>& a, patch p) {
  if (get_type (p) == PATCH_COMPOUND) {
    int i, n= N(p);
    for (i=0; i<n; i++)
      insert (a, p[i]);
  }
  else if (get_type (p) == PATCH_MODIFICATION &&
	   N(a) > 0 &&
	   get_type (a[N(a)-1]) == PATCH_MODIFICATION &&
	   (get_inverse (a[N(a)-1]) == get_modification (p) &&
	    get_modification (a[N(a)-1]) == get_inverse (p)))
    {
      // cout << "Cancel " << a[N(a)-1] << " against " << p << "\n";
      a->resize (N(a) - 1);
    }
  else a << p;
}

patch
compactify (patch p) {
  switch (get_type (p)) {
  case PATCH_COMPOUND:
    {
      double a= 0;
      for (int i=0; i<N(p); i++)
	if (get_type (p[i]) != PATCH_AUTHOR) a= -1;
	else if (a == 0) a= get_author (p[i]);
	else if (a != get_author (p[i])) a= -1;
      if (a <= 0) {
	array<patch> r;
	insert (r, p);
	if (N(r) == 1) return r[0];
	return patch (r);
      }
      else {
	array<patch> r;
	for (int i=0; i<N(p); i++) insert (r, p[i][0]);
	if (N(r) == 1) return patch (a, r[0]);
	return patch (a, patch (r));
      }
    }
  case PATCH_BRANCH:
    if (N(p) == 1) return p[0];
    else {
      int i, n= N(p);
      array<patch> r (n);
      for (i=0; i<n; i++) r[i]= compactify (p[i]);
      return patch (true, r);
    }
  case PATCH_AUTHOR:
    return patch (get_author (p), compactify (p[0]));
  }
  return p;
}

path
cursor_hint (modification m, tree t) {
  ASSERT (is_applicable (t, m), "modification not applicable");
  path rp= root (m);
  tree st= subtree (t, rp);
  switch (m->k) {
  case MOD_ASSIGN:
    return end (t, rp);
  case MOD_INSERT:
    if (is_atomic (st)) return rp * index (m);
    else if (index (m) == N (st)) return end (t, rp);
    else return start (t, rp * index (m));
  case MOD_REMOVE:
    if (is_atomic (st)) return rp * (index (m) + argument (m));
    else if (index (m) == N (st)) return end (t, rp);
    else if (argument (m) == 0) return start (t, rp * index (m));
    else return end (t, rp * (index (m) + argument (m) - 1));
  case MOD_SPLIT:
    if (is_atomic (st [index (m)])) return m->p;
    else if (argument (m) == N (st [index (m)])) return end (t, rp * index(m));
    else return start (t, m->p);
  case MOD_JOIN:
    return end (t, m->p);
  case MOD_ASSIGN_NODE:
    return end (t, rp);
  case MOD_INSERT_NODE:
    return end (t, rp);
  case MOD_REMOVE_NODE:
    return end (t, rp * index (m));
  case MOD_SET_CURSOR:
    return path ();
  default:
    FAILED ("unexpected situation");
    return path ();
  }
}

path
cursor_hint (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return cursor_hint (get_modification (p), t);
  case PATCH_COMPOUND:
    for (int i=0; i<N(p); i++) {
      path r= cursor_hint (p[i], t);
      if (!is_nil (r)) return r;
    }
    return path ();
  case PATCH_BRANCH:
    if (N(p) == 0) return path ();
  case PATCH_AUTHOR:
    return cursor_hint (p[0], t);
  case PATCH_BIRTH:
    return path ();
  default:
    FAILED ("unsupported patch type");
  }
  return path ();
}

patch
remove_set_cursor (patch p) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    if (get_modification (p)->k != MOD_SET_CURSOR) return copy (p);
    return patch (array<patch> ());
  case PATCH_COMPOUND:
    {
      array<patch> r;
      for (int i=0; i<N(p); i++) {
        patch q= remove_set_cursor (p[i]);
        r << children (q);
      }
      if (N(r) == 1) return r[0];
      return patch (r);
    }
  case PATCH_BRANCH:
  case PATCH_BIRTH:
    return copy (p);
  case PATCH_AUTHOR:
    {
      patch q= remove_set_cursor (p[0]);
      if (nr_children (q) == 0) return q;
      else return patch (get_author (p), q);
    }
  default:
    FAILED ("unsupported patch type");
  }
  return p;
}

bool
does_modify (patch p) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return get_modification (p)->k != MOD_SET_CURSOR;
  case PATCH_COMPOUND:
  case PATCH_BRANCH:
    for (int i=0; i<N(p); i++)
      if (does_modify (p[i]))
        return true;
    return false;
  case PATCH_BIRTH:
  case PATCH_AUTHOR:
    return false;
  default:
    return true;
  }
}
