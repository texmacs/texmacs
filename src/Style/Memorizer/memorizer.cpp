
/******************************************************************************
* MODULE     : memorizer.cpp
* DESCRIPTION: memorizing computations during style rewriting
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "memorizer.hpp"
#include "environment.hpp"

/******************************************************************************
* Default implementations
******************************************************************************/

int memorizer_count= 0;

void
memorizer_rep::compute () {
  FAILED ("memorizer can't compute its own output");
}

void
memorizer_rep::set_children (memorizer* a, int n) {
  (void) a; (void) n;
  FAILED ("not a compound memorizer");
}

void
memorizer_rep::get_children (memorizer*& a, int& n) {
  a= NULL;
  n= 0;
}

void
memorizer_rep::set_environment (environment env) {
  (void) env;
  FAILED ("not an environment memorizer");
}

environment
memorizer_rep::get_environment () {
  FAILED ("not an environment memorizer");
  return environment ();
}

void
memorizer_rep::set_tree (tree t) {
  (void) t;
  FAILED ("not a tree memorizer");
}

tree
memorizer_rep::get_tree () {
  FAILED ("not a tree memorizer");
  return "";
}

/******************************************************************************
* Compound memorizers
******************************************************************************/

void
compound_memorizer_rep::set_children (memorizer* a2, int n2) {
  if (n != 0) {
    n= 0;
    tm_delete_array (a);
  }
  if (n2 != 0) {
    n= n2;
    a= tm_new_array<memorizer> (n);
    for (int i=0; i<n; i++)
      a[i]= a2[i];
  }
}

void
compound_memorizer_rep::get_children (memorizer*& a2, int& n2) {
  a2= a;
  n2= n;
}

void
print_tree (memorizer mem) {
  mem->print (cout);
  cout << LF;
  memorizer* a;
  int n;
  mem->get_children (a, n);
  if (n != 0) {
    cout << INDENT;
    for (int i=0; i<n; i++)
      print_tree (a[i]);
    cout << UNINDENT;
  }
}

/******************************************************************************
* Management of children
******************************************************************************/

static int  mem_cur;
static int  mem_max_pos;
static int* mem_pos;
static int  mem_max_stack;
static memorizer* mem_stack;

template<typename T> void
double_size (T*& in, int& size) {
  T* out= tm_new_array<T> (2*size);
  for (int i=0; i<size; i++)
    out[i]= in[i];
  tm_delete_array (in);
  in= out;
  size *= 2;
}

void
memorize_initialize () {
  cout << "Memorize initialize" << INDENT << LF;
  mem_max_pos  = 16;
  mem_pos      = tm_new_array<int> (mem_max_pos);
  mem_max_stack= 16;
  mem_stack    = tm_new_array<memorizer> (mem_max_stack);
  mem_cur      = 0;
  mem_pos[0]   = 0;
}

memorizer
memorize_finalize () {
  cout << UNINDENT << "Memorize finalize" << LF;
  memorizer mem= mem_stack[0];
  tm_delete_array (mem_pos);
  tm_delete_array (mem_stack);
  mem_max_pos  = 0;
  mem_pos      = NULL;
  mem_max_stack= 0;
  mem_stack    = NULL;
  return mem;
}

void
memorize_start () {
  //cout << "Memorize start" << INDENT << LF;
  mem_cur++;
  if (mem_cur == mem_max_pos)
    double_size (mem_pos, mem_max_pos);
  mem_pos[mem_cur]= mem_pos[mem_cur-1];
}

void
memorize_end () {
  mem_cur--;
  int start= mem_pos[mem_cur], end= mem_pos[mem_cur+1];
  //cout << UNINDENT << "Memorize end [" << start << ", " << end << "]" << LF;
  mem_stack[start-1]->set_children (mem_stack + start, end - start);
}

/******************************************************************************
* Global memorization
******************************************************************************/

typedef memorizer_rep* memorizer_ptr;
typedef memorizer_rep** memorizer_ptrs;

static int bigmem_size= 0;
static int bigmem_bags= 0; // zero or power of two
static int bigmem_bags_mask= bigmem_bags-1;
static memorizer_ptrs* bigmem_mem= NULL;
static int* bigmem_len= NULL;
static int* bigmem_cap= NULL; // elements are zeros or powers of two

static memorizer_ptr
bigmem_insert (memorizer_ptr ptr) {
  if (bigmem_size >= bigmem_bags) {
    if (bigmem_bags == 0) {
      bigmem_bags= 2;
      //bigmem_bags= 1<<24;
      //cout << "Construct bigmem " << bigmem_bags << LF;
      bigmem_mem = tm_new_array<memorizer_ptrs> (bigmem_bags);
      bigmem_len = tm_new_array<int> (bigmem_bags);
      bigmem_cap = tm_new_array<int> (bigmem_bags);
      for (int i=0; i<bigmem_bags; i++) {
	bigmem_mem[i]= NULL;
	bigmem_len[i]= 0;
	bigmem_cap[i]= 0;
      }
    }
    else {
      int new_bags= bigmem_bags << 1;
      //cout << "Larger bigmem " << new_bags << LF;
      int new_bags_mask= new_bags-1;
      memorizer_ptrs* new_mem= tm_new_array<memorizer_ptrs> (new_bags);
      int* new_len= tm_new_array<int> (new_bags);
      int* new_cap= tm_new_array<int> (new_bags);
      for (int i=0; i<bigmem_bags; i++) {
	int j= i+ bigmem_bags;
	int len= bigmem_len[i];
	int cap= bigmem_cap[i];
	//cout << "bag " << i << ": " << len << ", " << cap << LF;
	if (len == 0) cap= 0;
	else while ((len<<1) <= cap && cap >= 4) cap= cap>>1;
	new_len[i]= 0;
	new_len[j]= 0;
	new_cap[i]= cap;
	new_cap[j]= cap;
	if (cap == 0) {
	  new_mem[i]= NULL;
	  new_mem[j]= NULL;
	}
	else {
	  new_mem[i]= tm_new_array<memorizer_ptr> (cap);
	  new_mem[j]= tm_new_array<memorizer_ptr> (cap);
	  for (int k=0; k<len; k++) {
	    memorizer_ptr mem_ptr= bigmem_mem[i][k];
	    int h= mem_ptr->hash () & new_bags_mask;
	    //cout << "  insert " << h << LF;
	    ASSERT (h == i || h == j, "failed assertion");
	    new_mem[h][new_len[h]++]= mem_ptr;
	  }
	}
	if (cap != 0) tm_delete_array (bigmem_mem[i]);
      }
      tm_delete_array (bigmem_mem);
      tm_delete_array (bigmem_len);
      tm_delete_array (bigmem_cap);
      bigmem_bags= new_bags;
      bigmem_mem = new_mem;
      bigmem_len = new_len;
      bigmem_cap = new_cap;
    }
    bigmem_bags_mask= bigmem_bags-1;
  }

  int h= ptr->hash () & bigmem_bags_mask;
  memorizer_ptrs a= bigmem_mem[h];
  int i, len= bigmem_len[h];
  for (i=0; i<len; i++)
    if (a[i]->type () == ptr->type () && a[i]->equal (ptr))
      return a[i];
  int cap= bigmem_cap[h];
  if (len >= cap) {
    if (cap == 0) {
      a  = tm_new_array<memorizer_ptr> (2);
      cap= 2;
    }
    else {
      int new_cap= cap<<1;
      memorizer_ptrs b= tm_new_array<memorizer_ptr> (new_cap);
      for (i=0; i<len; i++) b[i]= a[i];
      tm_delete_array (a);
      a  = b;
      cap= new_cap;
    }
    bigmem_mem[h]= a;
    bigmem_cap[h]= cap;
  }
  a[bigmem_len[h]++]= ptr;
  bigmem_size++;
  //cout << "Added [" << bigmem_size << ", " << ptr << "] ";
  //ptr->print (cout);
  //cout << LF;
  return ptr;
}

static void
bigmem_remove (memorizer_ptr ptr) {
  int h= ptr->hash () & bigmem_bags_mask;
  memorizer_ptrs a= bigmem_mem[h];
  int i, len= bigmem_len[h];
  for (i=0; i<len; i++)
    if (a[i] == ptr) {
      len--;
      a[i]= a[len];
      bigmem_len[h]= len;
      bigmem_size--;
      //cout << "Removed [" << bigmem_size << ", " << ptr << "] ";
      //ptr->print (cout);
      //cout << LF;
      return;
    }
  FAILED ("pointer not found");
}

memorizer::memorizer (memorizer_rep* ptr) {
  rep= bigmem_insert (ptr);
  //cout << "construct " << ptr << " -> " << rep << LF;
  if (rep != ptr) tm_delete (ptr);
  //cout << "  set " << mem_pos[mem_cur] << ": " << rep << LF;
  memorizer_rep*& old_rep (mem_stack[mem_pos[mem_cur]++].rep);
  if (rep == old_rep) rep->ref_count++;
  else {
    if (old_rep != NULL) {
      old_rep->ref_count--;
      if (old_rep->ref_count == 0) tm_delete (old_rep);
    }
    old_rep= rep;
    rep->ref_count += 2;
  }
  if (mem_pos[mem_cur] == mem_max_stack)
    double_size (mem_stack, mem_max_stack);
}

memorizer::~memorizer () {
  if (rep != NULL) {
    rep->ref_count--;
    //cout << "destroy " << rep << ", " << rep->ref_count << LF;
    if (rep->ref_count == 0) {
      bigmem_remove (rep);
      tm_delete (rep);
    }
  }
}

memorizer&
memorizer::operator = (memorizer mem) {
  //cout << "assign " << rep << ", " << mem.rep << LF;
  if (rep == mem.rep) return *this;
  if (rep != NULL) {
    rep->ref_count--;
    //cout << "refcount " << rep << ", " << rep->ref_count << "\n";
    if (rep->ref_count == 0) {
      bigmem_remove (rep);
      tm_delete (rep);
    }
  }
  rep= mem.rep;
  if (rep != NULL) rep->ref_count++;
  return *this;
}
