
/******************************************************************************
* MODULE     : memorizer.hpp
* DESCRIPTION: memorizing computations during style rewriting
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MEMORIZER_H
#define MEMORIZER_H
#include "tree.hpp"

class environment;

#define MEMORIZE_EVALUATE      0
#define MEMORIZE_REWRITE       1
#define MEMORIZE_INACTIVE      2

#define MEMORIZE_ASSIGN        10
#define MEMORIZE_BEGIN_WITH    11
#define MEMORIZE_END_WITH      12
#define MEMORIZE_MACRO_DOWN    13
#define MEMORIZE_MACRO_REDOWN  14
#define MEMORIZE_MACRO_UP      15

/******************************************************************************
* Abstract memorizers
******************************************************************************/

extern int memorizer_count;
class memorizer;
class memorizer_rep: public abstract_struct {
public:
  inline memorizer_rep () { TM_DEBUG (memorizer_count++); }
  inline virtual ~memorizer_rep () { TM_DEBUG (memorizer_count--); }

  virtual void print (tm_ostream& out) = 0;
  virtual int  type () = 0;
  virtual int  hash () = 0;
  virtual bool equal (memorizer_rep* mem) = 0;

  virtual void compute ();
  virtual void set_children (memorizer* a, int n);
  virtual void get_children (memorizer*& a, int& n);
  virtual void set_tree (tree t);
  virtual tree get_tree ();
  virtual void set_environment (environment env);
  virtual environment get_environment ();
};

class memorizer {
public: // should rebecome private
  memorizer_rep* rep;
public:
  inline memorizer (): rep (NULL) {}
  inline memorizer (const memorizer& mem): rep (mem.rep) {
    if (rep != NULL) {
      //cout << "copy " << rep << LF;
      rep->ref_count++; } }
  memorizer (memorizer_rep* rep);
  ~memorizer ();
  memorizer& operator = (memorizer mem);
  inline memorizer_rep* operator -> () { return rep; }
  inline friend bool is_memorized (const memorizer& mem) {
    return mem.rep->ref_count >= 3; }
  inline friend bool operator == (memorizer o1, memorizer o2) {
    return o1.rep->type () == o2.rep->type () && o1.rep->equal (o2.rep); }
  inline friend bool operator != (memorizer o1, memorizer o2) {
    return o1.rep->type () != o2.rep->type () || !o1.rep->equal (o2.rep); }
  inline friend int hash (memorizer o1) {
    return o1.rep->hash (); }
  inline friend tm_ostream& operator << (tm_ostream& out, memorizer mem) {
    mem->print (out); return out; }
};

/******************************************************************************
* Compound memorizers
******************************************************************************/

class compound_memorizer_rep: public memorizer_rep {
protected:
  memorizer* a;
  int n;
public:
  inline compound_memorizer_rep (): memorizer_rep (), a (NULL), n (0) {}
  inline ~compound_memorizer_rep () { if (n!=0) tm_delete_array (a); }
  void set_children (memorizer* a, int n);
  void get_children (memorizer*& a, int& n);
};

void print_tree (memorizer mem);

/******************************************************************************
* Public interface
******************************************************************************/

void memorize_initialize ();
memorizer memorize_finalize ();
void memorize_start ();
void memorize_end ();

#endif // defined MEMORIZER_H
