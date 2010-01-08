
/******************************************************************************
* MODULE     : std_environment.cpp
* DESCRIPTION: environments for standard TeXmacs style rewriting mechanism
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "list_environment.hpp"
#include "memorizer.hpp"
#include "iterator.hpp"

/******************************************************************************
* Standard environments
******************************************************************************/

class std_environment_rep;
class std_environment {
  ABSTRACT_NULL(std_environment);
  inline std_environment
    (bool pure, list_environment env, std_environment next,
     list_environment accel, list_environment args);
  inline friend environment as_environment (const std_environment& env) {
    return environment ((environment_rep*) env.rep); }
  inline friend std_environment as_std_environment (const environment& env) {
    return std_environment ((std_environment_rep*) as_pointer (env)); }
  inline friend int weak_hash (std_environment env) {
    return hash ((void*) env.rep); }
  inline friend bool weak_equal (std_environment env1, std_environment env2) {
    return env1.rep == env2.rep; }
};

class std_environment_rep: public environment_rep {
public:
  bool             pure;   // pure environment or allow side effects?
  list_environment env;    // the local variables
  std_environment  next;   // the next environment
  list_environment accel;  // accelerated access to environment
  list_environment args;   // recursive macro arguments environment

public:
  inline std_environment_rep (bool pure2,
			      list_environment env2,
			      std_environment next2,
			      list_environment accel2,
			      list_environment args2):
    pure (pure2), env (env2), next (next2), accel (accel2), args (args2) {}

  inline bool contains (int key) {
    return accel->contains (key); }
  inline tree read (int key) {
    return accel->read (key); }
  inline void write (int key, const tree& val) {
    env->write (key, val);
    accel->write (key, val); }
  inline void remove (int key) {
    // NOTE: it is not allowed to change next, in the case
    // when 'key' does not exist in the std environment 'env'
    env->remove (key);
    accel->remove (key); }
  void print (const string& prefix);
};

inline std_environment::std_environment
  (bool pure, list_environment env, std_environment next,
   list_environment accel, list_environment args):
     rep (tm_new<std_environment_rep> (pure, env, next, accel, args)) {}
ABSTRACT_NULL_CODE(std_environment);

void
std_environment_rep::print (const string& prefix) {
  cout << prefix << "Std environment" << LF;
  env->print (prefix * (pure? string ("|* "): string ("|  ")));
  std_environment it= next;
  while (!is_nil (it)) {
    it->env->print (prefix * (it->pure? string ("|* "): string ("|  ")));
    it= it->next;
  }
  if (total_size (args) != 0)
    args->print (prefix * "|o ");
}

/******************************************************************************
* Primitive environments
******************************************************************************/

std_environment
primitive_environment (basic_environment global) {
  list_environment env (global);
  return std_environment (false, env, std_environment (),
			  env, list_environment ());
}

std_environment
primitive_environment (hashmap<string,tree> h) {
  basic_environment global (1);
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    string key= it->next();
    global->write ((int) make_tree_label (key), copy (h[key]));
  }
  return primitive_environment (global);
}

void
primitive (environment& env, hashmap<string,tree> h) {
  env= as_environment (primitive_environment (h));
  //cout << HRULE;
  //env->print ("");
}

/******************************************************************************
* Assignments
******************************************************************************/

std_environment
assign_environment (std_environment env, basic_environment local) {
  list_environment accel (local, env->accel);
  if (env->pure) return std_environment (false, local, env, accel, env->args);
  list_environment merged (local, env->env);
  return std_environment (false, merged, env->next, accel, env->args);
}

class assign_memorizer_rep: public memorizer_rep {
  std_environment   in;
  assoc_environment local;
  std_environment   out;
  int               h;

public:
  inline assign_memorizer_rep (environment env, assoc_environment ch):
    in (as_std_environment (env)), local (ch), out (in),
    h (weak_hash (env) ^ weak_hash (ch)) {}

  void print (tm_ostream& out) { out << "assign_memorizer"; }
  int type () { return MEMORIZE_ASSIGN; }
  int hash () { return h; }
  bool equal (memorizer_rep* mem) {
    assign_memorizer_rep* rep= (assign_memorizer_rep*) mem;
    return weak_equal (in, rep->in) && weak_equal (local, rep->local); }
  void compute () {
    out= assign_environment (in, basic_environment (local)); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

void
assign (environment& env, assoc_environment local) {
  memorizer mem= tm_new<assign_memorizer_rep> (env, local);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

/******************************************************************************
* Starting local environments
******************************************************************************/

std_environment
begin_with_environment (std_environment env, basic_environment local) {
  list_environment accel (local, env->accel);
  return std_environment (true, local, env, accel, env->args);
}

class begin_with_memorizer_rep: public memorizer_rep {
  std_environment   in;
  assoc_environment local;
  std_environment   out;
  int               h;

public:
  inline begin_with_memorizer_rep (environment env, assoc_environment ch):
    in (as_std_environment (env)), local (ch), out (in),
    h (weak_hash (env) ^ weak_hash (ch)) {}

  void print (tm_ostream& out) { out << "begin_with_memorizer"; }
  int type () { return MEMORIZE_BEGIN_WITH; }
  int hash () { return h; }
  bool equal (memorizer_rep* mem) {
    begin_with_memorizer_rep* rep= (begin_with_memorizer_rep*) mem;
    return weak_equal (in, rep->in) && weak_equal (local, rep->local); }
  void compute () {
    out= begin_with_environment (in, basic_environment (local)); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

void
begin_with (environment& env, assoc_environment local) {
  memorizer mem= tm_new<begin_with_memorizer_rep> (env, local);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

/******************************************************************************
* Closing local environments
******************************************************************************/

std_environment
end_with_environment (std_environment env) {
  if (env->pure) return env->next;
  ASSERT (env->next->pure, "pure environment expected");
  basic_environment patch= flatten (env->env);
  basic_environment with_env= flatten (env->next->env);
  patch->multiple_remove (with_env->a, with_env->n);
  if (patch->size == 0) return env->next->next;
  while (patch->size < (patch->n>>1))
    patch->resize (patch->n >> 1);
  std_environment up= env->next->next;
  std_environment aux (up->pure, up->env, up->next, up->accel, env->args);
  return assign_environment (aux, patch);
}

class end_with_memorizer_rep: public memorizer_rep {
  std_environment   in;
  std_environment   out;

public:
  inline end_with_memorizer_rep (environment env):
    in (as_std_environment (env)), out (in) {}

  void print (tm_ostream& out) { out << "end_with_memorizer"; }
  int type () { return MEMORIZE_END_WITH; }
  int hash () { return weak_hash (in); }
  bool equal (memorizer_rep* mem) {
    end_with_memorizer_rep* rep= (end_with_memorizer_rep*) mem;
    return weak_equal (in, rep->in); }
  void compute () {
    out= end_with_environment (in); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

void
end_with (environment& env) {
  memorizer mem= tm_new<end_with_memorizer_rep> (env);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

/******************************************************************************
* Adding an additional macro level
******************************************************************************/

#ifdef CLASSICAL_MACRO_EXPANSION

std_environment
macro_down_environment (std_environment env, basic_environment local) {
  list_environment args (local, env->args);
  return std_environment (env->pure, env->env, env->next, env->accel, args);
}

class macro_down_memorizer_rep: public memorizer_rep {
  std_environment   in;
  assoc_environment local;
  std_environment   out;
  int               h;

public:
  inline macro_down_memorizer_rep (environment env, assoc_environment ch):
    in (as_std_environment (env)), local (ch), out (in),
    h (weak_hash (env) ^ weak_hash (ch)) {}

  void print (tm_ostream& out) { out << "macro_down_memorizer"; }
  int type () { return MEMORIZE_MACRO_DOWN; }
  int hash () { return h; }
  bool equal (memorizer_rep* mem) {
    macro_down_memorizer_rep* rep= (macro_down_memorizer_rep*) mem;
    return weak_equal (in, rep->in) && weak_equal (local, rep->local); }
  void compute () {
    out= macro_down_environment (in, basic_environment (local)); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

class macro_redown_memorizer_rep: public memorizer_rep {
  std_environment   in;
  basic_environment local;
  std_environment   out;
  int               h;

public:
  inline macro_redown_memorizer_rep (environment env, basic_environment ch):
    in (as_std_environment (env)), local (ch), out (in),
    h (weak_hash (env) ^ weak_hash (as_environment (ch))) {}

  void print (tm_ostream& out) { out << "macro_redown_memorizer"; }
  int type () { return MEMORIZE_MACRO_DOWN; }
  int hash () { return h; }
  bool equal (memorizer_rep* mem) {
    macro_redown_memorizer_rep* rep= (macro_redown_memorizer_rep*) mem;
    return weak_equal (in, rep->in) &&
           weak_equal (as_environment (local), as_environment (rep->local)); }
  void compute () {
    out= macro_down_environment (in, local); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

void
macro_down (environment& env, assoc_environment local) {
  memorizer mem= tm_new<macro_down_memorizer_rep> (env, local);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

void
macro_redown (environment& env, basic_environment local) {
  memorizer mem= tm_new<macro_redown_memorizer_rep> (env, local);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

bool
macro_top_level (environment& env) {
  std_environment std= as_std_environment (env);
  return is_nil (std->args);
}

basic_environment
macro_arguments (environment& env) {
  std_environment std= as_std_environment (env);
  return std->args->env;
}

#endif // CLASSICAL_MACRO_EXPANSION

/******************************************************************************
* Remove the uppermost macro level
******************************************************************************/

#ifdef CLASSICAL_MACRO_EXPANSION

std_environment
macro_up_environment (std_environment env) {
  return std_environment (env->pure, env->env, env->next, env->accel,
			  env->args->next);
}

class macro_up_memorizer_rep: public memorizer_rep {
  std_environment   in;
  std_environment   out;

public:
  inline macro_up_memorizer_rep (environment env):
    in (as_std_environment (env)), out (in) {}

  void print (tm_ostream& out) { out << "macro_up_memorizer"; }
  int type () { return MEMORIZE_MACRO_UP; }
  int hash () { return weak_hash (in); }
  bool equal (memorizer_rep* mem) {
    macro_up_memorizer_rep* rep= (macro_up_memorizer_rep*) mem;
    return weak_equal (in, rep->in); }
  void compute () {
    out= macro_up_environment (in); }
  void set_environment (environment out2) {
    out= as_std_environment (out2); }
  environment get_environment () {
    return as_environment (out); }
};

void
macro_up (environment& env) {
  memorizer mem= tm_new<macro_up_memorizer_rep> (env);
  if (!is_memorized (mem)) mem->compute ();
  env= mem->get_environment ();
}

#endif // CLASSICAL_MACRO_EXPANSION

/******************************************************************************
* Testing the code for environments
******************************************************************************/

assoc_environment
assoc (int key1, const tree& val1) {
  assoc_environment env (1);
  env->raw_write (0, key1, val1);
  return env;
}

assoc_environment
assoc (int key1, const tree& val1, int key2, const tree& val2) {
  assoc_environment env (2);
  env->raw_write (0, key1, val1);
  env->raw_write (1, key2, val2);
  return env;
}

basic_environment
test_environment (int n, int code) {
  array<string> s (8);
  s[0]= "Hallo"; s[1]= "Hop"; s[2]= "Blah"; s[3]= "Holala";
  s[4]= "Ploef"; s[5]= "Blurk"; s[6]= "Blauwbilgorgel"; s[7]= "Vrolijk";
  int modulo= 32;
  while (modulo<n) modulo <<= 1;
  basic_environment env (1);
  for (int i=0; i<n; i++) {
    int i1= code%8, i2= (3*code)%8;
    if (i1 == i2) i2= (i2+1)%8;
    string s1= copy (s[i1]), s2= copy (s[i2]);
    env->write (code % modulo, s1);
    int j1= code % N(s1), j2= code % N(s2);
    s[i1]= s2 (0, j2) * s1 (j1, N(s1));
    s[i2]= s1 (0, j1) * s2 (j2, N(s2));
    code= code*code % 176246173;
    if (code < 0) code= -code;
    while (env->contains (code % modulo)) code= (code+1) % 176246173;
  }
  return env;
}

void
test_environments () {
  assoc_environment a1= assoc (6, "Sylvie", 9, "Nicole");
  assoc_environment a2= assoc (9, "Joris");
  assoc_environment a3= assoc (21, "Judith");
  assoc_environment a4= assoc (9, "Judith", 6, "Tessa");
  assoc_environment a5= assoc (9, "Piet", 21, "Joris");

  basic_environment b1= test_environment (23, 1234567);
  std_environment std1= primitive_environment (b1);
  std1->print (""); cout << HRULE;
  std1->accel->print (""); cout << HRULE;
  std_environment std2= begin_with_environment (std1, a1);
  std2->print (""); cout << HRULE;
  std2->accel->print (""); cout << HRULE;
  std_environment std3= assign_environment (std2, a5);
  std3->print (""); cout << HRULE;
  std3->accel->print (""); cout << HRULE;
  std_environment std4= assign_environment (std3, a3);
  std4->print (""); cout << HRULE;
  std4->accel->print (""); cout << HRULE;
  std_environment std5= begin_with_environment (std4, a4);
  std5->print (""); cout << HRULE;
  std5->accel->print (""); cout << HRULE;
  std_environment std6= assign_environment (std5, a2);
  std6->print (""); cout << HRULE;
  std6->accel->print (""); cout << HRULE;
  for (int i=0; i<10; i++)
    cout << i << "\t" << std6->read (i) << "\n";
  cout << HRULE;
  std6->accel->print (""); cout << HRULE;
  for (int i=0; i<10; i++)
    cout << i << "\t" << std6->read (i) << "\n";
  cout << HRULE;
  std6->accel->print (""); cout << HRULE;
  for (int i=0; i<40; i++)
    cout << i << "\t" << std6->read (i) << "\n";
  cout << HRULE;
  std6->accel->print (""); cout << HRULE;
  std_environment std7= end_with_environment (std6);
  std7->print (""); cout << HRULE;
  std7->accel->print (""); cout << HRULE;
  std_environment std8= end_with_environment (std7);
  std8->print (""); cout << HRULE;
  std8->accel->print (""); cout << HRULE;
}
