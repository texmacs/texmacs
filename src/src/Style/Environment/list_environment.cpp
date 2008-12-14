
/******************************************************************************
* MODULE     : list_environment.hpp
* DESCRIPTION: linked lists of several environments
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "list_environment.hpp"

tree list_environment_rep::uninit (UNINIT);

/******************************************************************************
* Environment compression: when 'misses' becomes large, the environment
* is automatically compressed so as to reduce subsequent access times
******************************************************************************/

void
list_environment_rep::compress () {
  while (!is_nil (next->next) &&
	 env->size > next->env->size + next->next->env->size)
    next->compress ();
    
  int new_size= env->size+ next->env->size;
  int new_n=2;
  while (new_n < new_size) new_n= new_n<<1;
  basic_environment new_env (new_n);
  new_env->multiple_insert (next->env->a, next->env->n);
  new_env->multiple_write (env->a, env->n);
  misses= 0;
  env   = new_env;
  next  = next->next;
}

/******************************************************************************
* A method for fast read access
******************************************************************************/

tree*
list_environment_rep::raw_read (int key) {
  tree* r= env->raw_read (key);
  if (r != NULL || is_nil (next)) return r;
  misses++;
  if (misses >= env->size + next->env->size) {
    compress ();
    return raw_read (key);
  }
  else return next->raw_read (key);
}

/******************************************************************************
* Printing
******************************************************************************/

void
list_environment_rep::print (const string& prefix) {
  cout << prefix << "List environment" << LF;
  env->print (prefix * "|  ");
  list_environment it= next;
  while (!is_nil (it)) {
    it->env->print (prefix * "|  ");
    it= it->next;
  }
}

/******************************************************************************
* Flattening the environment
******************************************************************************/

int
total_size (list_environment l) {
  if (is_nil (l)) return 0;
  return l->env->size + total_size (l->next);
}

static void
flatten (basic_environment& env, list_environment l) {
  if (is_nil (l->next))
    env->multiple_insert (l->env->a, l->env->n);
  else {
    flatten (env, l->next);
    env->multiple_write (l->env->a, l->env->n);
  }
}

basic_environment
flatten (list_environment l) {
  if (is_nil (l)) return basic_environment (2);
  if (is_nil (l->next)) return l->env;
  int size= total_size (l);
  int n=2;
  while (n < size) n= n<<1;
  basic_environment env (n);
  flatten (env, l);
  return env;
}
