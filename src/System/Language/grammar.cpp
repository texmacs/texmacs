
/******************************************************************************
* MODULE     : grammar.cpp
* DESCRIPTION: packrat parsing
* COPYRIGHT  : (C) 2009  Francis Jamet, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "grammar.hpp"
#include "iterator.hpp"
#include "analyze.hpp"
#include "impl_language.hpp"
#include "Scheme/object.hpp"

parser_rep::parser_rep(hashmap<tree,tree> g, string s) {
  grammar=g; xstring=s;
  set_emptyness(); cout<<can_be_empty_table;
  set_dependance(); cout<<dependance;
}

parser::parser (hashmap<tree,tree> g, string s) { 
  rep= tm_new<parser_rep> (g, s);
}

/*
      iterator<src_t> it= iterate (src_to_dest);
      while (it->busy()) {
	src_t  src = it->next();
	dest_t dest= src_to_dest[src];
	...
      }
*/

void parser_rep::set_emptyness() {
  tree var_tree;
  string var;
  tree rule;
  bool new_empty;
  do {
    new_empty= false;
    iterator<tree> it= iterate(grammar);
    while (it->busy()) {
      var_tree= it->next();
      var= var_tree[0]->label;
      if (!(can_be_empty_table->contains(var))) {
	rule= grammar(var_tree);
	if (can_be_empty(rule)) {
	  new_empty= true;
	  can_be_empty_table(var)= true;
	}
      }
    }
  }
  while (new_empty== true);
}

bool
parser_rep::can_be_empty(tree rule) {
  if (L(rule)==as_tree_label("DOLLAR")) {
      if (can_be_empty_table->contains(rule[0]->label))
	{return true; } else return false;}
  if (L(rule)==as_tree_label("STAR")) return true;
  if (is_atomic(rule) && rule->label =="") return true;
  if (is_atomic(rule) && rule->label !="") return false;
  if (L(rule)==as_tree_label("OR")) {
    int i=0;
    while(i<N(rule)) { if (can_be_empty(rule[i])) return true; i++;}
    }
  if (L(rule)==as_tree_label("CONCAT")) {
    int i=0;
    while(i<N(rule)) { if (can_be_empty(rule[i])==false) return false; i++;}
    return true;
  }
  if (L(rule)==as_tree_label("RANGE")) return false;
  return false;
}

void
parser_rep::set_dependance(string var, tree rule) {
  if (L(rule)==as_tree_label("DOLLAR") && N(rule)==1) {
    pair<string,string> p(var,rule[0]->label);
    dependance(p)=true;}
  if (L(rule)==as_tree_label("OR")) {
    int i=0;
    while(i<N(rule)) {set_dependance(var,rule[i]);i++;}
  }
  if (L(rule)==as_tree_label("STAR")) set_dependance(var,rule[0]);
  if (L(rule)==as_tree_label("CONCAT")) {
    int i=0;
    while(i<N(rule) && can_be_empty(rule[i])) {set_dependance(var,rule[i]);i++;}
  }
}

void
parser_rep::set_dependance() {
  tree var_tree;
  string var;
  iterator<tree> it= iterate(grammar);
  while (it->busy()) {
    var_tree=it->next();
    var=var_tree[0]->label;
    set_dependance(var,grammar(var_tree));
  }
}

int
parser_rep::parse (tree parsing_tree, int pos) {
  if (pos >= N(xstring)) return -1;
  pair<tree,int> p(parsing_tree,pos);
  if (evaluated_pair->contains(p)) return evaluated_pair(p);
  if (wanted_pair->contains(p)) return -1;
  if ( L(parsing_tree)==as_tree_label("DOLLAR")) {
    if (! grammar->contains(parsing_tree)) return -1;
    tree regle;
    regle= grammar(parsing_tree);
    int opos=pos;
    p= pair<tree,int> (parsing_tree, opos);
    wanted_pair(p)= true;
    pos= parse(regle, pos);
    cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";  //effacer wanted_pair apres
    evaluated_pair(p)= pos;
    wanted_pair->reset(p);
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("OR") && N(parsing_tree)>=1) {    // or
    tree parsing_tree2;
    int i;
    int init_pos= pos;
    p= pair<tree, int> (parsing_tree, init_pos);
    wanted_pair(p)= true;
    i=0;
    do {
      parsing_tree2= parsing_tree[i];
      pos= parse(parsing_tree2, init_pos);
      i++;
    } while (pos==-1 && i<N(parsing_tree));
    evaluated_pair(p)= pos;
    cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    wanted_pair->reset(p);
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("CONCAT") && N(parsing_tree)>=1) { 
    tree parsing_tree2;
    int i=0;
    int init_pos= pos;
    p= pair<tree, int> (parsing_tree, init_pos);
    wanted_pair(p)= true;
    do {
      parsing_tree2= parsing_tree[i];
      pos=parse(parsing_tree2, pos);
      i++;
    } while (pos!=-1 && i<N(parsing_tree));
    cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    wanted_pair->reset(p);
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("STAR") && N(parsing_tree)==1) {
    tree parsing_tree1;
    parsing_tree1= parsing_tree[0];
    int init_pos= pos;
    int opos;
    p= pair<tree, int> (parsing_tree, init_pos);    
    wanted_pair(p)= true;
    do {
      opos= pos;
      pos= parse(parsing_tree1, pos);
    } while (pos!=-1 && pos<N(xstring));
    if (pos==-1) pos= opos;
    cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    wanted_pair->reset(p);
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("RANGE") && N(parsing_tree)==2) {
    string s1,s2;
    s1= parsing_tree[0]->label;
    s2= parsing_tree[1]->label;
    int opos= pos;
    if (s1 <= xstring(pos,pos+1)
	&& xstring(pos,pos+1) <=s2) {pos++;}
    else pos=-1;
    p= pair<tree, int> (parsing_tree, opos);
    cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    wanted_pair->reset(p);
    return pos;
  }
  if (is_atomic(parsing_tree)) {
    string s;
    s= parsing_tree->label;
    int opos= pos;
    if (pos+N(s) <= N(xstring) && s == xstring(pos,pos+N(s))) {pos+=N(s);}
    else pos= -1;
    p= pair<tree, int> (parsing_tree, opos);
    cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    return pos;
  }
  return -1;
}

static hashmap<tree,tree>* global_grammar= NULL;

void
define_grammar_rule (tree var, tree gram) {
  if (global_grammar == NULL) global_grammar= new hashmap<tree,tree> ();
  (*global_grammar) (var)= gram;
}

int
grammar_parse (tree var, string s) {
  if (global_grammar == NULL) global_grammar= new hashmap<tree,tree> ();
  parser p (*global_grammar, s);
  return p->parse (var, 0);
}
