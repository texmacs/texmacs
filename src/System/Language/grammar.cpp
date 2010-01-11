
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
  closure=set_closure(dependance); cout<<closure;
  set_dag(); cout<<dag;
}

parser::parser (hashmap<tree,tree> g, string s) { 
  rep= tm_new<parser_rep> (g, s);
}

void
parser_rep::set_emptyness() {
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
  while (new_empty);
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
    dependance(p)=true;
    closure(p)=true;}
  if (L(rule)==as_tree_label("OR")) {
    int i=0;
    while(i<N(rule)) {set_dependance(var,rule[i]);i++;}
  }
  if (L(rule)==as_tree_label("STAR")) set_dependance(var,rule[0]);
  if (L(rule)==as_tree_label("CONCAT")) {
    int i=0;
    while(i<N(rule) && (i==0 || can_be_empty(rule[i-1])))
      {set_dependance(var,rule[i]);i++;}
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

hashmap <pair<string,string>, bool>
parser_rep::set_closure(hashmap <pair<string,string>,bool> r) {
  string var1,var2,var3,var4;
  bool new_dependance;
  hashmap<pair<string,string>,bool> c;
  c=copy(r);
  do {
    new_dependance= false;
    iterator<pair<string,string> > it12= iterate(c);
    while(it12->busy()) {
      pair<string,string> p=it12->next();
      var1=p.x1;
      var2=p.x2;
      iterator<pair<string,string> > it34= iterate(c);
      while(it34->busy()) { 
	p=it34->next();
	var3=p.x1;
	var4=p.x2;
	p=pair<string,string>(var1,var4);
	if (var2==var3 && !c->contains(p)) {
	  new_dependance= true;
	  c(p)=true;
	}
      }
    }
  }
  while(new_dependance);
  return c;
}

void
parser_rep::set_dag() {
  string var1, var2;
  hashmap<pair<string,string>,bool> closure_dag;
  iterator<pair<string,string> > it= iterate(dependance);
  while(it->busy()) {
    pair<string,string> p= it->next();
    dag(p)= true;
    closure_dag= set_closure(dag);
    pair<string,string> p1(p.x1,p.x1);
    if (closure_dag->contains(p1)) dag->reset(p);
  }
}

int
parser_rep::parse_level(string calling_letter,
			int level, tree parsing_tree, int pos) {
  if (pos > N(xstring)) return -1;
  if (level > N(xstring)) return -1;
  quartet <string,int,tree,int> t(calling_letter,level,parsing_tree,pos);
  if (evaluated_quartet->contains(t)) return evaluated_quartet(t);
  if (L(parsing_tree)==as_tree_label("DOLLAR")) {
    if (! grammar->contains(parsing_tree)) return -1;
    int opos= pos;
    string called_letter= parsing_tree[0]->label;
    pair<string,string> p1(calling_letter,called_letter);
    pair<string,string> p2(called_letter,calling_letter);
    tree rule= grammar(parsing_tree);
    if (closure->contains(p1) && closure->contains(p2)) {
      if ((! dag->contains(p1)) && level==0) {pos=-1;}
      else {
	if (! dag->contains(p1)) level--;
	pos= parse_level(called_letter, level, rule, pos);
      }
    }
    else {pos= parse(parsing_tree,opos);}
    evaluated_quartet(t)= pos;
    //cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("OR") && N(parsing_tree)>=1) {    // or
    tree parsing_tree2;
    int i;
    int init_pos= pos;
    i=0;
    do {
      parsing_tree2= parsing_tree[i];
      pos= parse_level(calling_letter, level, parsing_tree2, init_pos);
      i++;
    } while (pos==-1 && i<N(parsing_tree));
    //cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_quartet(t)= pos;
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("CONCAT") && N(parsing_tree)>=1) { 
    tree parsing_tree2;
    int i=0;
    int init_pos= pos;
    do {
      parsing_tree2= parsing_tree[i];
      if (init_pos==pos) { pos=parse_level(calling_letter, level, parsing_tree2, pos);}
      else {pos=parse(parsing_tree2, pos);}
      i++;
    } while (pos!=-1 && i<N(parsing_tree));
    // cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_quartet(t)= pos;
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("STAR") && N(parsing_tree)==1) {
    tree parsing_tree1;
    parsing_tree1= parsing_tree[0];
    int init_pos= pos;
    int opos;    
    do {
      opos= pos;
      if(init_pos==pos) {pos= parse_level(calling_letter, level, parsing_tree1, pos);}
      else {pos=parse(parsing_tree1, pos);}
    } while (pos!=-1 && pos<N(xstring));
    if (pos==-1) pos= opos;
    // cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_quartet(t)= pos;
    return pos;
  }
  pos= parse(parsing_tree, pos);
  evaluated_quartet(t)= pos;
  return pos;
}

int
parser_rep::parse(tree parsing_tree, int pos) {
  pair<tree, int> p(parsing_tree, pos);
  // cout<<evaluated_triple;
  if (pos > N(xstring)) return -1;
  if (evaluated_pair->contains(p)) return evaluated_pair(p);
  if(L(parsing_tree)==as_tree_label("DOLLAR")) {
    int init_pos= pos;
    pos=-1;
    int opos;
    int i=0;
    string letter= parsing_tree[0]->label;
    tree rule=grammar(parsing_tree);
    do {
      opos=pos; //cout << "test"<<i;
      pos=parse_level(letter,i,rule, init_pos);
      //triple<string, int, int> t(letter,i,init_pos);
      //evaluated_triple(t)=pos;
      i++;
    }
    while (pos > opos);
    pos= opos;
    evaluated_pair(p)= pos;
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("OR") && N(parsing_tree)>=1) {    // or
    tree parsing_tree2;
    int i;
    int init_pos= pos;
    i=0;
    do {
      parsing_tree2= parsing_tree[i];
      pos= parse(parsing_tree2, init_pos);
      i++;
    } while (pos==-1 && i<N(parsing_tree));
    evaluated_pair(p)= pos;
    // cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("CONCAT") && N(parsing_tree)>=1) { 
    tree parsing_tree2;
    int i=0;
    int init_pos= pos;
    do {
      parsing_tree2= parsing_tree[i];
      pos=parse(parsing_tree2, pos);
      i++;
    } while (pos!=-1 && i<N(parsing_tree));
    // cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    return pos;
  }
  if (L(parsing_tree)==as_tree_label("STAR") && N(parsing_tree)==1) {
    tree parsing_tree1;
    parsing_tree1= parsing_tree[0];
    int init_pos= pos;
    int opos;    
    do {
      opos= pos;
      pos=parse(parsing_tree1, pos);
    } while (pos!=-1 && pos<N(xstring));
    if (pos==-1) pos= opos;
    // cout<<parsing_tree<<" "<<init_pos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    return pos;
  }  
  if (L(parsing_tree)==as_tree_label("RANGE") && N(parsing_tree)==2) {
    string s1,s2;
    s1= parsing_tree[0]->label;
    s2= parsing_tree[1]->label;
    int opos= pos;
    if (pos+1 <= N(xstring) && s1 <= xstring(pos,pos+1)
	&& xstring(pos,pos+1) <=s2) {pos++;}
    else pos=-1;
    //cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    return pos;
  }
  if (is_atomic(parsing_tree)) {
    string s;
    s= parsing_tree->label;
    int opos= pos;
    if (pos+N(s) <= N(xstring) && s == xstring(pos,pos+N(s))) {pos+=N(s);}
    else pos= -1;
    // cout<<parsing_tree<<" "<<opos<<" "<<pos<<"\n";
    evaluated_pair(p)= pos;
    return pos;
  }
  return -1;
}
/*
string
parser_rep::translate_rec(tree t, int pos) {
  if (L(t)==as_tree_label("DOLLAR")) {
      return translate_rec(grammar(t));
    }
  if (is_atomic(t)) {
    string s;
    s=t->label;
    return s;
  }
  if (L(t)==as_tree_label("RANGE")) {
    return xstring(pos,pos+1);
  }
  if (L(t)==as_tree_label("STAR")) {
    string s="";
    int opos;
    do {
      opos=pos;
      pos=parse(t[0],opos);
      if (pos !=-1) { s=s*translate_rec(t[0],opos);}
    } 
    while(pos!=-1);
    return s;
  }
  if (L(t)==as_tree_label("OR")) {
    int opos=pos;
    int i=0;
    do {
      pos=parse(t[i],opos);
      i++;
    } while(pos == -1);
    return translate_rec(t[i-1],opos);
  }
  if (L(t)==as_tree_label("CONCAT")) {
    string s="";
    int i;
    for(i=0;i<N(t);i++) {
      s=s*translate_rec(t[i],pos);
      pos=parse(t[i],pos);
    }
    return s;
  }
  if (L(t)==as_tree_label("TRANSLATE")) {
    hashmap<int, string> sub;
    tree c=t[0];
    tree r=t[1];
    string s;
    int i;
    if (is_atomic(c)) {sub(1)=translate_rec(c,pos); }
    else {
      for(i=1;i<N(c);i++) {
	sub(i)=translate_rec(c[i],pos);
	pos=parse(c[i],pos);
      }
    }
    if (is_atomic(r)) return r->label;
    for(i=0;i<N(r);i++) {
      if (is_atomic(r[i])) {s=s*(r[i]->label);}
      else if (L(r[i])==as_tree_label("INT")) s=s*sub(as_int(r[i][0]->label));
    }
    return s;
  }
  return "";
}
*/

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
