
/******************************************************************************
* MODULE     : hashtree
* DESCRIPTION: A tree class that stores a node's children in a hashmap instead
*              of a list. Can be used to implement a dictionary that maps
*              strings to strings efficiently.
* COPYRIGHT  : (C) 2002  Felix Breuer
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HASHTREE_H
#define HASHTREE_H
#include "hashmap.hpp"
#include "hashmap.cpp"
 
template<class K, class V> class hashtree;
template<class K, class V> int N (hashtree<K,V> tree);
template<class K, class V> bool is_nil (hashtree<K,V> tree);
 
template<class K, class V> class hashtree_rep: concrete_struct {
  hashmap<K,hashtree<K,V> > children;
public:
  V label;

  inline hashtree_rep(): children(hashtree<K,V> (false)), label() {}
  inline hashtree_rep(V val): children(hashtree<K,V> ()), label(val) {}
  
  inline bool contains (K key);
  void add_child (K key, hashtree<K,V>& child);
  void add_new_child (K key);
  void set_label (V val);
  V get_label ();
  
  friend class hashtree<K,V>;  
#ifdef OS_WIN32
  friend int N (hashtree<K,V> tree);
#else
  friend int N<K,V>(hashtree<K,V> tree);
#endif
};

/******************************************************************************
* The class hashtree is a tree-class with the special property that
* a node stores its children not in a list or in an array but in a hashmap
* so as to make child-lookup efficient (for broad trees). Each edge of
* the tree has a label and these labels are used as keys for the hashmap.
* The nodes of the may have a value associated with them. 
*
* This data structure is suitable for storing dictionaries which map
* strings of keys to some value in the following fashion:
* Consider this englisch->german dictionary:
*
*   he -> er
*   head -> Kopf
*   heaven -> Himmel
*   hell -> H?lle
*   hello -> hallo
*
* which is represented by the following tree.
*  
*   []--h--[]--e--[er]--a--[]--d--[Kopf]
*                  |       |
*                  |       v
*                  l       |
*                  |       +--e--[]--n--[Himmel]
*                  |
*                  +--l--[H?lle]--o--[hallo]
*
* However, using hashmaps to store the children of a node is problematic
* in TeXmacs. A Hashmap contains a *copy* of a default value.
* If now a hashtree (node) contains a hasmap which in turn
* contains *at least one* hashtree, the instantiation of a single
* hashtree leads to an infinite recursion. I chose to solve this problem
* by allowing hashtree which do not have a hashtree_rep (the equivalent
* of NULL pointers so to speak). These NULL nodes are created by passing
* a boolean value to the hashtree constructor. One cannot accidentally
* obtain a NULL element by e.g. accessing a child (see below).
*
* In general, I tried to imitate the TeXmacs-way of memory management
* as closely as possibly, however the workaround is not that pretty.
* As more elegant way might be to modify the hashmap class so that
* a hashmap contains only a pointer to a function that returns
* a default value instead of a instance of a value-element.
* But I didn't want to modify core TeXmacs code.
******************************************************************************/
  
template<class K, class V> class hashtree {
  //CONCRETE_TEMPLATE_2(hashtree,K,V);
  hashtree_rep<K,V>* rep;

  // this constructor always returns a NULL element
  inline hashtree (bool): rep (NULL) {}
  
  // ensures that this hashtree has a rep
  void realize();

public:
  inline hashtree (const hashtree<K,V>&);
  inline ~hashtree ();
  inline hashtree<K,V>& operator= (hashtree<K,V> x);
    
  // default constructor returns a non-NULL node, which does not have a value
  inline hashtree (): rep (new hashtree_rep<K,V>()) {}
  
  // returns a non-NULL node, that has value
  inline hashtree (V val): rep (new hashtree_rep<K,V>(val)) {}
  
  // returns this node's value
  inline hashtree_rep<K,V>* operator-> (void);
  
  // returns this node's child with the label "key". If the node doesn't
  // have such a child, an error is raised.
  inline hashtree<K,V> operator() (K key);   // read only access
  
  // returns this node's child with the label "key". If the node doesn't
  // have such a child, a non-NULL node is created, added to this node's 
  // children with the appropriate label and returned. 
  // Thus, this method may change the in-memory representation of a tree
  // but it does not change the dictionary the tree represents.  
  inline hashtree<K,V> operator[] (K key);   // rw access

  friend class hashtree_rep<K,V>;
#ifdef OS_WIN32
  friend bool is_nil (hashtree<K,V> ht);
  friend int N (hashtree<K,V> ht);
#else
  friend bool is_nil<K,V> (hashtree<K,V> ht);
  friend int N<K,V>(hashtree<K,V> ht);
#endif
};

#include "hashtree.cpp"

#endif // HASHTREE_H
