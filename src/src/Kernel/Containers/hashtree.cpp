
/******************************************************************************
* MODULE     : hashtree
* DESCRIPTION: A tree class that stores a node's children in a hashmap instead
*              of a list. Can be used to implement a dictionary that maps
*              strings to strings efficiently.
* COPYRIGHT  : (C) 2002  Felix Breuer
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef HASHTREE_C
#define HASHTREE_C
#include "hashtree.hpp"

/******************************************************************************
* Methods normally provided by
* CONCRETE_TEMPLATE_2_CODE(hashtree,class,K,class,V);
******************************************************************************/

template<class K,class V> inline 
hashtree<K,V>::hashtree(const hashtree<K,V>& x): rep(x.rep) {
  if (this->rep!=NULL) INC_COUNT (this->rep);
}

template<class K,class V> inline 
hashtree<K,V>::~hashtree() {
  if (this->rep!=NULL) DEC_COUNT (this->rep);
}

template<class K,class V> inline hashtree<K,V>&
hashtree<K,V>::operator= (hashtree<K,V> x) {
  if (this->rep!=NULL) DEC_COUNT (this->rep); 
  this->rep = x.rep;
  if (x.rep!=NULL) INC_COUNT (x.rep);
  return *this;
}

/******************************************************************************
* Methods of hashtree_rep<K,V>
******************************************************************************/

template<class K, class V> inline bool
hashtree_rep<K,V>::contains (K key) {
  return children->contains(key);
}

template<class K, class V> void 
hashtree_rep<K,V>::add_child (K key, hashtree<K,V>& child) {
  child.realize();                   // make sure the child has a rep!
  children (key) = child;
}

template<class K, class V> void 
hashtree_rep<K,V>::add_new_child (K key) {
  hashtree<K,V> child;
  add_child (key, child);
}

template<class K, class V> void 
hashtree_rep<K,V>::set_label (V val) {
  label = val;
}

template<class K, class V> V 
hashtree_rep<K,V>::get_label () {
  return label;
}

/******************************************************************************
* Method to ensure that hashtree is non null
******************************************************************************/

template<class K, class V> inline void
hashtree<K,V>::realize() {
  if (rep == NULL) {
    rep = new hashtree_rep<K,V> ();
    INC_COUNT(rep);
  }
}
  
/******************************************************************************
* Overloaded operators
******************************************************************************/
  
template<class K, class V> inline hashtree_rep<K,V>* 
hashtree<K,V>::operator-> (void) {
  // always make sure there is a rep!
  realize ();
  return rep;
}

template<class K, class V> inline hashtree<K,V> 
hashtree<K,V>::operator[] (K key) {
  if (*this->contains (key)) return *this->children (key);
  else fatal_error("Hashtree: read-access to non-existent node requested");
}
  
template<class K, class V> inline hashtree<K,V> 
hashtree<K,V>::operator() (K key) {
  realize ();
  if (!(*this)->contains (key)) (*this)->add_new_child (key);
  return (*this)->children (key);
}

/******************************************************************************
* Functions
******************************************************************************/

template<class K, class V> inline bool
is_nil (hashtree<K,V> ht) {
  return ht.rep == NULL;
}

template<class K, class V> inline int
N (hashtree<K,V> ht) {
  return N(ht->children);
}

#endif // HASHTREE_C
