
/******************************************************************************
* MODULE     : skeleton.cpp
* DESCRIPTION: Line breaking facility for paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "skeleton.hpp"

bool var_path_inf_eq (path p1, path p2);

insertion_rep::insertion_rep (tree type2, skeleton sk2):
  type (type2), begin (sk2[0]->ins[0]->begin),
  end (sk2[0]->ins[0]->end), sk (sk2)
{
  int i, n= N(sk);
  for (i=0; i<n; i++) {
    int j, k= N(sk[i]->ins);
    for (j=0; j<k; j++) {
      if (var_path_inf_eq (sk[i]->ins[j]->begin, begin))
	begin= sk[i]->ins[j]->begin;
      if (var_path_inf_eq (end, sk[i]->ins[j]->end))
	end= sk[i]->ins[j]->end;
    }
  }
}

bool
operator == (insertion ins1, insertion ins2) {
  return
    (ins1->type  == ins2->type ) &&
    (ins1->begin == ins2->begin) &&
    (ins1->end   == ins2->end  ) &&
    (ins1->sk    == ins2->sk   );
}

bool
operator != (insertion ins1, insertion ins2) {
  return
    (ins1->type  != ins2->type ) ||
    (ins1->begin != ins2->begin) ||
    (ins1->end   != ins2->end  ) ||
    (ins1->sk    != ins2->sk   );
}

bool
operator < (insertion ins1, insertion ins2) {
  if (ins1->type != ins2->type) {
    if (is_tuple (ins1->type, "footnote")) return false;
    if (is_tuple (ins2->type, "footnote")) return true;
    if (ins1->type == tuple ("float", "b")) return false;
    if (ins2->type == tuple ("float", "b")) return true;
    if (ins1->type == tuple ("float", "t")) return true;
    if (ins2->type == tuple ("float", "t")) return false;
  }
  return !var_path_inf_eq (ins2->begin, ins1->begin);
}

tm_ostream&
operator << (tm_ostream& out, insertion ins) {
  if (ins->type != "") out << ins->type << " ";
  out << "insertion [ " << ins->begin << " -- " << ins->end;
  if (N(ins->sk)>0) out << "; " << ins->sk;
  return out << " ]";
}

bool
operator == (pagelet pg1, pagelet pg2) {
  if (is_nil (pg1) || is_nil (pg2)) return is_nil (pg1) == is_nil (pg2);
  return (pg1->ins == pg2->ins);
}

bool
operator != (pagelet pg1, pagelet pg2) {
  if (is_nil (pg1) || is_nil (pg2)) return is_nil (pg1) != is_nil (pg2);
  return (pg1->ins != pg2->ins);
}

tm_ostream&
operator << (tm_ostream& out, pagelet pg) {
  return out << "pagelet " << pg->ins;
}

void
sort (pagelet& pg) {
  int i, n= N (pg->ins);
  while (true) {
    bool flag =true;
    for (i=0; i<n-1; i++)
      if (pg->ins[i+1] < pg->ins[i]) {
	insertion tmp= pg->ins[i];
	pg->ins[i]= pg->ins[i+1];
	pg->ins[i+1]= tmp;
	flag= false;
      }
    if (flag) break;
  }
}
