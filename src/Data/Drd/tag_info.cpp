
/******************************************************************************
* MODULE     : tag_info.cpp
* DESCRIPTION: DRD information about tags
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tag_info.hpp"
#include "hashmap.hpp"

#define get_bits(which,nr) which=i&((1<<nr)-1);i=i>>nr
#define set_bits(which,nr) i+=((int)which)<<offset;offset+=nr

/******************************************************************************
* Compact representation for environment changes
******************************************************************************/

static hashmap<tree,int> encode_table (-1);
static array<tree>       decode_table;

int
drd_encode (tree t) {
  if (encode_table->contains (t))
    return encode_table[t];
  int n= N(decode_table);
  ASSERT (n < (1 << 16), "drd_encode overflow");
  encode_table (t) = n;
  decode_table << t;
  return n;
}

tree
drd_decode (int i) {
  ASSERT (i >= 0 && i < N (decode_table), "out of range");
  return decode_table[i];
}

/******************************************************************************
* Names for drd types
******************************************************************************/

string
drd_decode_type (int i) {
  switch (i) {
  case TYPE_REGULAR: return "regular";
  case TYPE_ADHOC: return "adhoc";
  case TYPE_VARIABLE: return "variable";
  case TYPE_ARGUMENT: return "argument";
  case TYPE_BOOLEAN: return "boolean";
  case TYPE_INTEGER: return "integer";
  case TYPE_STRING: return "string";
  case TYPE_LENGTH: return "length";
  case TYPE_NUMERIC: return "numeric";
  case TYPE_CODE: return "code";
  case TYPE_IDENTIFIER: return "identifier";
  case TYPE_URL: return "url";
  case TYPE_COLOR: return "color";
  case TYPE_GRAPHICAL: return "graphical";
  case TYPE_POINT: return "point";
  case TYPE_CONSTRAINT: return "constraint";
  case TYPE_GRAPHICAL_ID: return "graphical-id";
  case TYPE_EFFECT: return "effect";
  case TYPE_ANIMATION: return "animation";
  case TYPE_DURATION: return "duration";
  case TYPE_FONT_SIZE: return "font-size";
  case TYPE_OBSOLETE: return "obsolete";
  case TYPE_UNKNOWN: return "unknown";
  case TYPE_ERROR: return "error";
  default: return "unknown";
  }
}

int
drd_encode_type (string s) {
  if (s == "regular") return TYPE_REGULAR;
  else if (s == "adhoc") return TYPE_ADHOC;
  else if (s == "variable") return TYPE_VARIABLE;
  else if (s == "argument") return TYPE_ARGUMENT;
  else if (s == "boolean") return TYPE_BOOLEAN;
  else if (s == "integer") return TYPE_INTEGER;
  else if (s == "string") return TYPE_STRING;
  else if (s == "length") return TYPE_LENGTH;
  else if (s == "numeric") return TYPE_NUMERIC;
  else if (s == "code") return TYPE_CODE;
  else if (s == "identifier") return TYPE_IDENTIFIER;
  else if (s == "url") return TYPE_URL;
  else if (s == "color") return TYPE_COLOR;
  else if (s == "graphical") return TYPE_GRAPHICAL;
  else if (s == "point") return TYPE_POINT;
  else if (s == "constraint") return TYPE_CONSTRAINT;
  else if (s == "graphical-id") return TYPE_GRAPHICAL_ID;
  else if (s == "effect") return TYPE_EFFECT;
  else if (s == "animation") return TYPE_ANIMATION;
  else if (s == "duration") return TYPE_DURATION;
  else if (s == "font-size") return TYPE_FONT_SIZE;
  else if (s == "unknown") return TYPE_UNKNOWN;
  else if (s == "error") return TYPE_ERROR;
  else return -1;
}

/******************************************************************************
* Properties of the tag
******************************************************************************/

parent_info::parent_info (int a, int x, int am, int cm, bool frozen) {
  type             = TYPE_REGULAR;
  arity_mode       = am;
  arity_base       = a;
  arity_extra      = x;
  child_mode       = cm;
  border_mode      = BORDER_YES;
  block            = BLOCK_NO;
  with_like        = false;
  var_type         = VAR_MACRO;
  freeze_arity     = frozen;
  freeze_border    = frozen;
  freeze_block     = frozen;
  freeze_with      = frozen;
  freeze_var_type  = frozen;
}

parent_info::parent_info (tree t) {
  long int i= as_long_int (t);
  get_bits (type            , 5);
  get_bits (arity_mode      , 2);
  get_bits (arity_base      , 6);
  get_bits (arity_extra     , 4);
  get_bits (child_mode      , 2);
  get_bits (border_mode     , 2);
  get_bits (block           , 2);
  get_bits (with_like       , 1);
  get_bits (var_type        , 2);
  get_bits (freeze_type     , 1);
  get_bits (freeze_arity    , 1);
  get_bits (freeze_border   , 1);
  get_bits (freeze_block    , 1);
  get_bits (freeze_with     , 1);
  get_bits (freeze_var_type , 1);
}

parent_info::operator tree () {
  long int i=0;
  int offset=0;
  set_bits (type            , 5);
  set_bits (arity_mode      , 2);
  set_bits (arity_base      , 6);
  set_bits (arity_extra     , 4);
  set_bits (child_mode      , 2);
  set_bits (border_mode     , 2);
  set_bits (block           , 2);
  set_bits (with_like       , 1);
  set_bits (var_type        , 2);
  set_bits (freeze_type     , 1);
  set_bits (freeze_arity    , 1);
  set_bits (freeze_border   , 1);
  set_bits (freeze_block    , 1);
  set_bits (freeze_with     , 1);
  set_bits (freeze_var_type , 1);
  return as_string (i);
}

bool
parent_info::operator == (const parent_info& pi) {
  return
    (type             == pi.type            ) &&
    (arity_mode       == pi.arity_mode      ) &&
    (arity_base       == pi.arity_base      ) &&
    (arity_extra      == pi.arity_extra     ) &&
    (child_mode       == pi.child_mode      ) &&
    (border_mode      == pi.border_mode     ) &&
    (block            == pi.block           ) &&
    (with_like        == pi.with_like       ) &&
    (var_type         == pi.var_type        ) &&
    (freeze_arity     == pi.freeze_arity    ) &&
    (freeze_border    == pi.freeze_border   ) &&
    (freeze_block     == pi.freeze_block    ) &&
    (freeze_with      == pi.freeze_with     ) &&
    (freeze_var_type  == pi.freeze_var_type );
}

bool
parent_info::operator != (const parent_info& pi) {
  return !(operator == (pi));
}

tm_ostream&
operator << (tm_ostream& out, parent_info pi) {
  return out << ((tree) pi);
}

/******************************************************************************
* Properties of the children of the tag
******************************************************************************/

child_info::child_info (bool frozen) {
  type               = TYPE_ADHOC;
  accessible         = ACCESSIBLE_NEVER;
  writability        = WRITABILITY_NORMAL;
  block              = 0;
  env                = drd_encode (tree (WITH));
  freeze_type        = frozen;
  freeze_accessible  = frozen;
  freeze_writability = frozen;
  freeze_block       = frozen;
  freeze_env         = frozen;
}

child_info::child_info (tree t) {
  int i= as_int (is_atomic (t)? t: t[N(t)-1]);
  get_bits (type              ,  5);
  get_bits (accessible        ,  2);
  get_bits (writability       ,  2);
  get_bits (block             ,  2);
  get_bits (freeze_type       ,  1);
  get_bits (freeze_accessible ,  1);
  get_bits (freeze_writability,  1);
  get_bits (freeze_block      ,  1);
  get_bits (freeze_env        ,  1);
  if (is_atomic (t)) env= drd_encode (tree (WITH));
  else env= drd_encode (t (0, N(t)-1));
}

child_info::operator tree () {
  int i=0, offset=0;
  set_bits (type              ,  5);
  set_bits (accessible        ,  2);
  set_bits (writability       ,  2);
  set_bits (block             ,  2);
  set_bits (freeze_type       ,  1);
  set_bits (freeze_accessible ,  1);
  set_bits (freeze_writability,  1);
  set_bits (freeze_block      ,  1);
  set_bits (freeze_env        ,  1);
  if (drd_decode (env) == tree (WITH)) return as_string (i);
  else return drd_decode (env) * tree (WITH, as_string (i));
}

bool
child_info::operator == (const child_info& ci) {
  return
    (type               == ci.type              ) &&
    (accessible         == ci.accessible        ) &&
    (writability        == ci.writability       ) &&
    (block              == ci.block             ) &&
    (env                == ci.env               ) &&
    (freeze_type        == ci.freeze_type       ) &&
    (freeze_accessible  == ci.freeze_accessible ) &&
    (freeze_writability == ci.freeze_writability) &&
    (freeze_block       == ci.freeze_block      ) &&
    (freeze_env         == ci.freeze_env        );
}

bool
child_info::operator != (const child_info& ci) {
  return !(operator == (ci));
}

tm_ostream&
operator << (tm_ostream& out, child_info ci) {
  return out << ((tree) ci);
}

/******************************************************************************
* Constructors, destructors and converters
******************************************************************************/

tag_info_rep::tag_info_rep (parent_info pi2, array<child_info> ci2, tree x):
  pi (pi2), ci (ci2), extra (x) {}

tag_info_rep::tag_info_rep (int a, int x, int am, int cm, bool frozen):
  pi (a, x, am, cm, frozen),
  ci ((a+x)==0? 0: (cm==CHILD_UNIFORM? 1: (cm==CHILD_BIFORM? 2: (a+x))))
{
  if (frozen) {
    int i, n= N(ci);
    for (i=0; i<n; i++)
      ci[i]= child_info (true);
  }
}

tag_info::tag_info (parent_info pi, array<child_info> ci, tree extra) {
  rep= tm_new<tag_info_rep> (pi, ci, extra);
}

tag_info::tag_info (int a, int x, int am, int cm, bool frozen) {
  rep= tm_new<tag_info_rep> (a, x, am, cm, frozen);
}

tag_info::tag_info (tree t) {
  if ((!is_func (t, TUPLE)) || (N(t)<2) || (L(t[1]) != TUPLE)) {
    failed_error << "t= " << t << "\n";
    FAILED ("bad tag_info");
  }
  parent_info pi (t[0]);
  int i, n= N(t[1]);
  array<child_info> ci (n);
  for (i=0; i<n; i++)
    ci[i]= child_info (t[1][i]);
  rep= tm_new<tag_info_rep> (pi, ci, N(t)==3? t[2]: tree (""));
}

tag_info::operator tree () {
  if (rep->extra == "") return tree (TUPLE, (tree) rep->pi, (tree) rep->ci);
  else return tree (TUPLE, (tree) rep->pi, (tree) rep->ci, rep->extra);
}

/******************************************************************************
* Access routines and getting the index of a child
******************************************************************************/

tag_info
tag_info_rep::inner_border () {
  pi.border_mode= BORDER_INNER;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::outer_border () {
  pi.border_mode= BORDER_OUTER;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::with_like () {
  pi.with_like= true;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::var_parameter () {
  pi.var_type= VAR_PARAMETER;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::var_macro_parameter () {
  pi.var_type= VAR_MACRO_PARAMETER;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::type (int tp) {
  pi.type= tp;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::type (int i, int tp) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].type= tp;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::accessible (int i) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].type= TYPE_REGULAR;
  ci[i].accessible= ACCESSIBLE_ALWAYS;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::hidden (int i) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].type= TYPE_REGULAR;
  ci[i].accessible= ACCESSIBLE_HIDDEN;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::disable_writable (int i) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].writability= WRITABILITY_DISABLE;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::enable_writable (int i) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].writability= WRITABILITY_ENABLE;
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::locals (int i, string var, string val) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  ci[i].env= drd_encode (tree (ATTR, var, val));
  return tag_info (pi, ci, extra);
}

void
tag_info_rep::set_attribute (string which, tree val) {
  if (extra == "") extra= tree (ATTR);
  for (int i=0; i+1<N(extra); i+=2)
    if (extra[i] == tree (which)) {
      extra[i+1]= val;
      return;
    }
  extra << tree (which) << val;
}

tree
tag_info_rep::get_attribute (string which) {
  if (!is_func (extra, ATTR)) return "";
  int i, n= N(extra);
  for (i=0; i+1<n; i+=2)
    if (extra[i] == which)
      return extra[i+1];
  return "";
}

tag_info
tag_info_rep::name (string s) {
  set_attribute ("name", s);
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::long_name (string s) {
  set_attribute ("long-name", s);
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::name (int i, string s) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  set_attribute ("name-" * as_string (i), s);
  return tag_info (pi, ci, extra);
}

tag_info
tag_info_rep::long_name (int i, string s) {
  if (i < 0 || i >= N(ci)) cout << i << " out of " << N(ci) << "\n";
  ASSERT (i >= 0 && i<N(ci), "index out of range");
  set_attribute ("long-name-" * as_string (i), s);
  return tag_info (pi, ci, extra);
}

int
tag_info_rep::get_index (int child, int n) {
  int r= 0;
  switch (pi.child_mode) {
  case CHILD_UNIFORM:
    r= 0;
    break;
  case CHILD_BIFORM:
    if (pi.arity_mode != ARITY_VAR_REPEAT) {
      if (child < ((int) pi.arity_base)) r= 0;
      else r= 1;
    }
    else {
      if (child < (n-((int) pi.arity_base))) r= 0;
      else r= 1;
    }
    break;
  case CHILD_DETAILED:
    if (((int) pi.arity_mode) <= ARITY_OPTIONS)
      r= child;
    else if (pi.arity_mode == ARITY_REPEAT) {
      if (child < ((int) pi.arity_base)) r= child;
      else r= (child - pi.arity_base) % pi.arity_extra + pi.arity_base;
    }
    else {
      if (child < (n-((int) pi.arity_base))) r= child % pi.arity_extra;
      else r= pi.arity_base + pi.arity_extra + child - n;
    }
    break;
  }
  return r;
}

child_info&
tag_info::operator () (int child, int n) {
  int index= rep->get_index (child, n);
  if (index < 0 || index >= N(rep->ci)) {
    failed_error << "child       = " << child << "\n";
    failed_error << "out of      = " << n << "\n";
    failed_error << "child_mode  = " << rep->pi.child_mode << "\n";
    failed_error << "arity_mode  = " << rep->pi.arity_mode << "\n";
    failed_error << "arity_base  = " << rep->pi.arity_base << "\n";
    failed_error << "arity_extra = " << rep->pi.arity_extra << "\n";
    failed_error << "N(ci)       = " << N(rep->ci) << "\n";
    FAILED ("index out of range");
  }
  return rep->ci [index];
}

/******************************************************************************
* Usual extra routines
******************************************************************************/

tm_ostream&
operator << (tm_ostream& out, tag_info ti) {
  out << "[ " << ti->pi << ", " << ti->ci;
  if (ti->extra != "") out << ", " << ti->extra << "\n";
  return out << " ]";
}

tag_info
copy (tag_info ti) {
  return tag_info (ti->pi, copy (ti->ci), copy (ti->extra));
}

bool
operator == (tag_info ti1, tag_info ti2) {
  return
    (ti1->pi == ti2->pi) && (ti1->ci == ti2->ci) && (ti1->extra == ti2->extra);
}

bool
operator != (tag_info ti1, tag_info ti2) {
  return !(ti1 == ti2);
}
