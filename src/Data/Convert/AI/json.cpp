
/******************************************************************************
* MODULE     : json.cpp
* DESCRIPTION: json parser and pretty printer
* COPYRIGHT  : (C) 2026  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"

#define JSON_NULL           1
#define JSON_BOOLEAN        2
#define JSON_NUMBER         4
#define JSON_SHORT_ARRAY    8
#define JSON_SHORT_OBJECT  16

tree json_parse (string s, int& pos, int mode);
void json_print (string& s, tree t, int mode, int indent);

/******************************************************************************
* JSON parser
******************************************************************************/

tree
json_null (int mode) {
  if ((mode & JSON_NULL) == 0) return "";
  return compound ("json-null");
}

tree
json_boolean (string s, int mode) {
  if ((mode & JSON_BOOLEAN) == 0) return s;
  return compound ("json-boolean", s);
}

tree
json_number (string s, int mode) {
  if ((mode & JSON_NUMBER) == 0) return s;
  return compound ("json-number", s);
}

void
json_skip (string s, int& pos) {
  while (pos < N(s)) {
    switch (s[pos]) {
    case '\"': case ',':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case ':': case '[': case '\\': case ']':
    case 'f': case 'n': case 't':
    case '{': case '}':
      return;
    }
    pos++;
  }
}

tree
json_parse_string (string s, int& pos, int mode) {
  string r;
  pos++;
  while (pos < N(s))
    if (s[pos] == '\"') { pos++; break; }
    else if (s[pos] == '\\' && pos + 1 < N(s)) {
      pos++;
      if (s[pos] == '\"' || s[pos] == '\\') r << s[pos++];
      else if (s[pos] == 'b') { pos++; r << '\b'; }
      else if (s[pos] == 'f') { pos++; r << '\b'; }
      else if (s[pos] == 'n') { pos++; r << '\n'; }
      else if (s[pos] == 'r') { pos++; r << '\r'; }
      else if (s[pos] == 't') { pos++; r << '\t'; }
    }
    else r << s[pos++];
  return r;
}

tree
json_parse_number (string s, int& pos, int mode) {
  int start= pos;
  while (pos < N(s) &&
         ((s[pos] >= '0' && s[pos] <= '9') ||
          s[pos] == '.'))
    pos++;
  return json_number (s (start, pos), mode);
}

tree
json_parse_array (string s, int& pos, int mode) {
  tree t (TUPLE);
  pos++;
  json_skip (s, pos);
  if (pos < N(s) && s[pos] == ']') { pos++; return t; }
  t << json_parse (s, pos, mode);
  json_skip (s, pos);
  while (pos < N(s) && s[pos] == ',') {
    pos++;
    json_skip (s, pos);
    t << json_parse (s, pos, mode);
    json_skip (s, pos);
  }
  if (pos < N(s) && s[pos] == ']') pos++;
  return t;
}

tree
json_parse_object (string s, int& pos, int mode) {
  tree t (ATTR);
  pos++;
  json_skip (s, pos);
  while (pos < N(s) && s[pos] != '}') {
    tree key= json_parse (s, pos, mode);
    json_skip (s, pos);
    tree val= json_null (mode);
    if (pos < N(s) && s[pos] == ':') {
      pos++;
      json_skip (s, pos);
      val= json_parse (s, pos, mode);
      json_skip (s, pos);
    }
    t << key << val;
    if (pos < N(s) && s[pos] == ',') {
      pos++;
      json_skip (s, pos);
    }
  }
  if (pos < N(s) && s[pos] == '}') pos++;
  return t;
}

tree
json_parse (string s, int& pos, int mode) {
  while (pos < N(s)) {
    switch (s[pos]) {
    case '\"':
      return json_parse_string (s, pos, mode);
    case ',':
      break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return json_parse_number (s, pos, mode);
    case '[':
      return json_parse_array (s, pos, mode);
    case ']':
      break;
    case 'f':
      if (read (s, pos, "false"))
        return json_boolean ("false", mode);
      break;
    case 'n':
      if (read (s, pos, "null"))
        return json_null (mode);
      break;
    case 't':
      if (read (s, pos, "true"))
        return json_boolean ("true", mode);
      break;
    case '{':
      return json_parse_object (s, pos, mode);
    case '}':
      break;
    }
    pos++;
  }
  return json_null (mode);
}

tree
json_to_tree (string s, int mode) {
  int pos= 0;
  return json_parse (s, pos, mode);
}

/******************************************************************************
* JSON pretty printer
******************************************************************************/

void
json_lf (string& r, int indent) {
  r << "\n";
  for (int i=0; i<indent; i++) r << " ";
}

void
json_space (string& r) {
  int l= N(r) - 1;
  if (l < 0) return;
  if (r[l] == '[' || r[l] == '{') r << " ";
}

void
json_open (string& r, string br) {
  json_space (r);
  r << br;
}

void
json_close (string& r, string br) {
  if (N(r) != 0 && r[N(r)-1] != ' ' && r[N(r)-1] != '\n') r << " ";
  r << br;
}

void
json_print_string (string& r, string s, int mode) {
  json_space (r);
  r << "\"";
  int i, n= N(s);
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\b': r << "\\b"; break;
    case '\t': r << "\\t"; break;
    case '\n': r << "\\n"; break;
    case '\f': r << "\\f"; break;
    case '\r': r << "\\r"; break;
    case '\"': r << '\\' << s[i]; break;
    case '\\': r << '\\' << s[i]; break;
    default: r << s[i];
    }
  r << "\"";
}

bool
is_short (tree t) {
  if (is_atomic (t)) return true;
  else if (is_func (t, TUPLE, 0) || is_func (t, ATTR, 0))
    return true;
  else if (is_func (t, TUPLE, 1))
    return true; // is_short (t[0]);
  else if (is_func (t, ATTR, 2))
    return true; // is_short (t[0]) && is_short (t[1]);
  else return false;
}

void
json_print_array (string& r, tree t, int mode, int indent) {
  if (N(t) == 0) {
    json_space (r);
    r << "[]";
  }
  else if ((mode & JSON_SHORT_ARRAY) != 0 || is_short (t)) {
    json_open (r, "[");
    for (int i=0; i<N(t); i++) {
      json_print (r, t[i], mode, indent);
      if (i+1 < N(t)) r << ", ";
    }
    json_close (r, "]");
  }
  else {
    json_open (r, "[");
    indent += 2;
    json_lf (r, indent);
    for (int i=0; i<N(t); i++) {
      json_print (r, t[i], mode, indent);
      if (i+1 < N(t)) {
        r << ",";
        json_lf (r, indent);
      }
    }
    indent -= 2;
    json_lf (r, indent);
    json_close (r, "]");
  }
}

void
json_print_object (string& r, tree t, int mode, int indent) {
  if (N(t) == 0) {
    json_space (r);
    r << "{}";
  }
  else if ((mode & JSON_SHORT_OBJECT) != 0 || is_short (t)) {
    json_open (r, "{");
    for (int i= 0; i+1 < N(t); i += 2) {
      json_print (r, t[i], mode, indent);
      r << ": ";
      json_print (r, t[i+1], mode, indent);
      if (i+2 < N(t)) r << ", ";
    }
    json_close (r, "}");
  }
  else {
    json_open (r, "{");
    indent += 2;
    json_lf (r, indent);
    for (int i= 0; i+1 < N(t); i += 2) {
      json_print (r, t[i], mode, indent);
      r << ": ";
      json_print (r, t[i+1], mode, indent);
      if (i+2 < N(t)) {
        r << ",";
        json_lf (r, indent);
      }
    }
    indent -= 2;
    json_lf (r, indent);
    json_close (r, "}");
  }
}

void
json_print (string& r, tree t, int mode, int indent) {
  if (is_atomic (t))
    json_print_string (r, t->label, mode);
  else if (is_func (t, TUPLE))
    json_print_array (r, t, mode, indent);
  else if (is_func (t, ATTR))
    json_print_object (r, t, mode, indent);
  else if (is_compound (t, "json-null")) {
    json_space (r); r << "null"; }
  else if (is_compound (t, "json-boolean", 1) && is_atomic (t[0])) {
    json_space (r); r << t[0]->label; }
  else if (is_compound (t, "json-number", 1) && is_atomic (t[0])) {
    json_space (r); r << t[0]->label; }
}

string
tree_to_json (tree t, int mode) {
  string r;
  json_print (r, t, mode, 0);
  return r;
}

/******************************************************************************
* Further JSON tools
******************************************************************************/

tree
json_get (tree t, tree key, int mode) {
  if (!is_func (t, ATTR)) return json_null (mode);
  for (int i=0; i+1 < N(t); i += 2)
    if (t[i] == key) return t[i+1];
  return json_null (mode);
}
