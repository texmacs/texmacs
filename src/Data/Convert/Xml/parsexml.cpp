
/******************************************************************************
* MODULE     : parsehtml.cpp
* DESCRIPTION: conversion of xml and html strings into logical html trees
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "hashset.hpp"
#include "converter.hpp"
#include "parse_string.hpp"

/******************************************************************************
* The xml/html parser aims to parse a superset of the set of valid documents.
* In other words, no attempts are made to signal error messages for
* incorrect documents; in the case of Html we even attempt to correct
* common mistakes, like badly structured documents. So correct documents
* should be parsed correctly and incorrect documents are transformed into
* correct documents in a heuristic way.
*
* The parser proceeds in three steps: the first pass does all parsing
* except for the construction of a tree structure for nested tags.
* The second stage takes care of the nesting, while heuristically
* correcting improper nested trees, and while taking care of optional
* closing tags in the case of Html. The last stage does some final
* white space and entity cleanup.
*
* Present limitations: we do not fully parse <!DOCTYPE ...> constructs yet.
* Entities which are present in the DOCTYPE definition of the document
* will be expanded. However, external DTD's are not read. Notice also that
* it is not yet possible to associate default xml:space attributes to tags.
******************************************************************************/

struct xml_html_parser {
  bool html;
  parse_string s;
  hashmap<string,string> entities;
  array<tree> a;
  int i, n;
  tree stack;

  xml_html_parser ();
  inline void skip_space () {
    while (s && is_space (s[0])) s += 1; }
  inline bool is_name_char (char c) {
    return is_alpha (c) || is_digit (c) ||
      (c == '_') || (c == ':') || (c == '.') || (c == '-') ||
      (((int) ((unsigned char) c)) >= 128); }

  string transcode (string s);

  string parse_until (string what);
  string parse_name ();
  string parse_quoted ();
  string expand_entity (string s);
  string expand_entities (string s);
  string parse_entity ();
  tree parse_attribute ();
  tree parse_opening ();
  tree parse_closing ();
  tree parse_pi ();
  tree parse_comment ();
  tree parse_cdata ();
  tree parse_misc ();
  void parse ();

  tree parse_system ();
  tree parse_public ();
  tree parse_element ();
  tree parse_attlist ();
  void parse_entity_decl ();
  tree parse_notation ();
  tree parse_doctype ();

  // NOTE: these routines should remain there even if they are not used
  bool finalize_preserve_space (string tag);
  string finalize_space (string s, bool first, bool last);
  tree finalize_space (tree t);
  // END NOTE
  bool build_valid_child (string parent, string child);
  bool build_must_close (string tag);
  bool build_can_close (string tag);
  void build (tree& r);

  tree finalize_sxml (tree t);
  tree parse (string s);
};

/******************************************************************************
* Initialization
******************************************************************************/

static hashset<string> html_empty_tag_table;
static hashset<string> html_auto_close_table;
static hashset<string> html_block_table;
static hashmap<string,string> html_entity ("");

void load_html_entities (hashmap<string, string> table, string fname) {
  string s;
  if (DEBUG_CONVERT) debug_convert << "Loading " << fname << "\n";
  if (load_string (url ("$TEXMACS_PATH/langs/encoding", fname), s, false)) return;
  tree t= block_to_scheme_tree (s);
  if (!is_tuple (t)) return;

  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2) &&
        is_atomic (t[i][0]) && is_atomic (t[i][1]))
      {
        string l= t[i][0]->label; if (is_quoted (l)) l= scm_unquote (l);
        string r= t[i][1]->label; if (is_quoted (r)) r= scm_unquote (r);
        table (l)= r;
      }
}

xml_html_parser::xml_html_parser (): entities ("") {
  if (N(html_empty_tag_table) == 0) {
    html_empty_tag_table->insert ("basefont");
    html_empty_tag_table->insert ("br");
    html_empty_tag_table->insert ("area");
    html_empty_tag_table->insert ("link");
    html_empty_tag_table->insert ("param");
    html_empty_tag_table->insert ("hr");
    html_empty_tag_table->insert ("input");
    html_empty_tag_table->insert ("col");
    html_empty_tag_table->insert ("frame");
    html_empty_tag_table->insert ("isindex");
    html_empty_tag_table->insert ("base");
    html_empty_tag_table->insert ("meta");
    html_empty_tag_table->insert ("img");
  }

  if (N(html_auto_close_table) == 0) {
    html_auto_close_table->insert ("body");
    html_auto_close_table->insert ("p");
    html_auto_close_table->insert ("dt");
    html_auto_close_table->insert ("dd");
    html_auto_close_table->insert ("li");
    html_auto_close_table->insert ("option");
    html_auto_close_table->insert ("thead");
    html_auto_close_table->insert ("tfoot");
    html_auto_close_table->insert ("tbody");
    html_auto_close_table->insert ("colgroup");
    html_auto_close_table->insert ("tr");
    html_auto_close_table->insert ("th");
    html_auto_close_table->insert ("td");
    html_auto_close_table->insert ("head");
    html_auto_close_table->insert ("html");
  }

  if (N(html_block_table) == 0) {
    html_block_table->insert ("h1");
    html_block_table->insert ("h2");
    html_block_table->insert ("h3");
    html_block_table->insert ("h4");
    html_block_table->insert ("h5");
    html_block_table->insert ("h6");
    html_block_table->insert ("ul");
    html_block_table->insert ("ol");
    html_block_table->insert ("li");
    html_block_table->insert ("dl");
    html_block_table->insert ("dd");
    html_block_table->insert ("dt");
    html_block_table->insert ("pre");
    html_block_table->insert ("div");
    html_block_table->insert ("p");
    html_block_table->insert ("noscript");
    html_block_table->insert ("blockquote");
    html_block_table->insert ("form");
    html_block_table->insert ("hr");
    html_block_table->insert ("table");
    html_block_table->insert ("fieldset");
    html_block_table->insert ("address");
  }

  if (N (html_entity) == 0) {
    load_html_entities (html_entity, "HTMLlat1.scm");
    load_html_entities (html_entity, "HTMLspecial.scm");
    load_html_entities (html_entity, "HTMLsymbol.scm");
  }
}

/******************************************************************************
* Transcoding input to UTF-8
******************************************************************************/

// TODO: support BOM and other bells and whistles
// http://www.w3.org/TR/REC-xml#sec-guessing

// TODO: support HTML http-equiv Content-Type
// http://www.w3.org/TR/html4/charset.html#h-5.2.2

// Currently, the input encoding is expected to be ASCII-compatible.
// If no <?xml?> prolog is found, the encoding is assumed to be UTF-8 or
// ISO-8859-1 if iconv cannot perform an utf8->utf8 conversion.

string
xml_html_parser::transcode (string s2) {
  s= parse_string (s2);

  string encoding;
  if (test (s, "<?")) {
    s += 2;
    string target= parse_name ();
    skip_space ();
    if (target == "xml") {
      // since html==true implies we can accept horribly broken HTML, the
      // presence of an XML prolog is not enough to clear the flag.
      /* html= false; */
      while (s && !test (s, "?>")) {
        string attname= parse_name ();
        skip_space ();
        if (!test (s, "=")) break;
        s += 1;
        skip_space ();
        string val;
        if (test (s, "\"")) {
          s += 1;
          val= parse_until ("\"");
          skip_space ();
        }
        else if (test (s, "'")) {
          s += 1;
          val= parse_until ("'");
          skip_space ();
        }
        if (attname == "encoding") {
          encoding= upcase_all (val);
          break;
        }
      }
    }
  }

  if (N(encoding) != 0) {
    // cout << "encoding was specified\n" ;
    string s3= convert (s2, encoding, "UTF-8");
    if (N(s3) == 0)
      /* conversion from specified charset failed, do nothing (and pray) */ ;
    else return s3;
  }
  else {
    // cout << "guess encoding\n" ;
    if (check_encoding (s2, "UTF-8"))
      /* input encoding seems to be utf-8, do nothing */ ;
    else {
      string s3= convert (s2, "ISO-8859-1", "UTF-8");
      if (N(s3) != 0) return s3;
    }
  }

  return s2;
}

/******************************************************************************
* Parsing without structuring
******************************************************************************/

string
xml_html_parser::parse_until (string what) {
  string r;
  while (s && !test (s, what)) r << s->read (1);
  if (test (s, what)) s += N(what);
  return expand_entities (r);
}

string
xml_html_parser::parse_name () {
  string r;
  while (s && is_name_char (s[0])) r << s->read (1);
  if (html) return locase_all (r);
  return expand_entities (r);
}

string
xml_html_parser::expand_entity (string s) {
  if (entities->contains (s)) return entities[s];
  else if (s[0] == '&') {
    if (N(s)>1 && s[1] == '#') {
      int i= 2;
      bool okay= false;
      string r= convert_char_entity (s, i, okay);
      if (okay) return r;
      return s;
    }
    else if (html) {
      string ss= s (1, s [N(s)-1] == ';' ? N(s)-1 : N(s));
      if (html_entity->contains (ss))
        // HTML entity references expand to character references
        // so they need to be finalized a second time.
        return expand_entity (html_entity [ss]);
    }
  }
  return s;
}

string
xml_html_parser::expand_entities (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; ) {
    if (s[i] == '&' || s[i] == '%') {
      int start= i++;
      if (i<n && s[i] == '#') {
        i++;
        if (i<n && (s[i] == 'x' || s[i] == 'X')) {
          i++;
          while (i<n && is_hex_digit (s[i])) i++;
        }
        else while (i<n && is_digit (s[i])) i++;
      }
      else while (i<n && is_name_char (s[i])) i++;
      if (i<n && s[i] == ';') i++;
      r << expand_entity (s (start, i));
    }
    else r << s[i++];
  }
  if (r == s) return r;
  return expand_entities (r);
}

string
xml_html_parser::parse_entity () {
  string r= s->read (1);
  if (test (s, "#")) {
    r << s->read (1);
    if (test (s, "x") || test (s, "X")) {
      r << s->read (1);
      while (s && is_hex_digit (s[0])) r << s->read (1);
    }
    else while (s && is_digit (s[0])) r << s->read (1);
  }
  else while (s && is_name_char (s[0])) r << s->read (1);
  if (test (s, ";")) r << s->read (1);
  string x= expand_entity (r);
  if (x == r || r == "&lt;" || r == "&amp;") return x;
  s->write (x);
  return "";
}

string
xml_html_parser::parse_quoted () {
  if (test (s, "\42")) {
    s += 1;
    return parse_until ("\42");
  }
  if (test (s, "'")) {
    s += 1;
    return parse_until ("'");
  }
  return "";
}

tree
xml_html_parser::parse_attribute () {
  string attr= parse_name (), val;
  bool no_val= false;
  skip_space ();
  if (test (s, "=")) s += 1;
  skip_space ();
  if (test (s, "\42") || test (s, "'"))
    val= parse_quoted ();
  else { // for Html
    string r;
    while (s) {
      if (is_space (s[0]) || (s[0]=='<') || (s[0]=='>')) break;
      r << s->read (1);
    }
    val   = r;
    no_val= N(r) == 0;
  }
  if (!no_val) return tuple ("attr", attr, val);
  else if (attr != "") return tuple ("attr", attr);
  else return tuple ("attr");
}

tree
xml_html_parser::parse_opening () {
  s += 1;
  string name= parse_name ();
  tree t= tuple ("begin", name);
  while (true) {
    skip_space ();
    if (!s || s[0] == '>' || test (s, "/>")) break;
    tree attr= parse_attribute ();
    if (attr == tuple ("attr")) break;
    t << attr;
  }
  if (test (s, "/>")) { t[0]= "tag"; s += 2; }
  else if (test (s, ">")) s += 1;
  return t;
}

tree
xml_html_parser::parse_closing () {
  s += 2;
  string name= parse_name ();
  (void) parse_until (">");
  return tuple ("end", name);
}

tree
xml_html_parser::parse_pi () {
  s += 2;
  string name= parse_name ();
  skip_space ();
  return tuple ("pi", name, parse_until ("?>"));
}

tree
xml_html_parser::parse_comment () {
  s += 4;
  return tuple ("comment", parse_until ("-->"));
}

tree
xml_html_parser::parse_cdata () {
  s += 9;
  return tuple ("cdata", parse_until ("]]>"));
}

tree
xml_html_parser::parse_misc () {
  s += 2;
  tree t= tuple ("misc");
  while (true) {
    skip_space ();
    if (test (s, ">")) { s += 1; break; }
    string r;
    while (s) {
      if (is_space (s[0]) || (s[0] == '>')) break;
      r << s->read (1);
    }
    t << r;
  }
  return t;
}

void
xml_html_parser::parse () {
  string r;
  while (s) {
    if (s[0] == '<') {
      if (N(r) != 0) { a << tree (r); }
      if (test (s, "</")) a << parse_closing ();
      else if (test (s, "<?")) a << parse_pi ();
      else if (test (s, "<!--")) a << parse_comment ();
      else if (test (s, "<![CDATA[")) a << parse_cdata ();
      else if (test (s, "<!DOCTYPE")) a << parse_doctype ();
      else if (test (s, "<!")) a << parse_misc ();
      else a << parse_opening ();
      r= "";
    }
    else if (s[0] == '&') r << parse_entity ();
    else r << s->read (1);
  }
  if (N(r) != 0) a << tree (r);
}

/******************************************************************************
* Parsing the document type
******************************************************************************/

tree
xml_html_parser::parse_system () {
  s += 6;
  tree st= tuple ("system");
  skip_space ();
  st << parse_quoted ();
  return st;
}

tree
xml_html_parser::parse_public () {
  s += 6;
  tree st= tuple ("public");
  skip_space ();
  st << parse_quoted ();
  skip_space ();
  st << parse_quoted ();
  return st;
}

tree
xml_html_parser::parse_element () {
  s += 9;
  return tuple ("element", parse_until (">"));
}

tree
xml_html_parser::parse_attlist () {
  s += 9;
  return tuple ("attlist", parse_until (">"));
}

void
xml_html_parser::parse_entity_decl () {
  s += 8;
  skip_space ();
  bool parameter= test (s, "%");
  if (parameter) { s += 1; skip_space (); }
  string name= parse_name ();
  if (parameter) name= "%" * name * ";";
  else name= "&" * name * ";";
  skip_space ();

  if (test (s, "SYSTEM") || test (s, "PUBLIC")) {
    // TODO: allow for loading of external entities using wget
    if (test (s, "SYSTEM")) (void) parse_system ();
    else (void) parse_public ();
    skip_space ();
    if (test (s, "NDATA")) {
      s += 5;
      skip_space ();
      (void) parse_name ();
    }
  }
  else {
    string val= parse_quoted ();
    val= expand_entities (val);
    entities (name) = val;
    // cout << name << " := " << val << "\n";
  }

  skip_space ();
  if (test (s, ">")) s += 1;
}

tree
xml_html_parser::parse_notation () {
  s += 10;
  return tuple ("notation", parse_until (">"));
}

tree
xml_html_parser::parse_doctype () {
  s += 9;
  tree dt= tuple ("doctype");
  skip_space ();
  dt << parse_name ();
  skip_space ();
  if (test (s, "SYSTEM")) dt << parse_system ();
  else if (test (s, "PUBLIC")) dt << parse_public ();
  skip_space ();

  if (test (s, "[")) {
    s += 1;
    while (s) {
      skip_space ();
      if (test (s, "]")) { s += 1; break; }
      else if (test (s, "<!ELEMENT")) dt << parse_element ();
      else if (test (s, "<!ATTLIST")) dt << parse_cdata ();
      else if (test (s, "<!ENTITY")) parse_entity_decl ();
      else if (test (s, "<!NOTATION")) a << parse_notation ();
      else if (test (s, "<?")) dt << parse_pi ();
      else if (test (s, "<!--")) dt << parse_comment ();
      else if (s[0] == '&' || s[0] == '%') (void) parse_entity ();
      else s += 1;
    }
  }

  skip_space ();
  if (test (s, ">")) s += 1;
  return dt;
}

/******************************************************************************
* Building the structured parse tree with error correction
******************************************************************************/

bool
xml_html_parser::build_valid_child (string parent, string child) {
  if (!html) return true;
  if ((parent == "<bottom>") || (parent == "html") || (parent == "body"))
    return true;
  if (html_empty_tag_table->contains (parent)) return false;
  if (!html_auto_close_table->contains (child)) return true;
  if (parent == "p") return !html_block_table->contains (child);
  if ((child == "dt") || (child == "dd")) return parent == "dl";
  if (child == "li")
    return (parent == "ul") || (parent == "ol") ||
           (parent == "dir") || (parent == "menu");
  if (child == "option") return (parent == "select") || (parent == "optgroup");
  if ((child == "thead") || (child == "tfoot") || (child == "tbody"))
    return parent == "table";
  if (child == "colgroup") return parent == "table";
  if (child == "col") return (parent == "table") || (parent == "colgroup");
  if (child == "tr")
    return (parent == "table") || (parent == "thead") ||
           (parent == "tfoot") || (parent == "tbody");
  if ((child == "th") || (child == "td"))
    return (parent == "tr") ||
           (parent == "table") || (parent == "thead") ||
           (parent == "tfoot") || (parent == "tbody");
  return true;
}

bool
xml_html_parser::build_must_close (string tag) {
  if (build_valid_child (stack[0]->label, tag)) return false;
  // if !html, we have already returned false
  tree counter= stack;
  while (counter != tuple ("<bottom>")) {
    if (build_valid_child (counter[0]->label, tag)) return true;
    counter= counter[1];
  }
  // since <html> and <body> can have any child we only get here when parsing
  // something where both are omitted and we can close nodes up to the root.
  return true;
}

bool
xml_html_parser::build_can_close (string tag) {
  if (N(stack) < 2) return false;
  tree counter= stack[1];
  while (counter != tuple ("<bottom>")) {
    if (counter[0]->label == tag) return true;
    counter= counter[1];
  }
  return false;
}

void
xml_html_parser::build (tree& r) {
  while (i<n) {
    if (is_tuple (a[i], "begin")) {
      string name= a[i][1]->label;
      if (build_must_close (name)) return;
      tree sub= copy (a[i]); sub[0]= "tag";
      i++;
      if (html && html_empty_tag_table->contains (name))
        r << sub;
      else {
        stack= tuple (name, stack);
        build (sub);
        r << sub;
        stack= stack[1];
      }
    }
    else if (is_tuple (a[i], "end")) {
      if (stack[0]->label == a[i][1]->label) { i++; return; }
      if (build_can_close (a[i][1]->label)) return;
      i++;
    }
    else r << a[i++];
  }
}

/******************************************************************************
* Finalization
******************************************************************************/

bool
xml_html_parser::finalize_preserve_space (string tag) {
  return tag == "pre";
}

string
xml_html_parser::finalize_space (string s, bool first, bool last) {
  int i, n= N(s);
  string r;
  bool flag= first;
  for (i=0; i<n; i++)
    if (is_space (s[i])) {
      if (!flag) r << ' ';
      flag= true;
    }
    else {
      r << s[i];
      flag= false;
    }
  n= N(r);
  if (last && (n>0) && (r[n-1] == ' '))
    r->resize (n-1);
  return r;
}

tree
xml_html_parser::finalize_space (tree t) {
  if (is_atomic (t) || (!is_tuple (t, "tag"))) return t;
  else {
    int i, n= N(t);
    tree r= tuple (t[0], t[1]);
    int first= -1, last= -1;
    for (i=2; i<n; i++)
      if (!is_tuple (t[i], "attr")) {
        first= i; break;
      }
    if (!is_tuple (t[n-1], "attr"))
      last= n-1;
    (void) first; (void) last;
    for (i=2; i<n; i++) {
      if (is_atomic (t[i])) {
        if (finalize_preserve_space (t[1]->label)) r << t[i];
        else {
          string s= finalize_space (t[i]->label, i==2, i==(n-1));
          if (s != "") r << s;
        }
      }
      else if (is_tuple (t[i], "tag")) r << finalize_space (t[i]);
      else r << t[i];
    }
    return r;
  }
}

tree
xml_html_parser::finalize_sxml (tree t) {
  if (!is_tuple (t, "tag")) return ""; // sanity
  int i, n= N(t);
  tree tag = tuple (t[1]);
  if (t[1] == "<document>") tag= tuple ("*TOP*");
  tree attrs = tuple ("@");
  tree content = tuple ();
  for (i=2; i<n; i++)
    if (is_tuple (t[i], "attr")) {
      tree attr;
      if (N(t[i]) == 2) attr= tuple (t[i][1]);
      else attr= tuple (t[i][1]->label, raw_quote (t[i][2]->label));
      attrs << attr;
    }
    else if (is_tuple (t[i], "tag"))
      content << finalize_sxml (t[i]);
    else if (is_atomic (t[i]))
      content << raw_quote (t[i]->label);
    else if (is_tuple (t[i], "pi"))
      content << tuple ("*PI*", t[i][1]->label, raw_quote (t[i][2]->label));
    else if (is_tuple (t[i], "doctype"))
      // TODO: convert DTD declarations
      content << tuple ("*DOCTYPE*", raw_quote (t[i][1]->label));
    else if (is_tuple (t[i], "cdata"))
      content << raw_quote (t[i][1]->label);

  if (N(attrs) > 1) tag << attrs;
  tag << A(content);
  return tag;
}

/******************************************************************************
* Building the structured parse tree with error correction
******************************************************************************/

tree
xml_html_parser::parse (string s2) {
  // end of line handling
  string s3;
  i= 0, n= N(s2);
  bool is_cr= false;
  while (i<n) {
    bool prev_is_cr= is_cr;
    is_cr= false;
    char c= s2[i];
    if (c == '\15') {
      s3 << '\12';
      is_cr= true;
    }
    else if (prev_is_cr && (c == '\12')) /* no-op */;
    else s3 << c;
    i++;
  }
  s2= s3;

  // cout << "Transcoding " << s2 << "\n";
  if (html) s2= transcode (s2);
  // cout << HRULE << LF;
  s= parse_string (s2);
  //cout << "Parsing " << s << "\n";
  parse ();
  // cout << HRULE << LF;
  // cout << "a= " << a << "\n";
  i= 0; n= N(a); stack= tuple ("<bottom>");
  tree r= tuple ("tag", "<document>");
  build (r);
  // cout << HRULE << LF;
  // print_tree (r);
  r= finalize_sxml (r);
  // cout << HRULE << LF;
  // print_tree (r);
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
parse_xml (string s) {
  xml_html_parser parser;
  parser.html= false;
  tree t= parser.parse (s);
  return t;
}

tree
parse_html (string s) {
  xml_html_parser parser;
  parser.html= true;
  tree t= parser.parse (s);
  return t;
}
