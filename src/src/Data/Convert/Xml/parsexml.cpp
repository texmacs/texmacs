
/******************************************************************************
* MODULE     : parsehtml.cpp
* DESCRIPTION: conversion of xml and html strings into logical html trees
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "convert.hpp"
#include "hashset.hpp"
#include "converter.hpp"

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
* Present limitations: we do not really parse <!DOCTYPE ...> constructs yet
* Consequently, we perform no entity replacements and it is not yet possible
* to associate default xml:space attributes to tags.
******************************************************************************/

struct xml_html_parser {
  bool html;
  int i, n;
  string s;
  array<tree> a;
  tree stack;

  xml_html_parser ();
  inline void skip_space () {
    while ((i<n) && is_space (s[i])) i++; }
  inline bool is_name_char (char c) {
    return is_alpha (c) || is_digit (c) ||
      (c == '_') || (c == ':') || (c == '.') || (c == '-') ||
      (((int) ((unsigned char) c)) >= 128); }

  void transcode ();

  string parse_until (string what);
  string parse_name ();
  string parse_entity ();
  tree parse_attribute ();
  tree parse_opening ();
  tree parse_closing ();
  tree parse_pi ();
  tree parse_comment ();
  tree parse_cdata ();
  tree parse_doctype ();
  tree parse_misc ();
  void parse ();

  // NOTE: these routines should remain there even if they are not used
  bool finalize_preserve_space (string tag);
  string finalize_space (string s, bool first, bool last);
  tree finalize_space (tree t);
  // END NOTE
  bool build_valid_child (string parent, string child);
  bool build_must_close (string tag);
  bool build_can_close (string tag);
  void build (tree& r);

  string finalize_entities (string t);
  tree finalize_entities (tree t);
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
  if (DEBUG_AUTO) cout << "TeXmacs] Loading " << fname << "\n";
  if (load_string (url ("$TEXMACS_PATH/langs/encoding", fname), s)) return;
  tree t= block_to_scheme_tree (s);
  if (!is_tuple (t)) return;

  int i, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2) &&
	is_atomic (t[i][0]) && is_atomic (t[i][1]))
      {
	string l= t[i][0]->label; if (is_quoted (l)) l= unquote (l);
	string r= t[i][1]->label; if (is_quoted (r)) r= unquote (r);
	table (l)= r;
      }
}

xml_html_parser::xml_html_parser () {
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

void
xml_html_parser::transcode () {
  i=0; n=N(s);
  string encoding;
  if (test (s, i, "<?")) {
    i+= 2;
    string target= parse_name ();
    skip_space ();
    if (target == "xml") {
      // since html==true implies we can accept horribly broken HTML, the
      // presence of an XML prolog is not enough to clear the flag.
      /* html= false; */
      while (i < n && ! test (s, i, "?>")) {
	string attname= parse_name ();
	skip_space ();
	if (! test (s, i, "=")) break;
	i++;
	skip_space ();
	string val;
	if (test (s, i, "\"")) {
	  i++;
	  val= parse_until ("\"");
	  skip_space ();	  
	}
	else if (test (s, i, "'")) {
	  i++;
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
    string s2= convert (s, encoding, "UTF-8");
    if (N(s2) == 0)
      /* conversion from specified charsed failed, do nothing (and pray) */ ;
    else
      s= s2;
  }
  else {
    // cout << "guess encoding\n" ;
    if (check_encoding (s, "UTF-8"))
      /* input encoding seems to be utf-8, do nothing */ ;
    else {
      string s2= convert (s, "ISO-8859-1", "UTF-8");
      if (N(s2) != 0) s=s2;
    }
  }
}

/******************************************************************************
* Parsing without structuring
******************************************************************************/

string
xml_html_parser::parse_until (string what) {
  int start= i;
  while ((i<n) && (!test (s, i, what))) i++;
  if (test (s, i, what)) {
    string r= s (start, i);
    i += N(what);
    return r;
  }
  return "";
}

string
xml_html_parser::parse_name () {
  int start= i;
  while ((i<n) && is_name_char (s[i])) i++;
  if (html) return locase_all (s (start, i));
  return s (start, i);
}

string
xml_html_parser::parse_entity () {
  int start= i++;
  if (i<n) {
    if (s[i] == '#') {
      i++;
      if (i<n && (s[i] == 'x' || s[i] == 'X')) {
	i++;
	while (i<n && is_hex_digit (s[i])) i++;
      }
      else while (i<n && is_digit (s[i])) i++;
    }
    else while (i<n && is_name_char (s[i])) i++;
    if (i<n && s[i] == ';') i++;
  }
  return s (start, i);
}

tree
xml_html_parser::parse_attribute () {
  string attr= parse_name (), val;
  bool no_val= false;
  skip_space ();
  if (test (s, i, "=")) i++;
  skip_space ();
  if (test (s, i, "\42")) {
    i++;
    val= parse_until ("\42");
  }
  else if (test (s, i, "'")) {
    i++;
    val= parse_until ("'");
  }
  else { // for Html
    int start= i;
    while (i<n) {
      if (is_space (s[i]) || (s[i]=='<') || (s[i]=='>')) break;
      i++;
    }
    val= s (start, i);
    no_val= (start == i);
  }
  if (!no_val) return tuple ("attr", attr, val);
  else if (attr != "") return tuple ("attr", attr);
  else return tuple ("attr");
}

tree
xml_html_parser::parse_opening () {
  i++;
  string name= parse_name ();
  tree t= tuple ("begin", name);
  while (true) {
    skip_space ();
    if ((i==n) || (s[i]=='>') || test (s, i, "/>")) break;
    tree attr= parse_attribute ();
    if (attr == tuple ("attr")) break;
    t << attr;
  }
  if (test (s, i, "/>")) { t[0]= "tag"; i+=2; }
  else if (test (s, i, ">")) i++;
  return t;
}

tree
xml_html_parser::parse_closing () {
  i+=2;
  string name= parse_name ();
  (void) parse_until (">");
  return tuple ("end", name);
}

tree
xml_html_parser::parse_pi () {
  i+=2;
  string name= parse_name ();
  skip_space ();
  return tuple ("pi", name, parse_until ("?>"));
}

tree
xml_html_parser::parse_comment () {
  i+=4;
  return tuple ("comment", parse_until ("-->"));
}

tree
xml_html_parser::parse_cdata () {
  i+=9;
  return tuple ("cdata", parse_until ("]]>"));
}

tree
xml_html_parser::parse_doctype () {
  i+=9;
  // FIXME: doctype may contain '>' in ExternalID and in internal DTD subset
  skip_space ();
  return tuple ("doctype", parse_until (">"));
}

tree
xml_html_parser::parse_misc () {
  i+=2;
  tree t= tuple ("misc");
  while (true) {
    skip_space ();
    if (test (s, i, ">")) { i++; break; }
    int start= i;
    while (i<n) {
      char c= s[i];
      if (is_space (c) || (c == '>')) break;
      i++;
    }
    t << s (start, i);
  }
  return t;
}

void
xml_html_parser::parse () {
  int start= i;
  while (i<n) {
    // cout << "Parsing " << i << "/" << n << "\n";
    if (s[i] == '<') {
      if (i>start) a << tree (s (start, i));
      if (test (s, i, "</")) a << parse_closing ();
      else if (test (s, i, "<?")) a << parse_pi ();
      else if (test (s, i, "<!--")) a << parse_comment ();
      else if (test (s, i, "<![CDATA[")) a << parse_cdata ();
      else if (test (s, i, "<!DOCTYPE")) a << parse_doctype ();
      else if (test (s, i, "<!")) a << parse_misc ();
      else a << parse_opening ();
      start= i;
    }
    else if (s[i] == '&')
      (void) parse_entity ();
    else i++;
  }
  if (i>start) a << tree (s (start, i));
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
      if (html_empty_tag_table->contains (name))
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

string
xml_html_parser::finalize_entities (string s) {
  string r;
  int i, n= N(s);

  for (i=0; i<n; /* noop */) {
    if (s[i] == '&' && i+1<n) {
      if (s[i+1] == '#') {
	i += 2;
	bool okay= false;
	string rr= convert_char_entity (s, i, okay);
	if (okay) r << rr;
	else { r << "&#"; continue; }
      }
      else {
	int start= ++i;
	while (i<n && is_name_char (s[i])) i++;
	if (i == start) { r << '&'; continue; }
	string ss= s(start, i);
	// cout << "ent-ref:  " << ss << " -- ";
	if (html_entity->contains (ss)) {
	  // Use HTML entities even for XML.
	  // HTML entity references expand to character references
	  // so they need to be finalized a second time.
	  r << finalize_entities (html_entity[ss]);
	}
	else { r << "&" << ss; continue; }
	if (i<n && s[i]==';') i++;
      }
    }
    else r << s[i++];
  }
  return r;
}

tree
xml_html_parser::finalize_entities (tree t) {
  if (is_atomic (t))
    return finalize_entities (t->label);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= finalize_entities (t[i]);
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
      else attr= tuple (t[i][1]->label, quote (t[i][2]->label));
      attrs << attr;
    }
    else if (is_tuple (t[i], "tag"))
      content << finalize_sxml (t[i]);
    else if (is_atomic (t[i]))
      content << quote (t[i]->label);
    else if (is_tuple (t[i], "pi"))
      content << tuple ("*PI*", t[i][1]->label, quote (t[i][2]->label));
    else if (is_tuple (t[i], "doctype"))
      content << tuple ("*DOCTYPE*", quote (t[i][1]->label));
    else if (is_tuple (t[i], "cdata"))
      content << quote (t[i][1]->label);
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
  s= ""; i= 0; n= N(s2);
  bool is_cr= false;
  while (i<n) {
    bool prev_is_cr= is_cr;
    is_cr= false;
    char c= s2[i];
    if (c == '\15') {
      s << '\12';
      is_cr= true;
    }
    else if (prev_is_cr && (c == '\12')) /* no-op */;
    else s << c;
    i++;
  }

  // cout << "Transcoding " << s << "\n";
  if (html) transcode ();
  // cout << HRULE << LF;
  // cout << "Parsing " << s << "\n";
  i=0; n=N(s);
  parse ();
  // cout << HRULE << LF;
  // cout << "a= " << a << "\n";
  i= 0; n= N(a); stack= tuple ("<bottom>");
  tree r= tuple ("tag", "<document>");
  build (r);
  // cout << HRULE << LF;
  // print_tree (r);
  if (html) r= finalize_entities (r);
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
