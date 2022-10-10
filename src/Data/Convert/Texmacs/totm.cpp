
/******************************************************************************
* MODULE     : totm.cpp
* DESCRIPTION: conversion of TeXmacs trees to the TeXmacs file format
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Conversion of TeXmacs trees to the present TeXmacs string format
******************************************************************************/

struct tm_writer {
  string  buf;       // the resulting string
  string  spc;       // "" or " "
  string  tmp;       // not yet flushed characters
  int     mode;      // normal: 0, verbatim: 1, mathematics: 2

  int     tab;       // number of tabs after CR
  int     xpos;      // current horizontal position in buf
  bool    spc_flag;  // true if last printed character was a space or CR
  bool    ret_flag;  // true if last printed character was a CR

  tm_writer ():
    buf (""), spc (""), tmp (""), mode (0),
    tab (0), xpos (0), spc_flag (true), ret_flag (true) {}

  void cr ();
  void flush ();
  void write_space ();
  void write_return ();
  void write (string s, bool flag= true, bool encode_space= false);
  void br (int indent= 0);
  void tag (string before, string s, string after);
  void apply (string func, array<tree> args);
  void write (tree t);
};

void
tm_writer::cr () {
  int i, n= N(buf);
  for (i=n-1; i>=0; i--)
    if ((buf[i] != ' ') || ((i>0) && (buf[i-1] == '\\')))
      break;
  if (i<n-1) {
    buf= buf (0, i+1);
    n  = n- N(buf);
    for (i=0; i<n; i++) buf << "\\ ";
  }
  buf << '\n';
  for (i=0; i<min(tab,20); i++) buf << ' ';
  xpos= min(tab,20);
}

void
tm_writer::flush () {
  int i, m= N(spc), n= N(tmp);
  if ((m+n)==0) return;
  if ((xpos+m+n) < 78) {
    buf << spc << tmp;
    xpos += m+n;
  }
  else {
    if (spc == " ") {
      if (xpos > 40) cr ();
      else {
        buf << " ";
        xpos++;
      }
    }
    if ((xpos+n) < 78) {
      buf << tmp;
      xpos += n;
    }
    else for (i=0; i<n; ) {
      if (((i+1) < n) && (tmp[i] == '\\') && (tmp[i+1] == ' ')) {
        /* not nice when searching text in a .tm file
        if (xpos >= 76) {
          buf << "\\";
          cr ();
        }
        */
        buf << "\\ ";
        xpos += 2;
        i += 2;
      }
      else {
        /* not nice when searching text in a .tm file
        if (xpos >= 77) {
          buf << "\\";
          cr ();
        }
        */
        buf << tmp[i];
        xpos++;
        i++;
      }
    }
  }
  spc= "";
  tmp= "";
}

void
tm_writer::write_space () {
  if (spc_flag) tmp << "\\ ";
  else {
    flush ();
    spc= " ";
  }
  spc_flag= true;
  ret_flag= false;
}

void
tm_writer::write_return () {
  if (ret_flag) {
    buf << "\\;\n";
    cr ();
  }
  else {
    if ((spc == " ") && (tmp == "")) {
      spc= "";
      tmp= "\\ ";
    }
    flush ();
    buf << "\n";
    cr ();
  }
  spc_flag= true;
  ret_flag= true;
}

void
tm_writer::write (string s, bool flag, bool encode_space) {
  if (flag) {
    int i, n=N(s);
    for (i=0; i<n; i++) {
      char c= s[i];
      if ((c == ' ') && (!encode_space)) write_space ();
      else {
        if (c == ' ') tmp << "\\ ";
        else if (c == '\n') tmp << "\\n";
        else if (c == '\t') tmp << "\\t";
        else if (c == '\0') tmp << "\\0";
        else if (c == '\\') tmp << "\\\\";
        else if (c == '<') tmp << "\\<";
        else if (c == '|') tmp << "\\|";
        else if (c == '>') tmp << "\\>";
        else if (c == '\34') tmp << c;
        else if (((unsigned char) c) < ' ') tmp << '\\' << (c+'@');
        else tmp << c;
        spc_flag= false;
        ret_flag= false;
      }
    }
  }
  else {
    tmp << s;
    if (N(s) != 0) {
      spc_flag= false;
      ret_flag= false;
    }
  }
}

void
tm_writer::br (int indent) {
  int i;
  flush ();
  tab += indent;
  for (i=N(buf)-1; i>=0; i--) {
    if (buf[i] == '\n') return;
    if (buf[i] != ' ') {
      cr ();
      spc_flag= true;
      ret_flag= false;
      return;
    }
  }
}

void
tm_writer::tag (string before, string s, string after) {
  write (before, false);
  write (s);
  write (after, false);
}

void
tm_writer::apply (string func, array<tree> args) {
  int i, last, n=N(args);
  for (i=n-1; i>=0; i--)
    if (is_document (args[i]) || is_func (args[i], COLLECTION))
      break;
  last= i;

  if (last >= 0) {
    /*
    tag ("<\\", func, ">");
    for (i=0; i<n; i++) {
      if (is_empty (args[i])) br ();
      else {
        br (2);
        write (args[i]);
        br (-2);
      }
      if (i<(n-1)) tag ("<|", func, ">");
    }
    tag ("</", func, ">");
    */
    
    for (i=0; i<=n; i++) {
      bool flag=
        (i<n) && (is_document (args[i]) || is_func (args[i], COLLECTION));
      if (i==0) { write ("<\\", false); write (func, true, true); }
      else if (i==last+1) {write ("</", false); write (func, true, true); }
      else if (is_document (args[i-1]) || is_func (args[i-1], COLLECTION)) {
        write ("<|", false); write (func, true, true); }
      if (i==n) {
        write (">", false);
        break;
      }

      if (flag) {
        write (">", false);
        br (2);
        write (args[i]);
        br (-2);        
      }
      else {
        write ("|", false);
        write (args[i]);
      }
    }
  }
  else {
    write ("<", false);
    write (func, true, true);
    for (i=0; i<n; i++) {
      write ("|", false);
      write (args[i]);
    }
    write (">", false);
  }
}

void
tm_writer::write (tree t) {
  if (is_atomic (t)) {
    write (t->label);
    return;
  }

  int i, n= N(t);
  switch (L(t)) {
  case RAW_DATA:
    {
      write ("<#", false);
      string s= as_string (t[0]);
      for (i=0; i<N(s); i++)
        write (as_hexadecimal ((unsigned char) s[i], 2), false);
      write (">", false);
      break;
    }
  case DOCUMENT:
    spc_flag= true;
    ret_flag= true;
    for (i=0; i<n; i++) {
      write (t[i]);
      if (i<(n-1)) write_return ();
      else if (ret_flag) write ("\\;", false);
    }
    break;
  case CONCAT:
    for (i=0; i<n; i++) write (t[i]);
    break;
  case EXPAND:
    if ((n>=1) && is_atomic (t[0])) {
      string s= t[0]->label;
      if (std_contains (s));
      else if ((N(s)>0) && (!is_iso_alpha (s)));
      else {
        apply (s, A(t(1,n)));
        break;
      }
    }
    apply (as_string (EXPAND), A(t));
    break;
  case COLLECTION:
    tag ("<\\", as_string (COLLECTION), ">");
    if (n==0) br ();
    else {
      br (2);
      for (i=0; i<n; i++) {
        write (t[i]);
        if (i<(n-1)) br ();
      }
      br (-2);
    }
    tag ("</", as_string (COLLECTION), ">");
    break;
  default:
    apply (as_string (L(t)), A(t));
    break;
  }
}

/******************************************************************************
* Conversion of TeXmacs trees to TeXmacs strings
******************************************************************************/

string
tree_to_texmacs (tree t) {
  if (!is_snippet (t)) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      if (is_compound (t[i], "style", 1)) {
        tree style= t[i][0];
        if (is_func (style, TUPLE, 1)) style= style[0];
        r[i]= copy (t[i]);
        r[i][0]= style;
      }
      else r[i]= t[i];
    t= r;
  }

  tm_writer tmw;
  tmw.write (t);
  tmw.flush ();
  return tmw.buf;
}
