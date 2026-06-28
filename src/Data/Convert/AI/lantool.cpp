
/******************************************************************************
* MODULE     : lantool.cpp
* DESCRIPTION: interface with language tool
* COPYRIGHT  : (C) 2026  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "dictionary.hpp"
#include "scheme.hpp"

/******************************************************************************
* Subroutines
******************************************************************************/

static string
as_tm (string s) {
  s= utf8_to_cork (s);
  s= replace (s, "<varspace>", " ");
  return s;
}

void
utf8_forward (string s, int& i, int nr) {
  while (nr > 0 && i < N(s)) {
    unsigned char c = s[i];
    if ((0x80 & c) == 0) i++;
    else if ((0xE0 & c) == 0xC0) i += 2;
    else if ((0xF0 & c) == 0xE0) i += 3;
    else if ((0xF8 & c) == 0xF0) i += 4;
    else i++;
    nr--;
  }
}

/******************************************************************************
* Incorporate proposals for corrections into document
******************************************************************************/

string
lantool_correct (string s, string out) {
  tree t= json_to_tree (out, 0);
  tree l= json_get (t, "matches");
  //cout << "===> Got " << N(l) << " matches\n";
  if (!is_tuple (l)) return s;
  string r;
  int pos= 0, utf8_= 0;
  for (int i=0; i<N(l); i++) {
    tree msg = json_get (l[i], "message");
    tree repl= json_get (l[i], "replacements");
    tree off = json_get (l[i], "offset");
    tree len = json_get (l[i], "length");
    tree rule= json_get (json_get (l[i], "rule"), "id");
    //cout << "===> Rule    " << rule << "\n";
    //cout << "===> Message " << msg << "\n";
    //cout << "===> Current " << utf8_ << "\n";
    //cout << "===> Offset  " << off << "\n";
    //cout << "===> Length  " << len << "\n";
    if (is_tuple (repl) && is_int (off) && is_int (len)) {
      int off_= as_int (off);
      int len_= as_int (len);
      int prev= pos;
      utf8_forward (s, pos, off_ - utf8_);
      r << s (prev, pos);
      int start= pos;
      utf8_forward (s, pos, len_);
      int end= pos;
      utf8_= off_ + len_;
      string old= s (start, end);
      //cout << "===> Prev    " << prev << "\n";
      //cout << "===> Start   " << start << "\n";
      //cout << "===> End     " << end << "\n";
      //cout << "===> Old     " << old << "\n";
      //cout << "===> Replace " << repl << "\n";
      if (occurs ("<", old) || occurs (">", old) ||
          old == "DIV" ||
          old == "id" ||
          old == "cont" || starts (old, "cont-") ||
          (N(old) >= 2 && old[0] == 'x' && old[1] >= '0' && old[1] <= '9') ||
          old == "\"" || starts (old, "\"x")) {
        r << old;
        continue;
      }
      if (!is_atomic (msg)) msg= translate ("Spell error");
      tree subs= compound ("spell-error", as_tm (old), as_tm (msg->label));
      for (int j=0; j<N(repl) && j<9; j++) {
        tree val= json_get (repl[j], "value");
        if (val != "" && is_atomic (val))
          subs << as_tm (val->label);
      }
      //cout << "===> Subs    " << subs << "\n";
      subs= as_tree (call ("spell-replace-cached", subs));
      //cout << "===> Cached  " << subs << "\n";
      r << compress_html (subs, 3);
    }
  }
  r << s (pos, N(s));
  return r;
}
