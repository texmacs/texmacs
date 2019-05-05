
/******************************************************************************
* MODULE     : metadata_post.cpp
* DESCRIPTION: postprocess metadata
* COPYRIGHT  : (C) 2019 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "metadata.hpp"
#include "vars.hpp"
#include "universal.hpp"

/******************************************************************************
* Useful subroutines
******************************************************************************/

static bool
is_reference (tree t, bool any= true) {
  if (is_compound (t, "math", 1)) return is_reference (t[0], any);
  if (is_func (t, WITH)) return is_reference (t[N(t)-1], any);
  if (is_func (t, RSUP, 1)) return true;
  if (is_compound (t, "footnote")) return any;
  return false;
}

static tree
reference_radical (tree t) {
  if (is_compound (t, "math", 1)) return reference_radical (t[0]);
  if (is_func (t, WITH)) return reference_radical (t[N(t)-1]);
  if (is_func (t, RSUP, 1)) return reference_radical (t[0]);
  return t;
}

static bool
contains_label (tree t) {
  if (!is_concat (t)) return false;
  for (int i=0; i<N(t); i++)
    if (is_reference (t[i], false)) return true;
  return false;
}

static bool
is_tt (tree t) {
  if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt") return true;
  return false;
}

static tree
get_tt (tree t) {
  if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt") return t[2];
  return t;
}

static tree
author_data (tree t) {
  return compound ("doc-author", compound ("author-data", t));
}

static bool
is_author_with_name (tree t) {
  if (!is_compound (t, "doc-author", 1)) return false;
  if (!is_compound (t[0], "author-data")) return false;
  for (int i=0; i<N(t[0]); i++)
    if (is_compound (t[0][i], "author-name")) return true;
  return false;
}

static bool
is_author_with_affiliation (tree t) {
  if (!is_compound (t, "doc-author", 1)) return false;
  if (!is_compound (t[0], "author-data")) return false;
  for (int i=0; i<N(t[0]); i++)
    if (is_compound (t[0][i], "author-affiliation")) return true;
  return false;
}

static tree
get_author_name (tree t) {
  if (!is_compound (t, "doc-author", 1)) return "";
  if (!is_compound (t[0], "author-data")) return "";
  for (int i=0; i<N(t[0]); i++)
    if (is_compound (t[0][i], "author-name", 1)) return t[0][i][0];
  return "";
}

static bool
is_author_without_name (tree t) {
  if (!is_compound (t, "doc-author", 1)) return false;
  if (!is_compound (t[0], "author-data")) return false;
  for (int i=0; i<N(t[0]); i++)
    if (is_compound (t[0][i], "author-name")) return false;
  return true;
}

static bool
is_author_without_email (tree t) {
  if (!is_compound (t, "doc-author", 1)) return false;
  if (!is_compound (t[0], "author-data")) return false;
  for (int i=0; i<N(t[0]); i++)
    if (is_compound (t[0][i], "author-email")) return false;
  return true;
}

static bool
is_whitespace (string s) {
  for (int i=0; i<N(s); i++)
    if (s[i] != ' ') return false;
  return true;
}

/******************************************************************************
* Preprocessing
******************************************************************************/

static string
replace_email_prefix (string s) {
  s= replace (s, "mail :", "mail:");
  s= replace (s, "Email", "email");
  s= replace (s, "e-mail", "email");
  s= replace (s, "E-mail", "email");
  s= replace (s, "address :", "address:");
  s= replace (s, "email addresses", "email");
  s= replace (s, "email address", "email");
  return s;
}

static tree
tag_email (string s) {
  s= replace_email_prefix (s);
  int pos= search_forwards ("email:", s);
  if (pos >= 0) {
    int start= pos + 6;
    while (start<N(s) && s[start] == ' ') start++;
    int end= start;
    while (true) {
      int prev= end;
      while (end < N(s) && (s[end] == ' ' || s[end] == ',')) end++;
      for (; end < N(s); end++)
        if (s[end] == '{')
          while (end < N(s) && s[end] != '}') end++;
        else if (s[end] == ' ' || s[end] == ',') break;
      if (!occurs ("@", s (prev, end))) {
        end= prev;
        break;
      }
    }
    if (occurs ("@", s (start, end))) {
      string ss= s (start, end);
      while (ends (ss, ".")) ss= ss (0, N(ss)-1);
      tree it (WITH, FONT_FAMILY, "tt", ss);
      tree cc (CONCAT, s (0, start), it, s (end, N(s)));
      return simplify_concat (cc);
    }
  }
  return s;
}

static tree
cleanup (tree t) {
  if (is_document (t)) {
    tree r (L(t));
    for (int i=0; i<N(t); i++)
      r << cleanup (t[i]);
    if (N(r) == 1) r= r[0];
    return r;
  }
  if (!is_concat (t)) t= tree (CONCAT, t);
  tree r (CONCAT);
  for (int i=0; i<N(t); i++)
    if (is_atomic (t[i])) {
      string s= t[i]->label;
      if (N(r) == 0)
        while (starts (s, " "))
          s= s (1, N(s));
      r << tag_email (s);
    }
    else if (is_func (t[i], VSPACE));
    else if (is_func (t[i], VAR_VSPACE));
    else if (is_compound (t[i], "no-indent", 0));
    else if (is_compound (t[i], "nbsp", 0)) r << tree (" ");
    else if (is_func (t[i], WITH, 3) &&
             t[i][0] == FONT_FAMILY && t[i][1] == "tt") {
      tree c= copy (t[i]);
      tree b= cleanup (c[2]);
      if (is_concat (b)) {
        tree cc (CONCAT);
        for (int i=0; i<N(b); i++)
          if (is_func (b[i], NEXT_LINE, 0));
          else if (is_func (b[i], SPACE));
          else cc << b[i];
        b= simplify_concat (cc);
      }
      c[2]= b;
      r << c;
    }
    else if (is_func (t[i], WITH, 3) && t[i][0] == FONT_SIZE)
      r << cleanup (t[i][2]);
    else if (is_func (t[i], WITH, 3) && t[i][0] == FONT_SERIES)
      r << cleanup (t[i][2]);
    else if (is_func (t[i], WITH, 5) && t[i][0] == FONT_SIZE)
      r << cleanup (tree (WITH, t[i][2], t[i][3], t[i][4]));
    else if (is_func (t[i], WITH, 5) && t[i][2] == FONT_SIZE)
      r << cleanup (tree (WITH, t[i][0], t[i][1], t[i][4]));
    else if (is_func (t[i], WITH, 5) && t[i][0] == FONT_SERIES)
      r << cleanup (tree (WITH, t[i][2], t[i][3], t[i][4]));
    else if (is_func (t[i], WITH, 5) && t[i][2] == FONT_SERIES)
      r << cleanup (tree (WITH, t[i][0], t[i][1], t[i][4]));
    else if (is_func (t[i], WITH)) {
      tree c= copy (t[i]);
      c[N(c)-1]= cleanup (c[N(c)-1]);
      r << c;
    }
    else r << t[i];
  while (N(r) > 0 &&
         (is_func (r[N(r)-1], NEXT_LINE, 0) ||
          (is_atomic (r[N(r)-1]) && is_whitespace (r[N(r)-1]->label))))
    r= r (0, N(r)-1);
  return simplify_concat (r);
}

static bool
is_misc_address (tree t) {
  if (is_concat (t) || is_document (t)) {
    for (int i=0; i<N(t); i++)
      if (is_misc_address (t[i])) return true;
    return false;
  }
  else if (is_tt (t)) return true;
  else if (is_atomic (t)) {
    string s= t->label;
    if (occurs ("partment", s)) return true;
    if (occurs ("niversit", s)) return true;
    if (occurs ("England", s)) return true;
    if (occurs ("France", s)) return true;
    if (occurs ("Israel", s)) return true;
    if (occurs ("USA", s)) return true;
    return false;
  }
  else return false;
}

static tree
promote_misc_address (tree t) {
  if (is_document (t)) {
    tree r (CONCAT);
    for (int i=0; i<N(t); i++) {
      if (i>0) r << tree (NEXT_LINE);
      r << t[i];
    }
    return simplify_concat (r);
  }
  else return t;
}

static tree
clean_doc_data (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "author-name", 1))
    return compound ("author-name", cleanup (t[0]));
  else if (is_compound (t, "author-affiliation", 1))
    return compound ("author-affiliation", cleanup (t[0]));
  else if (is_compound (t, "author-note", 1))
    return compound ("author-note", cleanup (t[0]));
  else if (is_compound (t, "author-misc", 1)) {
    tree r= compound ("author-misc", cleanup (t[0]));
    if (is_misc_address (r[0]))
      return compound ("author-affiliation", promote_misc_address (r[0]));
    return r;
  }
  else if (is_compound (t, "doc-date", 1)) {
    tree r= compound ("doc-date", cleanup (t[0]));
    if (is_misc_address (r[0])) {
      tree prom= promote_misc_address (r[0]);
      return author_data (compound ("author-affiliation", prom));
    }
    return r;
  }
  else {
    tree r (L(t), N(t));
    for (int i=0; i<N(t); i++)
      r[i]= clean_doc_data (t[i]);
    return r;
  }
}

/******************************************************************************
* Extract author and affiliation information
******************************************************************************/

static void
explode_affiliation (tree t, array<tree>& affs) {
  //cout << "Explode affiliation " << t << LF;
  if (is_atomic (t)) { affs << t; return; }
  int i= 0, n= N(t);
  while (i<n) {
    tree aff (CONCAT);
    while (i<n) {
      if ((L(t[i]) == NEXT_LINE || L(t[i]) == NEW_LINE) &&
          i+1<n && is_reference (t[i+1], false))
        break;
      else aff << t[i++];
    }
    if (N(aff) > 0) affs << simplify_concat (aff);
    if (i<n) i++;
  }
}

static bool
has_lowercase (tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    return uni_upcase_all (s) != s;
  }
  else if (is_concat (t)) {
    for (int i=0; i<N(t); i++)
      if (has_lowercase (t[i])) return true;
    return false;
  }
  else return false;
}

static void
add_author (array<tree>& auts, tree& aut) {
  if (N(aut) == 0) return;
  if (N(auts) == 0 || has_lowercase (aut))
    auts << simplify_concat (aut);
  else {
    aut= tree (CONCAT, auts[N(auts)-1], ", ", aut);
    auts[N(auts)-1]= simplify_concat (aut);
  }
  aut= tree (CONCAT);
}
  
static void
explode_author (tree t, array<tree>& auts, array<tree>& affs) {
  //cout << "Explode author " << t << LF;
  bool start= true;
  int i= 0, n= N(t);
  tree aut (CONCAT);
  while (i<n) {
    if (is_atomic (t[i])) {
      //cout << "    -- " << t[i] << LF;
      string s= t[i]->label;
      while (N(s)>0 && s[0]==' ') s= s (1, N(s));
      while (s != "") {
        int pos= search_forwards (",", s);
        if (pos < 0) pos= search_forwards (" and ", s);
        if (pos < 0 && test (s, 0, "and ")) pos= 0;
        if (pos >= 0) {
          if (pos > 0) aut << tree (s (0, pos));
          add_author (auts, aut);
          if (test (s, pos, ",")) pos++;
          if (test (s, pos, " and ")) pos += 5;
          if (test (s, pos, "and ")) pos += 4;
          while (test (s, pos, " ")) pos++;
          s= s (pos, N(s));
          continue;
        }
        aut << tree (s);
        start= false;
        break;
      }
    }
    else if (is_reference (t[i]) || is_tt (t[i])) {
      if (start && N(aut) == 0 &&
          (is_reference (t[i], false) || is_tt (t[i]))) {
        tree aff (CONCAT);
        while (i<n && L(t[i]) != NEXT_LINE && L(t[i]) != NEW_LINE)
          aff << t[i++];
        i++;
        break;
      }
      else {
        aut << t[i];
        start= false;
      }
    }
    else if (L(t[i]) == NEXT_LINE || L(t[i]) == NEW_LINE) {
      i++;
      break;
    }
    else {
      aut << t[i];
      start= false;
    }
    if (i<n) i++;
  }
  add_author (auts, aut);
  if (i < N(t)) explode_affiliation (t (i, N(t)), affs);
}

/******************************************************************************
* Rewrite author data
******************************************************************************/

static bool
matches (tree aff, tree ref) {
  if (!is_concat (aff)) return false;
  for (int i=0; i<N(aff); i++)
    if (is_reference (aff[i], false)) {
      tree lab= reference_radical (aff[i]);
      if (ref == lab) return true;
      if (is_atomic (ref)) {
        string s= ref->label;
        for (int k=0; k<N(s); k++)
          if (lab == s(k,k+1)) return true;
      }
    }
  return false;
}

static tree
rewrite_affiliation (tree aff) {
  if (!is_concat (aff)) return aff;
  tree r (CONCAT);
  for (int i=0; i<N(aff); i++)
    if (is_reference (aff[i])) {
      i++;
      if (i<N(aff) && is_atomic (aff[i])) {
        string s= aff[i]->label;
        while (starts (s, " ")) s= s (1, N(s));
        r << tree (s);
      }
    }
    else r << aff[i];
  return simplify_concat (r);
}

static array<tree>
rewrite_footnote (tree note) {
  array<tree> r;
  if (!is_concat (note)) note= tree (CONCAT, note);
  tree rew (CONCAT);
  for (int i=0; i<N(note); i++)
    if (is_atomic (note[i])) {
      string s= note[i]->label;
      s= replace_email_prefix (s);
      if (i+1<N(note) && is_tt (note[i+1])) {
        while (ends (s, " ")) s= s (0, N(s)-1);
        while (ends (s, ":")) s= s (0, N(s)-1);
        while (ends (s, " ")) s= s (0, N(s)-1);
        if (ends (s, "email") || ends (s, "Email")) s= s (0, N(s)-5);
        if (ends (s, "web") || ends (s, "Web")) s= s (0, N(s)-3);
        if (ends (s, "homepage") || ends (s, "Homepage")) s= s (0, N(s)-8);
        if (ends (s, "home page") || ends (s, "Home page")) s= s (0, N(s)-9);
      }
      if (i>0 && is_tt (note[i-1])) {
        while (starts (s, ",")) s= s (1, N(s));
        while (starts (s, " ")) s= s (1, N(s));
      }
      rew << tag_email (s);
    }
    else if (is_tt (note[i])) {
      tree v= get_tt (note[i]);
      if (is_atomic (v) && starts (v->label, "http"))
        r << compound ("author-homepage", v);
      else if (is_atomic (v) && occurs ("@", v->label))
        r << compound ("author-email", v);
    }
    else rew << note[i];
  rew= simplify_concat (rew);
  if (rew != "") r << compound ("author-note", rew);
  return r;
}

static tree
rewrite_author_data (tree aut, array<tree> affs, array<bool>& done) {
  tree data= compound ("author-data", compound ("author-name", aut));
  if (is_concat (aut)) {
    tree rew (CONCAT);
    for (int i=0; i<N(aut); i++)
      if (is_atomic (aut[i])) rew << aut[i];
      else if (is_compound (aut[i], "footnote", 1))
        data << rewrite_footnote (aut[i][0]);
      else if (is_reference (aut[i])) {
        tree ref= reference_radical (aut[i]);
        for (int j=0; j<N(affs); j++)
          if (matches (affs[j], ref)) {
            done[j]= true;
            data << compound ("author-affiliation",
                              rewrite_affiliation (affs[j]));
          }
      }
      else data << aut[i];
    data[0][0]= simplify_concat (rew);
  }
  return compound ("doc-author", data);
}

static tree
maximal_clustering (tree t) {
  for (int i=0; i<N(t); i++)
    if (is_compound (t[i], "doc-title-options"))
      return t;
  tree r= copy (t);
  r << compound ("doc-title-options", "cluster-all");
  return r;
}

static tree
postprocess_doc_data (tree t) {
  //cout << "Postprocess " << t << LF;
  bool cluster= false;
  tree r (L(t));
  for (int i=0; i<N(t); i++)
    if (is_author_with_name (t[i])) {
      for (int j=0; j<N(t[i][0]); j++)
        if (is_compound (t[i][0][j], "author-name")) {
          tree u= t[i][0][j][0];
          //cout << "Postprocess author " << u << LF;
          if (!is_concat (u)) u= tree (CONCAT, u);
          array<tree> auts, affs, extra;
          explode_author (u, auts, affs);
          for (int k=0; k<N(t[i][0]); k++)
            if (k != j) {
              //cout << "  Extra " << t[i][0][k] << LF;
              if (is_compound (t[i][0][k], "author-affiliation", 1))
                explode_affiliation (t[i][0][k][0], affs);
              else extra << t[i][0][k];
            }
          if (i+1 < N(t) &&
              is_compound (t[i+1], "doc-author", 1) &&
              is_compound (t[i+1][0], "author-data", 1) &&
              is_compound (t[i+1][0][0], "author-affiliation", 1) &&
              contains_label (t[i+1][0][0][0])) {
            explode_affiliation (t[i+1][0][0][0], affs);
            i++;
          }
          //cout << "authors= " << auts << LF;
          //cout << "affiliations= " << affs << LF;
          array<bool> done;
          for (int k=0; k<N(affs); k++) done << false;
          for (int k=0; k<N(auts); k++)
            r << rewrite_author_data (auts[k], affs, done);
          //cout << "Done? " << done << LF;
          for (int k=0; k<N(done); k++)
            if (!done[k])
              r << author_data (compound ("author-affiliation", affs[k]));
          for (int k=0; k<N(extra); k++)
            r << author_data (extra[k]);
          cluster= cluster || N(auts) > 1;
          break;
        }
    }
    else r << t[i];
  if (cluster) r= maximal_clustering (r);
  return r;
}

/******************************************************************************
* Merging duplicate entries
******************************************************************************/

static tree
remove_author_less (tree t) {
  bool ok= false;
  tree r (L(t));
  for (int i=0; i<N(t); i++) {
    if (is_author_with_name (t[i])) { ok= true; r << t[i]; }
    else if (ok || !is_author_without_name (t[i])) r << t[i];
  }
  return r;
}

static tree
merge_duplicates_sub (tree t) {
  tree r (L(t));
  for (int i=0; i<N(t); i++) {
    bool found= false;
    if (!is_author_without_name (t[i]) || !is_author_with_affiliation (t[i]))
      for (int j=0; j<N(r) && !found; j++)
        if (t[j] == t[i]) found= true;
    if (!found) r << t[i];
  }
  return r;
}
    
static tree
merge_duplicates (tree orig) {
  tree t= merge_duplicates_sub (remove_author_less (orig));
  tree r (L(t));
  for (int i=0; i<N(t); i++) {
    bool found= false;
    if (is_author_with_name (t[i]))
      for (int j=0; j<N(r) && !found; j++)
        if (is_author_with_name (r[j]))
          if (get_author_name (t[i]) == get_author_name (r[j])) {
            r[j][0] << A(t[i][0]);
            r[j][0]= merge_duplicates_sub (r[j][0]);
            found= true;
          }
    if (!found) r << t[i];  
  }
  return r;
}

/******************************************************************************
* Normalization of emails
******************************************************************************/

static void
extract_emails (tree& t, array<tree>& emails) {
  if (!is_concat (t)) return;
  for (int i=0; i<N(t); i++)
    if (is_tt (t[i])) {
      tree u= get_tt (t[i]);
      if (is_atomic (u) && occurs ("@", u->label)) {
        emails << compound ("author-email", u);
        t[i]= "";
        bool removed_space = false;
        bool removed_before= false;
        if (i>0) {
          if (is_atomic (t[i-1])) {
            string s= t[i-1]->label;
            while (ends (s, " ")) s= s (0, N(s)-1);
            if (ends (s, "email:")) s= s (0, N(s)-6);
            if (ends (s, "Email:")) s= s (0, N(s)-6);
            while (ends (s, " ")) { s= s (0, N(s)-1); removed_space= true; }
            while (ends (s, ",")) { s= s (0, N(s)-1); removed_before= true; }
            while (ends (s, " ")) { s= s (0, N(s)-1); removed_space= true; }
            t[i-1]= s;
          }
          else if (is_func (t[i-1], NEXT_LINE, 0)) {
            t[i-1]= "";
            removed_before= true;
          }
        }
        if (i+1<N(t) && !removed_before) {
          if (is_atomic (t[i+1])) {
            string s= t[i+1]->label;
            if (!removed_space)
              while (starts (s, " ")) s= s (1, N(s));
            while (starts (s, ",")) s= s (1, N(s));
            if (!removed_space)
              while (starts (s, " ")) s= s (1, N(s));
            t[i+1]= s;
          }
          else if (is_func (t[i+1], NEXT_LINE, 0)) t[i+1]= "";
        }
        //cout << "Removed " << t[i-1] << ", " << t[i] << ", " << t[i+1] << LF;
      }
    }
  t= simplify_concat (t);
}

static tree
expand_emails (tree t) {
  tree r (L(t));
  for (int i=0; i<N(t); i++)
    if (is_compound (t[i], "doc-author", 1) &&
        is_compound (t[i][0], "author-data")) {
      tree u= t[i][0];
      array<string> emails;
      tree v (L(u));
      for (int j=0; j<N(u); j++)
        if (is_compound (u[j], "author-email", 1) &&
            is_atomic (u[j][0])) {
          string s= u[j][0]->label;
          s= replace (s, ";", ",");
          s= replace (s, ", ", ",");
          s= replace (s, " ,", ",");
          while (true) {
            int start= search_forwards ("{", s);
            if (start<0) break;
            int end= start;
            while (end<N(s) && s[end] != '}') end++;
            int at= end;
            while (at<N(s) && s[at] != '@') at++;
            int next= at;
            while (next<N(s) && s[next] != ' ' && s[next] != ',') next++;
            array<string> a= tokenize (s (start+1, end), ",");
            string repl= s (0, start);
            for (int k=0; k<N(a); k++) {
              if (N(repl) != 0 && !ends (repl, ",")) repl << ",";
              repl << a[k] << s (at, next);
            }
            repl << s (next, N(s));
            s= repl;
          }
          while (ends (s, ",") || ends (s, ")") ||
                 ends (s, ".") || ends (s, " "))
            s= s (0, N(s)-1);
          array<string> b= tokenize (s, ",");
          if (N(b) <= 1) v << compound ("author-email", s);
          else emails << b;
        }
        else v << u[j];

      r << compound ("doc-author", v);
      for (int j=0; j<N(emails); j++)
        r << author_data (compound ("author-email", emails[j]));
    }
    else r << t[i];
  return r;
}

static tree
normalize_emails (tree t) {
  tree r= copy (t);
  for (int i=0; i<N(r); i++)
    if (is_compound (r[i], "doc-author", 1) &&
        is_compound (r[i][0], "author-data")) {
      tree u= r[i][0];
      array<tree> emails;
      for (int j=0; j<N(u); j++)
        if (is_compound (u[j], "author-affiliation", 1) ||
            is_compound (u[j], "author-note", 1) ||
            is_compound (u[j], "author-misc", 1)) {
          tree v= u[j][0];
          array<tree> extra;
          //cout << v << " -> ";
          extract_emails (v, extra);
          //cout << extra << LF;
          if (v == "" && N(extra) > 0) {
            r[i][0][j]= extra[0];
            emails << range (extra, 1, N(extra));
          }
          else emails << extra;
        }
      if (N(emails) > 0) r[i][0] << emails;
    }
  return expand_emails (r);
}

/******************************************************************************
* Attach pending affiliations and email addresses
******************************************************************************/

static void
fetch_names (tree t, array<string>& names) {
  if (is_atomic (t)) {
    array<string> l= tokenize (t->label, " ");
    for (int i=0; i<N(l); i++)
      if (uni_upcase_all (l[i]) != l[i] &&
          uni_locase_all (l[i]) != l[i] &&
          N(l[i]) > 3)
        names << l[i];
  }
  else if (is_concat (t)) {
    for (int i=0; i<N(t); i++)
      fetch_names (t[i], names);
  }
}

static void
fetch_emails (tree t, array<string>& emails) {
  if (is_atomic (t)) return;
  if (is_compound (t, "author-email", 1) && is_atomic (t[0]))
    emails << t[0]->label;
  //else if (is_tt (t)) emails << get_tt (t);
  else {
    for (int i=0; i<N(t); i++)
      fetch_emails (t[i], emails);
  }
}

static bool
match_email (string name, string email) {
  int pos= search_forwards ("@", email);
  if (pos < 0) return false;
  string email_name= email (0, pos);
  string n1= uni_unaccent_all (uni_locase_all (name));
  string n2= uni_unaccent_all (uni_locase_all (email_name));
  //string n1= uni_locase_all (name);
  //string n2= uni_locase_all (email_name);
  //cout << "Check " << n1 << ", " << n2 << LF;
  return occurs (n1, n2);
}

static bool
match_author_email (tree aut, string email) {
  array<string> names;
  fetch_names (get_author_name (aut), names);
  for (int i=0; i<N(names); i++)
    if (match_email (names[i], email)) return true;
  return false;
}

static void
suppress_email (tree& t, string email) {
  if (is_atomic (t)) return;
  for (int i=0; i<N(t); i++) {
    if (t[i] == compound ("author-email", email)) {
      t= t (0, i) * t (i+1, N(t));
      break;
    }
    else {
      suppress_email (t[i], email);
      if (t[i] == compound ("doc-author", compound ("author-data"))) {
        t= t (0, i) * t (i+1, N(t));
        break;
      }
    }
  }
}

static tree
clean_emails (tree t) {
  tree r (L(t));
  for (int i=0; i<N(t); i++)
    if (t[i] != compound ("doc-author", compound ("author-data")))
      r << t[i];
  return r;
}

static tree
attach_emails_in_order (tree t, bool strict) {
  tree r= copy (t);
  for (int i=0; i<N(t); )
    if (is_author_with_name (t[i])) {
      array<int> inds;
      int j, k;
      for (j= i; j < N(t); j++)
        if (is_author_with_name (t[j])) {
          if (is_author_without_email (t[j])) inds << j; }
        else if (is_author_without_name (t[j])) break;
      array<string> emails;
      for (k= j; k < N(t); k++)
        if (is_author_with_name (t[k])) break;
        else if (is_author_without_name (t[k]))
          fetch_emails (t[k][0], emails);
      if (N(inds) == N(emails) && N(inds) > 0) {
        bool ok= true;
        if (strict) {
          array<bool> done;
          for (int l=0; l<N(inds); l++)
            done << match_author_email (t[inds[l]], emails[l]);
          for (int l=0; l<N(inds); l++)
            for (int m=0; m<N(inds); m++)
              if (!done[l] && !done[m] && m != l)
                if (match_author_email (t[inds[l]], emails[m]))
                  ok= false;
        }
        if (ok) {
          if (N(inds) > N(emails))
            inds= range (inds, N(inds)-N(emails), N(inds));
          for (int l=0; l<N(emails); l++) {
            int m= inds[min (N(inds)-1, l)];
            r[m][0] << compound ("author-email", emails[l]);
            for (int n= j; n < k; n++)
              if (is_author_without_name (r[n]))
                suppress_email (r[n][0], emails[l]);
          }
        }
      }
      i= k;
    }
    else i++;
  return clean_emails (r);
}

static tree
attach_emails_by_name (tree t) {
  tree r= copy (t);
  for (int i=0; i<N(r); i++)
    if (is_author_with_name (r[i]) && is_author_without_email (r[i])) {
      tree name= get_author_name (r[i]);
      array<string> names;
      fetch_names (name, names);
      for (int j=i+1; j<N(r); j++)
        if (is_author_without_name (r[j])) {
          tree data= r[j][0];
          array<string> emails;
          fetch_emails (data, emails);
          bool found= false;
          for (int k=0; k<N(emails) && !found; k++)
            for (int l=0; l<N(names) && !found; l++)
              if (match_email (names[l], emails[k])) {
                suppress_email (r[j][0], emails[k]);
                r[i][0] << compound ("author-email", emails[k]);
                found= true;
              }
          if (found) break;
        }
    }
  return clean_emails (r);
}

static tree
attach_emails (tree t) {
  tree r= attach_emails_in_order (t, true);
  r= attach_emails_by_name (r);
  r= attach_emails_in_order (r, false);
  return r;
}

static tree
attach_pending (tree orig) {
  bool cluster= false;
  tree t= copy (orig);
  array<bool> done;
  for (int i=0; i<N(t); i++) done << false;

  // Detect author_1, ..., author_n, address_1, ..., address_n
  for (int i=0; i<N(t); )
    if (is_author_with_name (t[i])) {
      array<int> inds;
      int j, k, nr= 0;
      for (j= i; j < N(t); j++)
        if (is_author_with_name (t[j])) { inds << j; nr++; }
        else if (is_author_without_name (t[j])) break;
      hashmap<tree_label,int> h1 (0);
      for (k= j; k < N(t); k++)
        if (is_author_with_name (t[k])) break;
        else if (is_author_without_name (t[k]))
          for (int l=0; l<N(t[k][0]); l++)
            if (is_compound (t[k][0][l]) && N(t[k][0][l]) == 1)
              h1(L(t[k][0][l])) += 1;
      //cout << "inds= " << inds << LF;
      //cout << "h1  = " << h1   << LF;
      hashmap<tree_label,int> h2 (0);
      for (k= j; k < N(t); k++)
        if (is_author_with_name (t[k])) break;
        else if (is_author_without_name (t[k])) {
          int which= -1;
          bool ok= true;
          for (int l=0; l<N(t[k][0]); l++)
            if (is_compound (t[k][0][l]) && N(t[k][0][l]) == 1) {
              ok= ok && h1(L(t[k][0][l])) == nr;
              if (which == -1) which= h2[L(t[k][0][l])];
              ok= ok && (h2[L(t[k][0][l])] == which);
              h2(L(t[k][0][l])) += 1;
            }
          if (ok && which != -1) {
            int p= inds[which];
            t[p][0] << A(t[k][0]);
            done[k]= true;
          }
        }
      i= k;
    }
    else i++;
  
  // Detect author_1, ..., author_n, address
  for (int i=0; i<N(t); )
    if (is_author_with_name (t[i])) {
      array<int> inds;
      int j, k;
      for (j= i; j < N(t); j++)
        if (is_author_with_name (t[j])) inds << j;
        else if (is_author_without_name (t[j])) break;
      for (k= j; k < N(t); k++)
        if (is_author_with_name (t[k])) break;
        else if (!done[k] && is_author_without_name (t[k])) {
          cluster= cluster || N(inds) > 1;
          for (int l=0; l<N(inds); l++) {
            t[inds[l]][0] << A(t[k][0]);
            done[k]= true;
          }
        }
      i= k;
    }
    else i++;
  
  tree r (L(t));
  for (int i=0; i<N(t); i++)
    if (!is_author_without_name (t[i]))
      r << t[i];
  if (cluster) r= maximal_clustering (r);
  return r;
}

/******************************************************************************
* Final cleaning
******************************************************************************/

static tree
clean_line_breaks_sub (tree t) {
  tree r (DOCUMENT);
  if (is_document (t)) r= copy (t);
  else {
    if (!is_concat (t)) t= tree (CONCAT, t);
    tree c (CONCAT);
    for (int i=0; i<N(t); i++) {
      if (is_func (t[i], NEXT_LINE, 0)) {
        if (i>0 && is_atomic (t[i-1]) &&
            (ends (t[i-1]->label, ",") || ends (t[i-1]->label, ";")))
          c << " ";
        else {
          if (N(c) > 0) r << simplify_concat (c);
          c= tree (CONCAT);
        }
      }
      else c << t[i];
    }
    if (N(c) > 0) r << simplify_concat (c);
  }
  for (int i=0; i<N(r); i++) {
    if (!is_concat (r[i])) r[i]= tree (CONCAT, r[i]);
    for (int j=0; j<N(r[i]); j++) {
      if (j == 0 && is_atomic (r[i][j])) {
        string s= r[i][j]->label;
        while (starts (s, " ") || starts (s, ",") || starts (s, ";") ||
               starts (s, ")") || starts (s, "]") || starts (s, "}"))
          s= s (1, N(s));
        r[i][j]= s;
      }
      if (j == N(r[i])-1 && is_atomic (r[i][j])) {
        string s= r[i][j]->label;
        while (ends (s, " ") || ends (s, ",") || ends (s, ";") ||
               ends (s, "(") || ends (s, "{") || ends (s, "["))
          s= s (0, N(s)-1);
        r[i][j]= s;
      }
      if (is_atomic (r[i][j])) {
        string s= r[i][j]->label;
        s= replace (s, ",   ", ", ");
        s= replace (s, ";   ", "; ");
        s= replace (s, ",  ", ", ");
        s= replace (s, ";  ", "; ");
        s= replace (s, ",,", ",");
        s= replace (s, "  ,", ",");
        s= replace (s, " ,", ",");
        r[i][j]= s;
      }
    }
    r[i]= simplify_concat (r[i]);
  }
  tree u (DOCUMENT);
  for (int i=0; i<N(r); i++)
    if (r[i] != "")
      u << r[i];
  r= u;
  if (N(r) == 0) r= "";
  else if (N(r) == 1) r= r[0];
  return r;
}

static tree
clean_line_breaks (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "author-affiliation", 1))
    return compound ("author-affiliation", clean_line_breaks_sub (t[0]));
  else if (is_compound (t, "author-note", 1))
    return compound ("author-note", clean_line_breaks_sub (t[0]));
  else if (is_compound (t, "author-misc", 1))
    return compound ("author-misc", clean_line_breaks_sub (t[0]));
  else {
    tree r (L(t), N(t));
    for (int i=0; i<N(t); i++)
      r[i]= clean_line_breaks (t[i]);
    return r;
  }
}

/******************************************************************************
* Main routines
******************************************************************************/

tree
postprocess_metadata (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "doc-data")) {
    //cout << HRULE;
    //cout << "Clean doc data " << t << LF;
    tree t1= clean_doc_data (t);
    //cout << HRULE;
    //cout << "Postprocess doc data " << t1 << LF;
    tree t2= postprocess_doc_data (t1);
    //cout << HRULE;
    //cout << "Merge duplicates " << t2 << LF;
    tree t3= merge_duplicates (t2);
    //cout << HRULE;
    //cout << "Normalize emails " << t3 << LF;
    tree t4= normalize_emails (t3);
    //cout << HRULE;
    //cout << "Attach emails " << t4 << LF;
    tree t5= attach_emails (t4);
    //cout << HRULE;
    //cout << "Attach pending " << t5 << LF;
    tree t6= attach_pending (t5);
    //cout << HRULE;
    //cout << "Final cleaning " << t6 << LF;
    tree t7= clean_doc_data (t6);
    //cout << HRULE;
    //cout << "Final cleaning bis " << t7 << LF;
    tree t8= clean_line_breaks (t7);
    return t8;
  }
  else {
    tree r (L(t), N(t));
    for (int i=0; i<N(t); i++)
      r[i]= postprocess_metadata (t[i]);
    return r;
  }
}
