
/******************************************************************************
* MODULE     : metadata-acm.cpp
* DESCRIPTION: conversion of (ACM) tex metadata into texmacs metadata
* COPYRIGHT  : (C) 2012 Joris van der Hoeven, Poulain François
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"

tree
collect_metadata_acm (tree t) {
  int i, n=N(t);
  tree r (CONCAT);
  for (i=0; i<n; i++) {
    tree u= t[i];
    if (is_tuple (u, "\\title", 1) || is_tuple (u, "\\subtitle", 1)) {
      tree v (CONCAT), w= u[1], titlenote;
      for (int j=0; j<N(w); j++) {
        if (is_tuple (w[j], "\\titlenote", 1)     ||
            is_tuple (w[j], "\\thanks", 1)) {
          titlenote= copy (w[j]);
          if (u[0] == "\\title")
            titlenote[0]= "\\title-thanks";
          else if (u[0] == "\\subtitle")
            titlenote[0]= "\\doc-subtitle-note";
        }
        else
          v << w[j];
      }
      if (u[0] == "\\title")
        r << tuple ("\\title", v);
      else if (u[0] == "\\subtitle")
        r << tuple ("\\subtitle", v);
      if (is_tuple (titlenote, "\\title-thanks") ||
          is_tuple (titlenote, "\\doc-subtitle-note"))
        r << titlenote;
    }
    else if (is_tuple (u, "\\author", 1)) {
      tree v (CONCAT), w= u[1], a= tuple ("\\author");
      array<tree> l;
      for (int j=0; j<N(w); j++) {
        if (is_tuple (w[j], "\\titlenote", 1) ||
            is_tuple (w[j], "\\thanks", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\title-thanks";
          l << x;
        }
        else if (is_tuple (w[j], "\\affaddr", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\address";
          l << x;
        }
        else if (is_tuple (w[j], "\\email", 1)) {
          tree x= copy (w[j]);
          x[0] = "\\title-email";
          l << x;
        }
        else if (is_tuple (w[j], "\\alignauthor") ||
                 is_tuple (w[j], "\\and")) {
          if (N(v) > 0)
            a << v;
          v= concat ();
          if (is_tuple (a, "\\author", 1))
            r << a;
          a= a= tuple ("\\author");
          for (int j=0; j<N(l); j++)
            r << l[j];
          l= array<tree> ();
        }
        else if (is_tuple (w[j], "\\\\"));
        else
          v << w[j];
      }
      if (N(v) > 0)
        a << v;
      if (is_tuple (a, "\\author", 1))
        r << a;
      for (int j=0; j<N(l); j++)
        r << l[j];
    }
    else if (is_tuple (u, "\\footnotetext", 1) ||
             is_tuple (u, "\\footnotetext*", 2)) {
      tree v= tuple (u[0], u[N(u)-1]);
      v[0] = "\\title-thanks";
      r << v;
    }
    else if (is_tuple (u, "\\keywords"))
      r << u;
    else if (is_tuple (u, "\\category") || is_tuple (u, "\\category*")) {
      tree v= copy (u);
      v[0]= "\\doc-acm";
      r << v;
    }
    else if (is_tuple (u, "\\conferenceinfo")) {
      tree v= copy (u);
      v[0]= "\\doc-conference";
      r << v;
    }
    else if (is_tuple (u, "\\terms")) {
      tree v= copy (u);
      v[0]= "\\doc-terms";
      r << v;
    }
  }
  return r;
}


