<TeXmacs|1.0.0.13>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduction to the <name|Guile> extension language>

  Like <name|Emacs>, <apply|TeXmacs> comes with a <name|Lisp>-like extension
  language, namely the <with|font shape|small-caps|Guile Scheme> dialect from
  the <with|font shape|small-caps|Gnome> project. For documentation about
  <with|font shape|small-caps|Guile Scheme>, we refer to\ 

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <apply|scheme> has the advantage that it may be extended with extern C and
  C++ types and routines. In our case, we have extended <apply|scheme> with
  routines which you can use to create your own menus and key-combinations,
  and even to write your own extensions to <apply|TeXmacs>.

  If you have downloaded the source files of <apply|TeXmacs>, then it may be
  interesting for you to take a look at the files\ 

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<format|next line>
    \ \ Guile/Glue/build-glue-editor.scm<format|next line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  These three ``glue'' files contain the C++ routines, which are visible
  within <apply|scheme>. In what follows, we will discuss some of the most
  important routines. We plan to write a more complete reference guide later.
  You may also take a look at the scheme <verbatim|.scm> files in the
  directory <verbatim|$TEXMACS_PATH/progs>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
