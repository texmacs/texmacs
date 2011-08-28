<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduction au langage d'extension <name|Guile>>

  Tout comme <name|Emacs>, <apply|TeXmacs> est livré avec un langage
  d'extension similaire à <name|Lisp> : le dialecte <with|font
  shape|small-caps|Guile Scheme> du projet <with|font
  shape|small-caps|Gnome>. La documentation <with|font shape|small-caps|Guile
  Scheme> est accessible sur :\ 

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <apply|scheme> a l'avantage qu'il peut être étendu avec des types et des
  routines externes C et C++. Dans notre cas, nous avons étendu
  <apply|scheme> avec des routines que vous pouvez utiliser pour créer vos
  propres menus ou raccourcis claviers, et même vos propres extensions
  <apply|TeXmacs>.

  Si vous avez téléchargé les fichiers sources de <apply|TeXmacs>, examinez
  les fichiers suivants :

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<format|next line>
    \ \ Guile/Glue/build-glue-editor.scm<format|next line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  Ce sont trois fichiers <space|0.2spc>glue<space|0.2spc> qui contiennent
  les routines C++ routines, visibles dans <apply|scheme>. Dans la suite,
  nous aborderons les routines les plus importantes. Un guide de référence
  plus complet sera écrit plus tard. Vous pouvez aussi examiner les fichiers
  <with|font shape|small-caps|Scheme> <verbatim|.scm> situés dans le
  répertoire <verbatim|$TEXMACS_PATH/progs>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

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
    <associate|language|french>
  </collection>
</initial>
