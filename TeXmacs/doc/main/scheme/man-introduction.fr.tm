<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduction>

  Tout comme <name|Emacs>, <apply|TeXmacs> est fourni avec un langage
  d'extension de style <name|Lisp>, le dialecte <with|font
  shape|small-caps|Guile Scheme> du projet <with|font
  shape|small-caps|Gnome>. Pour plus d'informations au sujet de <with|font
  shape|small-caps|Guile Scheme>, allez sur :

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <apply|scheme> a l'avantage qu'il peut être étendu avec des routines et des
  types externes C et C++. Dans notre cas, nous avons étendu <apply|scheme>
  avec des routines que vous pouvez utiliser pour créer vos propres menus et
  combinaisons de touches, et même pour créer vos propres extensions à
  <apply|TeXmacs>.

  Si vous avez téléchargé les fichiers sources de <apply|TeXmacs>, vous
  pouvez examiner les fichiers :

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<format|next line>
    \ \ Guile/Glue/build-glue-editor.scm<format|next line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  Ces trois fichiers <space|0.2spc>glue<space|0.2spc> contiennent les
  routines \ C++, visibles dans <apply|scheme>. Dans la suite, nous donnerons
  des détails sur les routines les plus importantes. Nous avons prévu
  d'écrire ultérieurement un guide de référence plus complet. Vous pouvez
  aussi étudier les fichiers \ <apply|scheme> <verbatim|.scm> situés dans le
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
