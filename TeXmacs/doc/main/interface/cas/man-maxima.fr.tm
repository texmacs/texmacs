<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Maxima>

  Maxima est non seulement l'un des plus anciens et des meilleurs logiciels
  de calcul formel, mais il est aussi le seul qui soit gratuit. Vous pouvez
  le télécharger à partir de :

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  La version gérée par TeXmacs est la version basée sur GCL <with|font
  shape|small-caps|Maxima> 5.6. Pour la version basée sur clisp <with|font
  shape|small-caps|Maxima> 5.6, éditez le fichier <verbatim|tm_maxima> et
  remplacez <verbatim|-load> par <verbatim|-i>. Pour <with|font
  shape|small-caps|Maxima> 5.9-pre, remplacez <verbatim|-load> par
  <verbatim|-p>. Problèmes non résolus :

  <\itemize>
    <item>Si vous appuyez sur <key|retour chariot><verbatim|> alors qu'un
    ordre n'est pas complet (non terminé par <verbatim|;> ou <verbatim|$>),
    l'interface se bloque.

    <item>Si vous faites apparaître l'invite d'interruption de Lisp,
    l'interface se bloque.

    <item>La commande <verbatim|info> n'est pas gérée (elle est définie dans
    Lisp et est difficilement portable).

    <item>Certaines commandes du débogueur fonctionnent, d'autres (y compris
    <verbatim|:c>) ne fonctionnent pas ; personne ne sait pourquoi.

    <item>La commande <verbatim|load> peut parfois avoir un comportement
    erratique.
  </itemize>

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
