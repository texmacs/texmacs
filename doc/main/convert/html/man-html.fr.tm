<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion de documents <TeXmacs> en HTML>

  Nous avons commencé à implémenter la conversion de HTML à <apply|TeXmacs>
  et vice-versa. À l'heure actuelle, on ne peut qu'importer des documents
  HTML avec <apply|menu|Fichier|Importer|HTML>. La presque totalité de HTML
  2.0 et certains aspects de HTML 3.0 sont gérés. Néanmoins, il n'est pas
  possible, pour l'instant, de naviguer. Nous prévoyons d'implémenter Math-ML
  plus tard.

  Lors de l'importation de documents HTML, les fichiers dont le nom commence
  par \ <verbatim|http:> or <verbatim|ftp:> sont téléchargés à l'aide du
  programme <verbatim|wget>. Si vous compilez <apply|TeXmacs> vous-même, vous
  pouvez télécharger <verbatim|wget> à l'adresse suivante :\ 

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  Nous avons inclus <verbatim|wget> dans la distribution binaire.

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
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>|<with|font
      family|<quote|ss>|Importer>|<with|font
      family|<quote|ss>|HTML>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
