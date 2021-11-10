<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personnalisation de <TeXmacs>>

  Une des caractéristiques les plus importantes de <TeXmacs> est qu'il est
  fortement modulable. Tout d'abord, on peut
  <apply|hyper-link|configurer|../config/man-configuration.fr.tm> les aspects
  les importants du programme avec <apply|menu|Edit|Preferences>. Presque
  toutes les autres parties de <TeXmacs> peuvent être adaptées ou
  reprogrammées à l'aide du langage d'extension <name|Guile>/<name|Scheme>.
  Vous trouverez plus loin un bref aperçu de ce mécanisme dans les cas les
  plus simples.

  <\traverse>
    <apply|branch|Introduction au langage d'extension
    <name|Guile>|man-guile-intro.fr.tm>

    <apply|branch|Écriture de fichiers d'initialisation|man-initialization.fr.tm>

    <apply|branch|Création de menus dynamiques|man-menus.fr.tm>

    <apply|branch|Création de raccourcis clavier|man-custom-keyboard.fr.tm>

    <apply|branch|Autres fichiers à voir|man-files.fr.tm>
  </traverse>

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Préférences>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
