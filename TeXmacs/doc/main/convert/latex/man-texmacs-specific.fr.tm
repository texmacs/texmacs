<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Spécificités de <apply|TeXmacs>>

  Certaines primitives typographiques de <apply|TeXmacs> n'ont pas
  d'équivalent dans <apply|LaTeX> ; l'algorithme de conversion les
  transformera tout simplement en espace. Les principales spécificités de
  <apply|TeXmacs> sont les suivantes :

  <\itemize>
    <item>Primes à gauche.

    <item>Grands délimiteurs entre grandes parenthèses.

    <item>Mosaïques.

    <item>Arbres.

    <item>Macros personnalisées complexes.

    <item>Espaces verticales <space|0.2spc>avant<space|0.2spc> et
    <space|0.2spc>après<space|0.2spc>.

    <item>Drapeaux d'indentation <space|0.2spc>avant<space|0.2spc> et
    <space|0.2spc>après<space|0.2spc>.
  </itemize>

  Vous devez éviter d'utiliser ces propriétés, qui sont spécifiques à
  <apply|TeXmacs>, si vous convertissez votre document en document
  <apply|LaTeX>. Néanmoins, à l'avenir, il se peut que le programme de
  conversion puisse générer un fichier encapsulé PostScript à défaut d'une
  conversion satisfaisante.

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
    <associate|toc-1|<tuple|<uninit>|?>>
  </collection>
</references>
