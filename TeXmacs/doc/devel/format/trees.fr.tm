<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <\expand|tmdoc-title>
    Les documents sont des arbres

    \;
  </expand>

  <apply|TeXmacs> représente tous les textes sous forme d'arbres (pour un
  texte figé, l'arbre correspondant est appelé <expand|def-index|arbre
  d'édition>). Les noeuds internes de l'arbre sont étiquetés par des
  <expand|def-index|opérateurs> de type <verbatim|tree_label> (voir
  <verbatim|Basic/Data/tree.gen.h>). Les étiquettes des feuilles de l'arbre
  sont des chaînes de caractères, soit invisibles (telles les mesures ou les
  définitions de macros), soit visible (le texte lui-même). Les arbres
  <TeXmacs> peuvent être décrits à l'aide de notations diverses. Par exemple,
  l'arbre :

  <\expand|quote>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </expand>

  représente la formule :

  <\expand|tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </expand>

  et peut aussi être décrit de la façon suivante :

  <\expand|scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </expand>

  en notation <value|scheme>.

  La signification du texte et la façon dont il est typographié dépendent
  essentiellement de son environnement. L'environnement consiste en une table
  d'informations qui fait correspondre les variables d'environnement à leurs
  valeurs dans l'arbre. La langue, la police et la couleur actives sont des
  exemples de variables d'environnement système ; de nouvelles variables
  peuvent être définies par l'utilisateur. Par exemple, l'expression
  <value|scheme> suivante :

  <\expand|scheme-fragment>
    (concat

    \ \ "Some "

    \ \ (with "color" "blue" "blue")

    \ \ " text.")
  </expand>

  représente le fragment de document :

  <\expand|tm-fragment>
    Some <with|color|blue|blue> text
  </expand>

  La primitive <TeXmacs> <verbatim|with> signale un changement local de
  variable d'environnement.

  Dans la suite, nous décrirons plus en détail les opérateurs standards
  <apply|TeXmacs> et les variables d'environnement. Il faut noter que le
  format de données <apply|TeXmacs> est sujet à changement. Nous décrivons
  ces changements dans la dernière section. En général, l'utilisateur ne
  prend pas conscience de ces changements, car ils sont effectués par des
  programmes de conversion qui mettent automatiquement à jour les opérateurs.
  Néanmoins, ils ont parfois de l'importance pour les développeurs, bien que
  la plupart de ces changements concernent l'ajout de nouvelles primitives.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|arbre d'édition>|<pageref|idx-1>>

      <tuple|<tuple|opérateurs>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
