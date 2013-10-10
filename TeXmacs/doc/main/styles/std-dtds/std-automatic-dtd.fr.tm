<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Génération automatique de contenu>

  Le d.t.d. <tmdtd|std-automatic> définit la génération automatique de
  contenu, telles les tables de matières et les bibliographies, et leur
  affichage. Les balises suivantes sont utilisées pour les bibliographies\ 

  <\explain|<markup|cite>>
    Fonction avec un nombre arbitraire d'arguments. Chaque argument est une
    citation correspondant à un article dans un fichier BiB-<TeX> file. Les
    citations sont affichées telles que référencées dans la bibliographie et
    servent d'hyperliens aux références. Un point d'interrogation remplace
    les citations quand la bibliographie n'est pas générée.
  </explain>

  <\explain|<markup|nocite*>>
    Identique à <markup|cite>, mais les citations ne sont pas affichées dans
    le texte principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Fonction qui indique comment afficher un article de bibliographie.
  </explain>

  Les balises suivantes sont utilisées pour compiler des tables de matières :

  <\explain|<markup|toc-main-1>>
    Fonction à un argument pour créer une entrée principale dans la table des
    matières. Cette fonction peut être utilisée, par exemple, pour les
    différentes parties d'un livre.
  </explain>

  <\explain|<markup|toc-main-2>>
    Fonction à un argument pour créer une entrée principale dans la table des
    matières. Cette fonction est utilisée pour les chapitres.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Fonction à un argument pour créer une entrée ordinaire dans la table des
    matières. Cette fonction est souvent utilisée pour les sections.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Identique à <markup|toc-normal-2> pour des entrées moins importantes,
    telles les sous-sections.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Identique à <markup|toc-normal-3> pour des entrées encore moins
    importantes, telles les sous-sous-sections.
  </explain>

  <\explain|<markup|toc-small-1>>
    Utilisée pour des entrées de peu d'importance, tels les paragraphes (peut
    être ignorée).
  </explain>

  <\explain|<markup|toc-small-2>>
    Utilisée pour des entrées d'encore moins d'importance, tels les
    sous-paragraphes.
  </explain>

  <\explain|<markup|toc-dots>>
    Séparation entre une entrée dans la table des matières et le numéro de
    page correspondant. Par défaut, on utilise une suite de points
    horizontaux.
  </explain>

  Les balises suivantes sont utilisées pour les indices :

  <\explain|<markup|index>>
    Fonction à un argument <var|x>, qui l'insère dans l'index en tant
    qu'entrée principale.
  </explain>

  <\explain|<markup|subindex>>
    Fonction à deux arguments <var|x> et <var|y>, qui insère <var|y> dans
    l'index en tant que sous-entrée de <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Fonction à trois arguments <var|x>, <var|y> et <var|z>, qui insère
    <var|z> dans l'index en tant que sous-entrée de <var|y>, lui-même
    sous-entrée de <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Fonction à quatre arguments <var|key>, <var|how>, <var|range>,
    <var|entry>, expliquée dans le section <hlink|génération des
    index|../../links/man-index.fr.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Cette fonction a deux arguments. Le premier <var|key>, clé de tri,
    indique comment trier le second <var|entry>, l'entrée. Aucun numéro de
    page n'est généré.
  </explain>

  <\explain|<markup|index-1>>
    Macro avec une entrée d'index et un numéro de page. Utilisée pour
    l'affichage d'une entrée principale d'index.
  </explain>

  <\explain|<markup|index-1*>>
    Identique à <markup|index-1>, mais sans numéro de page.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (avec <math|n> compris entre 1 et 5) : macro avec une entrée d'index et
    un numéro de page, utilisée pour l'affichage d'une entrée de niveau
    <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Identique à <markup|index-<math|n>>, mais sans numéro de page.
  </explain>

  <\explain|<markup|index-dots>>
    Macro qui génère les points entre une entrée d'index et le(s) numéro(s)
    de page(s) correspondant(s).
  </explain>

  Les balises suivantes sont utilisées pour les glossaires :

  <\explain|<markup|glossary>>
    Fonction qui insère son unique argument dans un glossaire.
  </explain>

  <\explain|<markup|glossary-dup>>
    Crée un numéro de page supplémentaire pour une entrée déjà insérée.
  </explain>

  <\explain|<markup|glossary-explain>>
    Fonction pour insérer une entrée de glossaire accompagnée de son
    explication.
  </explain>

  <\explain|<markup|glossary-line>>
    Insère une entrée de glossaire sans numéro de page.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro pour afficher une entrée de glossaire et le numéro de page
    correspondant.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro pour afficher une entrée de glossaire, son explication et le numéro
    de page correspondant.
  </explain>

  <\explain|<markup|glossary-dots>>
    Macro qui génère les points entre une entrée de glossaire et le(s)
    numéro(s) de page(s) correspondant(s).
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>