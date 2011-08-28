<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Plans pour l'avenir>

  <with|color|red|[Doit être mis à jour]>

  <section|Mise en page>

  Certains outils de mise en page n'ont pas encore été implémentés. Ce sont
  les suivants :

  <\itemize>
    <item>Séparation d'un document en plusieurs parties.

    <item>Objets dynamiques, comme dans HTML.

    <item>Environnements pour les interfaces de logiciels de calcul formel.
  </itemize>

  Certains outils moins importants doivent être complétés. En voici une liste
  non exhaustive :

  <\itemize>
    <item>Considérer les fractions comme des opérateurs
    <with|mode|math|\<Rightarrow\>> espaces avant et après. Idem pour les
    indices et exposants <with|mode|math|\<Rightarrow\>> petite espace avant
    quand ils sont placés à gauche et petite espace après lorsqu'ils sont
    placés à droite.
  </itemize>

  <section|Outils d'édition de texte>

  Bien que les mouvements du curseur, les sélections, etc... soient déjà
  gérés, il reste à parfaire certains outils d'édition standards. Par exemple
  :

  <\itemize>
    <item>Recherche dans du texte, des formules, dans certains environnement,
    etc...

    <item>Demande de remplacement.

    <item>Outils mathématiques : simplification d'une région sélectionnée,
    substitutions de formules à l'intérieur d'autres formules, etc...

    <item>Contrôle de version.

    <item>Compression et protection de données.

    <item>Correcteurs grammaticaux et programmes de traduction automatiques.
    Si vous savez où trouvez des dictionnaires exhaustifs libres de droits ou
    du matériel se rapportant à la traduction, n'hésitez pas à nous en faire
    part.

    <item>Intégration d'un programme libre de reconnaissance vocale.
  </itemize>

  <section|Tableur universel>

  Nous aimerions incorporer un <space|0.2spc>tableur
  universel<space|0.2spc> dans <TeXmacs>. L'idée est que toutes les
  dépendances entre les cellules de la feuille de calcul soient analysées par
  <TeXmacs>, mais que les calculs réels soient délégués au système externe
  que vous aurez choisi, par exemple l'un de ceux qui sont gérés par les
  logiciels de calcul formel. Les données de la feuille de calcul ne seraient
  pas forcément formatées dans un tableau rectangulaire ; on peut imaginer
  des dépendances entre les noeuds d'un arbre, les éléments d'un graphe ou
  n'importe quoi d'autre.

  <section|Graphiques techniques>

  J'aimerais également incorporer un outil de dessin de graphiques
  techniques. On pourrait alors utiliser des macros définies par
  l'utilisateur pour faire des constructions géométriques. Il serait
  possible, par exemple, d'écrire un fichier de style pour dessiner des
  circuits électroniques ou des composants chimiques avec une barre d'icône
  pour accéder aux circuits ou composants voulus, de la même façon qu'on peut
  le faire pour sélectionner des lignes et des cercles dans les dessins
  ordinaires.

  <section|Interface avec les logiciels de calcul formel>

  Les changements suivants doivent être faits pour pouvoir relier
  <apply|TeXmacs> aux logiciels de calcul formel :

  <\enumerate>
    <item>Amélioration de la présentation des sessions de calcul formel.

    <item>Ajout de fonctionnalités nouvelles pour augmenter
    l'interopérabilité entre <apply|TeXmacs> et les logiciels de calcul
    formel et pour permettre un contrôle plus grand de la présentation de
    sorties longues.

    <item>Sémantique étendue au niveau de la communication entre objets. Ce
    pourrait être soit des informations de haut niveau (comme les balises
    mathématiques Openmath ou HTML 4.0) ou des informations de bas niveau (y
    compris celles concernant la représentation des données), selon la
    vitesse d'exécution requise.

    <item>Autres possibilités d'évolution : mise en surbrillance, débogage,
    etc...
  </enumerate>

  <section|Interaction avec d'autres projets de type GNU>

  Il serait bon d'augmenter l'interaction entre <apply|TeXmacs> et d'autres
  projets de type GNU, tels Gnome ou des GUIs multi-plateformes. Cela
  faciliterait l'intégration de données externes dans les documents
  <apply|TeXmacs> et augmenterait le nombre de plateformes supportées. D'un
  autre côté, certaines fonctionnalités propres à <apply|TeXmacs>, telle la
  gestion des polices, peuvent intéresser d'autres projets.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Typesetting><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Extra facilities for editing
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>A universal
      spreadsheet><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Technical
      pictures><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Interface with computer algebra
      systems><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Interaction with other GNU-like
      projects><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
