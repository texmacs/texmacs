<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Parcourir une présentation>

  Une grande partie des marqueurs pour les présentations concerne le parcours
  du document durant la présentation. Les touches <key|F10> et <key|F11> sont
  utilisées pour se déplacer dans le document en arrière <abbr|resp.> en
  avant. Les touches <key|F9> et <key|F12> sont utilisées pour aller au début
  <abbr|resp.> à la fin de la présentation. Quand on utilise le style
  <tmstyle|beamer> ou quand on a activé ``presentation tool'' dans le menu
  <menu|Tools>, un menu <menu|Dynamic> et des icônes supplémentaires
  apparaissent; Ils peuvent aussi être utilisés pour la navigation dans votre
  présentation.

  Le marqueur de parcours le plus basique est nommé \S \ <markup|switch> \T,
  il permet de montrer successivement les différentes parties du document. La
  présentation elle-même est habituellement un \ <markup|commutateur
  screens>, où les parties sont les vues successives. Après la selection du
  style <tmstyle|beamer>, ce commutateur peut être inséré à l'aide de
  <menu|Focus|Screens> ou <menu|Insert|Fold|Switch|Screens>. La vue nommée \S
  slide 1 \T est alors créée. Après avoir créer d'autres vues (voir comment
  plus bas<space|0.2spc>), vous pourrez sauter de vue en vue en utilisant
  \ <key|pageup> et <key|pagedown>

  A l'intérieur d'un <markup|switch>, de nouveaux \ \S branchements \T
  peuvent être insérés avant ou après la vue courante en utilisant
  <menu|Focus|Insert argument after> ou <menu|Focus|Insert argument before>.
  En plus du <markup|commutateur screens>, vous pouvez utiliser
  <menu|Insert|Fold|Switch|Standard> pour insérer des commutations sur des
  paragraphes entiers ou sur de simples lignes (de façon anlogue à
  l'affichage de formules en ligne).

  Une \ manière répandue de parcourir les présentations, est le déroulement
  progressif du contenu. Cela peut être fait par l'insertion de la balise
  <markup|unroll> à l'aide de <menu|Insert|Fold|Unroll>. Grâce à une astuce,
  cette balise peut être combinée avec les balises <markup|itemize> et
  <markup|enumerate> de la façon suivante :

  <\enumerate-numeric>
    <item>Créez la liste à l'aide de par exemple <menu|Insert|List|dash>

    <item>Enlevez le premier <markup|item> (le tiret dans cet exemple) à
    l'aide de <key|backspace>

    <item>Insérez la balise <markup|unroll> à l'aide de
    <menu|Insert|Fold|Unroll>

    <item>Pressez <key|enter> pour créer le premier item de la liste

    <item>Utilisez <menu|Focus|Insert argument after> suivi de <key|enter>
    pour ajouter un nouvel <markup|item>
  </enumerate-numeric>

  Notez que vous pouvez dérouler plusieurs \ <markup|items> à la fois en les
  ajoutant simplement à la suite (sans utiliser <menu|Focus|Insert argument
  after>).

  Une variante du déroulement est le développement d'un contenu. Il s'agit
  basiquement d'une balise à deux branchements, différentes variantes sont
  disponibles dans <menu|Insert|Fold|Folded> selon l'effet désiré. Quelques
  unes des variantes affichent un bouton dans la barre d'information
  <markup|Focus> permettant de développer ou plier le contenu. \ Les champs
  d'entrée-sortie d'une session de calcul sont aussi utilisables.
  Similairement, la balise <menu|Insert|Fold|Summarize> est un
  \ <markup|commutateur> à deux branchements avec là aussi différents types
  d'effets disponibles.

  Lors de l'utilisation de <TeXmacs> en combinaison avec un plug-in externe,
  tel qu'un système de calcul algébrique sur ordinateur, vous pouvez
  remarquer que tous les champs d'entrée-sortie des
  <hlink|sessions|../interface/man-session-basic.en.tm> sont pliables. En
  plus, vous pouvez créer un \S <hlink|executable
  switches|../interface/man-scripting-language.en.tm> \T en utilisant le
  sous-menu <menu|Insert|Fold|Executable>. Cela vous permet de commuter entre
  une entrée donnée au système de calcul et sa sortie correspondante.

  Tous les marqueurs utilisés pour le parcours des présentations, peuvent
  être imbriqués de manière naturelle. Dans le menu
  <menu|Insert|Fold|Traversal>, vous pouvez spécifier si le déroulement et le
  zones pliables qui doivent être repliées après la parcours.\ 

  <tmdoc-copyright|2010|Joris van der Hoeven|Denis Raux>

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