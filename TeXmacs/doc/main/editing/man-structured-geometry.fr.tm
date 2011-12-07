<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <with|language|french|><tmdoc-title|Positionnement et redimensionnement
  d'objects>

  Le préfixe <prefix|structured:geometry> peut être utilisé pour
  repositionner et redimensionner les objects. Par exemple, à l'intérieur
  d'une cellule d'un tableau, vous pouvez utiliser <key|structured:geometry
  right> pour déplacer les cellules plus vers la droite. Sur l'espace créé
  <em|via> <menu|Format|Space>, la même touche permet d'accroître sa largeur.
  Plus généralement, les raccourcis suivants sont définis :

  <\description>
    <item*|<shortcut|(geometry-left)>>Diminue la taille horizontale d'un
    objet, ou le déplace vers la gauche.

    <item*|<shortcut|(geometry-right)>>Augmente la taille horizontale d'un
    objet, ou le déplace vers la droite.

    <item*|<shortcut|(geometry-down)>>Diminue/augmente la taille verticale
    d'un objet, ou le déplace vers le bas.

    <item*|<shortcut|(geometry-up)>>Augmente/diminue la taille verticale d'un
    object, ou le déplace vers le haut.

    <item*|<shortcut|(geometry-start)>>Diminue le décalage horizontal d'un
    objet, ou l'aligne gauche.

    <item*|<shortcut|(geometry-end)>>Augmente le décalage horizontal d'un
    objet, ou l'aligne droite.

    <item*|<shortcut|(geometry-bottom)>>Diminue le décalage vertical d'un
    objet, ou l'aligne en bas.

    <item*|<shortcut|(geometry-top)>>Augmente le décalage vertical d'un
    objet, ou l'aligne en haut.

    <item*|<shortcut|(geometry-reset)>>Restaure la géomètrie (taille,
    position, alignement) aux valeurs par défaut.

    <item*|<shortcut|(geometry-circulate #t)>, <shortcut|(geometry-circulate
    #f)>>Boucle à travers toutes les unités possibles pour la géomètrie.

    <item*|<shortcut|(geometry-slower)>, <shortcut|(geometry-faster)>>Décroît
    ou augmente le pas de variation pour le redimensionnement ou le
    recadrage.
  </description>

  Tags spécifiques auxquels ces raccourcis s'appliquent :

  <\description>
    <item*|Espaces>A la fois horizontaux et verticaux à l'aide du menu
    <menu|Format|Space>. Vous devez mettre le curseur juste après l'espace
    pour appliquer le raccourci.

    <item*|Boîtes modificateurs>Les tags <markup|move>, <markup|shift>,
    <markup|resize> et <markup|clipped><compound|markup|> du menu
    <menu|Format|Transform>.

    <item*|Animations>Les durées des animations peuvent être modifiées avec
    <shortcut|(geometry-left)> and <shortcut|(geometry-right)>.

    <item*|Images>La taille et l'alignement des images peuvent être modifiés.
  </description>

  <tmdoc-copyright|1998--2010|Joris van der Hoeven|Denis Raux>

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
    <associate|preamble|false>
  </collection>
</initial>