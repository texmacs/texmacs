<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Listes>

  Vous pouvez insérer un liste ordinaire avec <menu|Insert|Itemize>. Vous
  pouvez aussi choisir un marqueur particulier pour chaque élément de la
  liste : <math|\<bullet\>> (puces), <math|<op|->> (tirets) or
  <math|<op|\<rightarrow\>>> (flèches) ou utiliser le marqueur par défaut.
  Les listes peuvent être <em|imbriquées> comme ci-dessous :

  <\itemize>
    <item>Premier élément.

    <item>Sous-liste :

    <\itemize>
      <item>Sous-élément.

      <item>Autre sous-élément.
    </itemize>

    <item>Dernier élément.
  </itemize>

  Le marqueur par défaut change suivant le niveau d'imbrication. Au niveau le
  plus haut, on utilise <math|\<bullet\>>, au niveau secondaire
  <math|<op|\<circ\>>>, et ainsi de suite. Quand le curseur se trouve à
  l'intérieur d'une liste et que vous appuyez sur <key|retour chariot>, un
  nouvel élément est automatiquement ajouté à la liste. Si la longueur de
  certains éléments dépasse la longueur de la ligne, appuyez sur
  <key|S-retour chariot> pour aller à la ligne sans sortir de l'élément.

  Les énumérations, obtenues avec <menu|Insert|Enumerate>, ont des propriétés
  similaires aux listes ordinaires. Leurs éléments sont numérotés. Voici une
  énumération obtenue avec <menu|Insert|Enumerate|I, II, III> :

  <\enumerate-Roman>
    <item>Premier élément.

    <item>Second élément.

    <item>Dernier élément.
  </enumerate-Roman>

  Le dernier type de liste sert à faire une description. On la génère avec
  <menu|Insert|Description> et cela permet de définir une liste de choses :

  <\description>
    <item*|Gnou>Un animal poilu, mais gentil.

    <item*|Moucheron>On ne le trouve qu'au zoo (NdT : sic).
  </description>

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Michèle Garoche, Daouda
  Niang Diatta>

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