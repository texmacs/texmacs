<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listes>

  Vous pouvez insérer un liste ordinaire avec <apply|menu|Text|Itemize>. Vous
  pouvez aussi choisir un marqueur particulier pour chaque élément de la
  liste : <with|mode|math|\<bullet\>> (puces), <with|mode|math|<op|->>
  (tirets) or <with|mode|math|<op|\<rightarrow\>>> (flèches) ou utiliser le
  marqueur par défaut. Les listes peuvent être <em|imbriquées> comme
  ci-dessous :

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
  plus haut, on utilise <with|mode|math|\<bullet\>>, au niveau secondaire
  <with|mode|math|<op|\<circ\>>>, et ainsi de suite. Quand le curseur se
  trouve à l'intérieur d'une liste et que vous appuyez sur <key|retour
  chariot>, un nouvel élément est automatiquement ajouté à la liste. Si la
  longueur de certains éléments dépasse la longueur de la ligne, appuyez sur
  <key|S-retour chariot> pour aller à la ligne sans sortir de l'élément.

  Les énumérations, obtenues avec <apply|menu|Text|Enumerate>, ont des
  propriétés similaires aux listes ordinaires. Leurs éléments sont numérotés.
  Voici une énumération obtenue avec <apply|menu|Text|Enumerate|I, II, III> :

  <\expand|enumerate-Roman>
    <item>Premier élément.

    <item>Second élément.

    <item>Dernier élément.
  </expand>

  Le dernier type de liste sert à faire une description. On la génère avec
  <apply|menu|Text|Description> et cela permet de définir une liste de choses
  :

  <\description>
    <expand|item*|Gnou.>Un animal poilu, mais gentil.

    <expand|item*|Moucheron.>On ne le trouve qu'au zoo (NdT : sic).
  </description>

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Liste>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Énumération>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Énumération>|<with|font family|<quote|ss>|I, II,
      III>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Description>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
