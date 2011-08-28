<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Génération d'un index>

  Pour générer un index, vous devez d'abord insérer les entrées d'index dans
  votre document avec <apply|menu|Insert|Link|Index entry>. Ensuite,
  positionnez votre curseur à l'endroit où vous voulez que votre index soit
  généré et cliquez sur <apply|menu|Insert|Automatic|Index>. Les index sont
  générés de la même façon que les tables de matières.

  Vous trouverez dans le menu <apply|menu|Insert|Link|Index entry>
  différentes sortes d'entrées d'index. Les plus simples sont
  <space|0.2spc>principal<space|0.2spc>, <space|0.2spc>sous<space|0.2spc>
  et <space|0.2spc>sous-sous<space|0.2spc>, qui sont des macros avec,
  respectivement, un, deux ou trois arguments. Les entrées de la forme
  <space|0.2spc>sous<space|0.2spc> ou <space|0.2spc>sous-sous<space|0.2spc>peuvent
  être utilisées pour subordonner certaines entrées d'index à d'autres.

  Une entrée d'index complexe comporte quatre arguments. La première est une
  clé de tri sur l'entrée et doit être un
  <space|0.2spc>tuplet<space|0.2spc> (créé avec <key|inactive \<less\>>),
  dont le premier élément est la clé principale, le second la clé secondaire,
  etc... Le second argument d'une entrée d'index complexe est soit vide soit
  <space|0.2spc>strong<space|0.2spc>, auquel cas le numéro de la page
  s'affichera en gras. Le troisième argument est en général vide, mais si
  vous créez deux entrées d'index dont le troisième argument est non vide et
  identique, cela générera un <space|0.2spc>champ<space|0.2spc> de numéros
  de page. Le quatrième argument correspond à l'entrée d'index exprimée sous
  forme de tuplet.

  On peut aussi créer une entrée d'index sans référence à un numéro de page
  en utilisant l'article <space|0.2spc>Interjection<space|0.2spc> du menu
  <apply|menu|Insert|Link|Index entry>. Le premier argument de cette macro
  est une clé de tri sur l'entrée. Le second argument contient le texte de
  l'entrée. Ceci permet de créer des sections
  <space|0.2spc>A<space|0.2spc>, <space|0.2spc>B<space|0.2spc>,
  <space|0.2spc>C<space|0.2spc>, etc... dans l'index.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entrée dans
      l'index>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Automatique>|<with|font
      family|<quote|ss>|Index>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entrée dans
      l'index>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entrée dans
      l'index>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
