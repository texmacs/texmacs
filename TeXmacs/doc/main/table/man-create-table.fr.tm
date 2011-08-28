<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Création de tableaux>

  Pour créer un tableau, vous pouvez utilisez, soit <apply|menu|Insert|Table>
  ou l'un des raccourcis clavier suivants :\ 

  <\description>
    <expand|item*|<key|table N t>>Crée un tableau ordinaire (sans
    bordure, ni encadrement).

    <expand|item*|<key|table N T>>Crée un tableau ordinaire dont les
    cellules sont centrées.

    <expand|item*|<key|table N b>>Crée un
    <space|0.2spc>bloc<space|0.2spc> ordinaire (tableau avec bordures et
    encadrement)

    <expand|item*|<key|table N B>>Crée un bloc dont les cellules sont
    centrées.
  </description>

  En mode math, vous pouvez aussi créer les structures tabulaires suivantes :

  <\description>
    <expand|item*|<key|table N m>>Crée une matrice.

    <expand|item*|<key|table N d>>Crée un déterminant.

    <expand|item*|<key|table N c>>Crée une liste de choix.
  </description>

  L'environnement <verbatim|\\eqnarray*> est, lui aussi, une sorte de
  structure tabulaire qui s'étend sur plusieurs lignes. Vous pouvez créer une
  liste d'équations avec <apply|menu|Insert|Mathematics|Equation>.

  À la création, un tableau a une taille minimale (en général 1 x 1) et ses
  cellules sont vides. On peut insérer de nouvelles lignes et de nouvelles
  colonnes avec les raccourcis <key|A-<with|mode|math|<op|\<leftarrow\>>>>,
  <key|A-<with|mode|math|<op|\<rightarrow\>>>>,
  <shortcut|(structured-insert-up)> et
  <shortcut|(structured-insert-down)>. Par exemple,
  <key|A-<with|mode|math|<op|\<rightarrow\>>>> crée une nouvelle colonne à la
  droite de la position du curseur. On peut aussi créer une nouvelle ligne
  en-dessous de la position du curseur en appuyant sur la touche <key|retour
  chariot>.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Tableau>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Mathématiques>|<with|font
      family|<quote|ss>|Équation>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
