<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Vérifier l'orthographe>

  Si le programme <verbatim|ispell> est installé sur votre système, vous
  pouvez l'utiliser pour vérifier les fautes d'orthographe avec
  <shortcut|(spell-start)> or <apply|menu|Edit|Spell>. Notez que vous devez
  vérifier que vous avez installé les dictionnaires correspondant aux langues
  utilisées dans vos textes ; c'est en général le cas pour l'anglais.

  Après activation de la vérification d'orthographe (soit sur le texte
  intégral, soit sur une zone particulière du texte), les options suivantes
  sont à votre disposition dans le pied de page en cas de faute d'orthographe
  :

  <\description>
    <expand|item*|a)>Accepter le mot tel quel ainsi que ses occurrences
    suivantes.

    <expand|item*|r)>Remplacer le mot par un mot nouveau à saisir.

    <expand|item*|i)>Indiquer que le mot <space|0.2spc>litigieux<space|0.2spc>
    est en fait correct et qu'il doit être incorporé dans votre dictionnaire
    personnel.

    <expand|item*|1-9)>Choisir une des suggestions proposées.
  </description>

  Notez que <verbatim|ispell> n'effectue qu'une correction orthographique. Il
  ne sait pas détecter les fautes de grammaire.

  Le vérificateur d'orthographe utilise le dictionnaire correspondant à la
  langue active à la position du curseur (ou du début d'une sélection). Seul
  le texte écrit dans cette langue est vérifié. Si votre document contient
  plusieurs langues, vous devrez lancer le vérificateur d'orthographe pour
  toutes ces langues.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Orthographe>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
