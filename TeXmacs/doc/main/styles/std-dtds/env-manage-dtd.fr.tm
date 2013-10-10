<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Définition de nouveaux environnements>

  Le fichier <tmdtd|env-manage> contient des balises de haut niveau qui
  peuvent être utilisées pour définir de nouveaux environnements de
  théorèmes, de remarques, d'exercices et de figures :

  <\explain|<markup|new-theorem>>
    Définit un environnement théorème. Vous devez spécifier un nom
    d'environnement (tel \S<space|0.2spc>expérimentation<space|0.2spc>\T) et
    le texte correspondant (tel \S<space|0.2spc>Expérimentation<space|0.2spc>\T).
  </explain>

  <\explain|<markup|new-remark>>
    Identique à <markup|new-theorem> pour les remarques.
  </explain>

  <\explain|<markup|new-exercise>>
    Identique à <markup|new-theorem> pour les exercices.
  </explain>

  <\explain|<markup|new-figure>>
    Identique à <markup|new-theorem> pour les figures (toujours par paire :
    petite et grande).
  </explain>

  Le <abbr|d.t.d.> contient aussi des balises de bas niveau pour définir les
  environnements. En fait, la définition de nouveaux théorèmes se fait en
  deux étapes. Dans la première étape, la balise <markup|new-theorem> est
  utilisée pour indiquer quel type d'environnement théorème sera défini. Dans
  une seconde étape, qui a lieu juste avant que le document utilisateur soit
  compilé, les environnements théorèmes sont effectivement définis. Ce
  mécanisme permet de personnaliser les environnements dans des packages qui
  sont mis en route entre les deux étapes. Par exemple, la numérotation des
  théorèmes est faite de cette façon.

  <\warning>
    À l'heure actuelle, nous ne devez utiliser la balise <markup|new-theorem>
    et les balises similaires qu'à l'intérieur d'un fichier de style
    personnalisé ou d'un package. Si vous utilisez <markup|new-theorem>
    directement à l'intérieur d'un document, la numérotation sera incorrecte,
    compte tenu du mécanisme en deux étapes expliqué ci-dessus. Ce problème
    sera résolu lorsqu'on pourra spécifier des préambules corrects pour les
    documents <TeXmacs>.
  </warning>

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