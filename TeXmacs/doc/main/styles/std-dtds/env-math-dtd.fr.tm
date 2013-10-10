<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements mathématiques>

  Le d.t.d. <tmdtd|env-math> spécifie quels environnements mathématiques
  peuvent être utilisés en mode texte. En d'autres termes, ces environnements
  doivent être utilisés en mode texte, mais leur corps contient des formules
  ou tableaux de formules mathématiques.

  <\explain|<markup|equation>>
    Équation numérotée.
  </explain>

  <\explain|<markup|equation*>>
    Équation non numérotée.
  </explain>

  <\explain|<markup|eqnarray>>
    Liste d'équations numérotées (à ne pas encore utiliser).
  </explain>

  <\explain|<markup|eqnarray*>>
    Liste d'équations non numérotées.
  </explain>

  En environnement <markup|eqnarray*>, on peut utiliser la balise
  <markup|eq-number> pour numéroter l'équation.\ 

  <\warning>
    La numérotation des équations dans les tableaux n'est pas encore à son
    optimum. La balise <markup|eqnarray> est identique à <markup|eqnarray*>
    pour le moment. Quand la balise <markup|eqnarray> sera implémentée
    correctement, vous aurez accès à une balise <markup|no-number> pour
    supprimer la numérotation d'une équation et un package de style pour
    appliquer une numérotation à gauche de l'équation..
  </warning>

  <\warning>
    Il n'existe, pour l'instant, pas d'option pour numéroter les équations à
    gauche. Néanmoins, vous pouvez utiliser la balise <markup|leq-number>
    pour ce faire. La balise <markup|next-number> vous permet également
    d'afficher directement le numéro et d'incrémenter le compteur d'équation.
  </warning>

  <\warning>
    Évitez d'utiliser les environnements AMS-<TeX> <verbatim|align>,
    <verbatim|gather> et <verbatim|split>. Néanmoins, si vous désirez le
    faire, ils sont disponibles sous les noms suivants : <markup|align>,
    <markup|gather>, <markup|eqsplit> de même que leurs variantes :
    <markup|align*>, <markup|gather*> et <markup|eqsplit*>. Nous prévoyons
    d'implémenter des environnements plus puissants plus tard.
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