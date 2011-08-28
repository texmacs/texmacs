<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environnements mathématiques>

  Le d.t.d. <tmdtd|env-math> spécifie quels environnements mathématiques
  peuvent être utilisés en mode texte. En d'autres termes, ces environnements
  doivent être utilisés en mode texte, mais leur corps contient des formules
  ou tableaux de formules mathématiques.

  <\description>
    <expand|item*|<markup|equation>>Équation numérotée.

    <expand|item*|<markup|equation*>>Équation non numérotée.

    <expand|item*|<markup|eqnarray>>Liste d'équations numérotées (à ne pas
    encore utiliser).

    <expand|item*|<markup|eqnarray*>>Liste d'équations non numérotées.
  </description>

  En environnement <markup|eqnarray*>, on peut utiliser la balise
  <markup|eqnumber> pour numéroter l'équation.\ 

  <\warning>
    La numérotation des équations dans les tableaux n'est pas encore à son
    optimum. La balise <markup|eqnarray> est identique à <markup|eqnarray*>
    pour le moment. Quand la balise <markup|eqnarray> sera implémentée
    correctement, vous aurez accès à une balise <markup|nonumber> pour
    supprimer la numérotation d'une équation et un package de style pour
    appliquer une numérotation à gauche de l'équation..
  </warning>

  <\warning>
    Il n'existe, pour l'instant, pas d'option pour numéroter les équations à
    gauche. Néanmoins, vous pouvez utiliser la balise <markup|leqnumber> pour
    ce faire. La balise <markup|nextnumber> vous permet également d'afficher
    directement le numéro et d'incrémenter le compteur d'équation.
  </warning>

  <\warning>
    Évitez d'utiliser les environnements AMS-<TeX> <verbatim|align>,
    <verbatim|gather> et <verbatim|split>. Néanmoins, si vous désirez le
    faire, ils sont disponibles sous les noms suivants : <markup|align>,
    <markup|gather>, <markup|eqsplit> de même que leurs variantes :
    <markup|align*>, <markup|gather*> et <markup|eqsplit*>. Nous prévoyons
    d'implémenter des environnements plus puissants plus tard.
  </warning>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|3|?>>
    <associate|idx-15|<tuple|3|?>>
    <associate|idx-16|<tuple|3|?>>
    <associate|idx-17|<tuple|3|?>>
    <associate|idx-18|<tuple|3|?>>
    <associate|idx-19|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnumber>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nonumber>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|leqnumber>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nextnumber>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit*>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
