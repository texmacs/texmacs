<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environnements théorèmes>

  Le d.t.d. <tmdtd|env-theorem> \ définit les balises d'affichage des
  environnements théorèmes. Les balises principales sont les suivantes :

  <\description>
    <expand|item*|<markup|theorem*>>Macro d'affichage des environnements
    théorèmes. Le premier argument donne le nom du théorème, tel
    <space|0.2spc>Théorème 1.2<space|0.2spc> et le second argument est le
    corps du théorème. Cet environnement est utilisé par les environnements
    définis avec <markup|newtheorem>.

    <expand|item*|<markup|remark*>>Identique à <markup|theorem*> pour les
    environnements remarques.

    <expand|item*|<markup|exercise*>>Identique à <markup|theorem*> pour les
    environnements exercices.

    <expand|item*|<markup|proof*>>Identique à <markup|theorem*> pour les
    démonstrations. Cet environnement est principalement utilisé pour
    personnaliser le nom d'une démonstration, comme dans <space|0.2spc>Fin
    de la démonstration du théorème 1.2<space|0.2spc>.\ 

    <expand|item*|<markup|dueto>>Environnement qui peut être utilisé pour
    indiquer des auteurs d'un théorème.

    <expand|item*|<markup|corollary*>>Pour les corollaires non numérotés. Cet
    environnement se base sur <markup|theorem*>.

    <expand|item*|<markup|proof>>Pour la démonstration des théorèmes. Cet
    environnement se base sur <markup|proof*>.
  </description>

  Les balises suivantes peuvent être utilisées pour personnaliser les
  environnements.

  <\description>
    <expand|item*|<markup|theoremname>>Macro qui gère l'apparence des noms
    des environnements théorèmes et remarques. La plupart utilisent un style
    gras ou des petites majuscules.

    <expand|item*|<markup|exercisename>>Identique à <markup|theoremname> pour
    les exercices.

    <expand|item*|<markup|theoremsep>>Séparateur entre le nom d'un
    environnement théorème ou remarque et son corps. Par défaut, il s'agit
    d'un point suivi d'une espace.

    <expand|item*|<markup|exercisesep>>Identique à <markup|theoremsep> pour
    les exercices.
  </description>

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
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-theorem>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|remark*>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercise*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollary*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisename>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisesep>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
