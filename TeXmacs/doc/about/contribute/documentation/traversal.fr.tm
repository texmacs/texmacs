<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Navigation dans la documentation <TeXmacs>>

  En règle générale, vous devez éviter d'utiliser des sections dans la
  documentation <TeXmacs> et essayer d'écrire des pages d'aide courtes sur
  des sujets bien précis. Ensuite, vous devez créer des
  <space|0.2spc>méta-fichiers d'aide<space|0.2spc> qui indiquent comment
  naviguer automatiquement dans la documentation. Ceci permet le réemploi
  d'une page d'aide pour différents usages (un livre destiné à l'impression,
  un tutoriel sur un site web, etc...).

  Le style <tmstyle|tmdoc> fournit trois macros de marquage pour indiquer
  comment naviguer dans la documentation. La macro <markup|traverse> est
  utilisée pour délimiter des régions de navigation. La macro <markup|branch>
  signale une page d'aide qui doit être considérée comme sous-section et la
  macro <markup|continue> indique une page de suite. Les macros
  <markup|branch> et <markup|continue> prennent deux arguments. Le premier
  argument décrit le lien, le second donne l'adresse physique relative du
  fichier lié.

  En général, à la fin d'un méta-fichier d'aide, on trouve plusieurs macros
  <markup|branch> ou <markup|continue> à l'intérieur d'une macro
  <markup|traverse>. Vous devez aussi indiquer en haut du document son titre
  avec la macro <markup|tmdoc-title>. Lors de la création automatique d'un
  livre destiné à l'impression, une structure chapitre-section-sous-section
  sera automatiquement générée à partir de ces informations et des titres des
  documents. On peut aussi créer automatiquement des boutons de navigation
  pour usage dans un navigateur.

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdoc-title>>|<pageref|idx-10>>
    </associate>
  </collection>
</auxiliary>
