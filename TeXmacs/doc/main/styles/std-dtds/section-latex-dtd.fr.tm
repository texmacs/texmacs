<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Sections de style <LaTeX>>

  Le d.t.d. <tmdtd|section-latex> définit les balises standards de section,
  qui sont les mêmes que dans <LaTeX>. La plupart des balises de section ne
  prennent qu'un argument : le nom de la section. Nous avons prévu
  d'implémenter d'autres balises à deux arguments qui permettront de
  considérer le corps de la section comme faisant partie de la structure. Les
  balises suivantes correspondent généralement à des sections numérotées qui
  sont référencées dans la table des matières :

  <\explain|<markup|chapter>>
    Macro pour créer un titre de chapitre qui peut être numéroté.
  </explain>

  <\explain|<markup|section>>
    Macro pour créer un titre de section qui peut être numérotée.
  </explain>

  <\explain|<markup|subsection>>
    Macro pour créer un titre de sous-section qui peut être numérotée.
  </explain>

  <\explain|<markup|subsubsection>>
    Macro pour créer un titre de sous-sous-section qui peut être numérotée.
  </explain>

  <\explain|<markup|paragraph>>
    Macro pour créer un titre de paragraphe qui peut être numéroté.
  </explain>

  <\explain|<markup|subparagraph>>
    Macro pour créer un titre de sous-paragraphe qui peut être numéroté.
  </explain>

  Les balises <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> et <markup|subparagraph*>
  peuvent être utilisées pour créer les variantes non numérotées des balises
  ci-dessus, qui ne sont pas référencées dans la table des matières. Le
  d.t.d. <tmdtd|section-latex> définit aussi les balises suivantes :

  <\explain|<markup|chapter**>>
    Macro à deux arguments : un type de chapitre spécial (tel
    \S<space|0.2spc>Épilogue<space|0.2spc>\T) et le nom du chapitre.
  </explain>

  <\explain|<markup|appendix>>
    Variante de <markup|chapter> ou <markup|section> pour créer des annexes.
  </explain>

  <\explain|<markup|section-sep>>
    Macro pour personnaliser le séparateur entre le numéro de la section et
    son titre. Par défaut, on utilise deux espaces.
  </explain>

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