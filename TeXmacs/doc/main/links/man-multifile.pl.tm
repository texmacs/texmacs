<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Ksi¡»ki i dokumenty wieloplikowe>

  Gdy dokument staje si¦ du»y, warto podzieli¢ go na mniejsze cz¦±ci. To
  zarówno pozwala ªatwo wykorzysta¢ fragmenty w innych pracach jak i poprawia
  reakcje edytora. Caªy plik mo»e by¢ wstawiony w inny poprzez
  <menu|Insert|Link|Include>. Aby przyspieszy¢ dziaªanie, wª¡czane dokumenty
  s¡ buforowane. Uaktualnienie wstawionych plików nast¦puje poprzez
  <menu|Tools|Update|Inclusions>.

  Przy pisaniu ksi¡»ki mo»na pojedyncze rozdziaªy umieszcza¢ w plikach
  <verbatim|r1.tm>, <verbatim|r2.tm> do <verbatim|rn.tm>. Nast¦pnie wª¡czy¢
  je do pliku <verbatim|ksi¡»ka.tm>. W tym pliku umieszczany te» zostanie
  spis tre±ci, bibliografia itp.

  Aby podczas edycji konkretnego rozdziaªu <verbatim|ri.tm> sprawnie dziaªaªy
  odwoªania pomi¦dzy rozdziaªami mo»na okre±li¢ <verbatim|ksi¡»ka.tm> jako
  nadrz¦dny plik'' dla plików <verbatim|rk.tm> u»ywaj¡c
  <menu|Document|Master|Attach>. Obecnie ten mechanizm nie potrafi okre±li¢
  numerów rozdziaªów, zatem warto r¦cznie przypisa¢ warto±¢ zmiennej
  <src-var|chapter-nr> na pocz¡tku ka»dego rozdziaªu, tak by numerowanie byªo
  prawidªowe podczas edycji.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|polish>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>