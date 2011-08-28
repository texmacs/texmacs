<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Ogólne zasady prefiksowania>

  Du»a liczba skrótów klawiszowych wymaga, aby w dzieli¢ je na grupy, celem
  ªatwiejszego zapami¦tania. Podstawow¡ zasad¡ jest i» skróty z tej samej
  kategorii maj¡ wspólny prefiks. Gªówne takie prefiksy to:

  <\description>
    <item*|<key|C-<with|mode|math|x><em|>>>Skróty bazuj¡ce na klawiszu
    Control najcz¦±ciej s¡ u»ywane dla polece« edycyjnych. Zale»¡ one od
    ustawienia opcji Wygl¡d'' w <menu|Edytuj|Ustawienia>. Dla przykªadu
    je±li u»ywasz wygl¡du <name|Emacs> to skróty <prefix|C->
    odpowiadaj¡ tym z <name|Emacs>, zatem <key|C-y> wkleja tekst z bufora.

    <item*|<prefix|A->>Klawisz Alternate jest u»ywany przy
    poleceniach które zale»¡ od trybu w jakim si¦ znajdujesz. Na przykªad
    <shortcut|(make-sqrt)> daje <strong|mocny> tekst w trybie tekstowym i pierwiastek
    kwadratowy <no-break><with|mode|math|<sqrt|>> w trybie matematycznym.
    <key|escape escape> jest równowa»ne <prefix|A->.

    <item*|<prefix|M->>Klawisz meta jest klawiszem dla polece«
    <TeXmacs> ogólnego przeznaczenia, wspólnych dla wszystkich trybów. Dla
    przykªadu <shortcut|(make-label)> tworzy etykiet¦. Jest równie» u»ywany dla dodatkowych
    polece« edytorskich, jak <key|M-w> dla kopiowania tekstu, je±li ustawiony
    jest wygl¡d <name|Emacs>. <key|escape> jest równowa»ny <prefix|M->.

    <item*|<prefix|M-A->>Zdefiniowany przez u»ytkownika klawisz
    modyfikuj¡cy jest u»ywany do tworzenia symboli specjalnych jak litery
    greckie w trybie matematycznym. Mo»na skonfigurowa¢ klawiatur¦ tak aby
    <key|capslock> peªniª funkcj¦ klawisza hyper. <prefix|math:greek> jest równowa»ne
    <prefix|M-A->.
  </description>

  Konkretne klawisze które mog¡ by¢ u»yte do otrzymania prefiksów <prefix|M-> i
  <prefix|M-A-> mo»na <hyper-link|ustawi¢|../../config/man-config-kbd-modkeys.pl.tm>
  poprzez <menu|Edit|Preferences>.

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