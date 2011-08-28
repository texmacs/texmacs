<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Oznaczenia i konwencje>

  W podr¦czniku <TeXmacs> elementy menu b¦d¡ opisywane czcionk¡ <em|sans
  serif>, tak jak <menu|Document>, <menu|File|Load> lub <menu|Insert|Font
  shape|Italic>. Dla znaków wprowadzanych z klawiatury u»ywana b¦dzie
  czcionka <em|typewriter> wewn¡trz ramki, na przykªad <key|C-s>. Po prawej
  stronie elementów menu s¡ zapisane kombinacje klawiszowe, gdy s¡ dost¦pne.
  Nast¦puj¡ce oznaczenia s¡ u»ywane przy skrótach klawiszowych:

  <\description>
    <item*|<prefix|S->>Dla kombinacji z klawiszem shift.

    <item*|<prefix|C->>Dla kombinacji z klawiszem control.

    <item*|<verbatim|><prefix|A->>Dla kombinacji z klawiszem alternate.

    <item*|<prefix|M->>Dla kombinacji z klawiszem meta.

    <item*|<prefix|M-A->>Dla kombinacji z klawiszem hyper.
  </description>

  Dla przykªadu <shortcut|(make-with font-series bold)> oznacza <key|A-C-b>.
  Spacja wewn¡trz skrótu informuje i» jest to wielokrokowa sekwencja. Czyli,
  <key|M-t N b> oznacza <key|meta-t> <key|N> <key|b>.

  Klawisze <prefix|A->, <prefix|M-> i <prefix|M-A-> nie s¡
  na wszystkich klawiaturach. Wspóªczesne PC-ty maj¡ klawisz <key|meta>
  cz¦sto zast¡piony przez <key|windows>. W przypadku, gdy na klawiaturze
  brakuje jednego lub kilku modyfikatorów, mo»na u»y¢ <key|escape> zamiast
  <prefix|M->, <key|escape escape> zamiast <prefix|A-> i <prefix|math:greek>, <key|escape
  escape escape> lub <prefix|A-C-> zamiast <prefix|M-A->. Czyli, <key|escape w> to
  jest to samo co <key|M-w>. Mo»na równie» <hyper-link|dostosowa¢ klawisz
  modyfikuj¡ce|../config/man-config-kbd-modkeys.pl.tm> aby w peªni cieszy¢
  si¦ zaletami pot¦»nego zbioru skrótów klawiszowych dost¦pnych w <TeXmacs>.

  Zachowanie menu i klawiatury jest <em|kontekstowe>, t.j. zale»y od
  aktualnego trybu (np. tekst czy tryb matematyczny'') i u»ywanego j¦zyka.
  Skróty przydatne do wprowadzania równa« s¡ bezu»yteczne w zwykªym trybie
  tekstowym.

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