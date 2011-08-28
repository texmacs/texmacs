<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Tworzenie indeksu>

  Dla stworzenia indeksu nale»y najpierw wstawi¢ hasªa w dokument u»ywaj¡c
  <menu|Insert|Link|Index entry>. Po wybraniu miejsca gdzie indeks ma zosta¢
  umieszczony nale»y u»y¢ <menu|Insert|Automatic|Index>. Indeks jest tworzony w
  podobny sposób jak spis tre±ci.

  W menu <menu|Insert|Link|Index entry> jest kilka rodzajów pozycji indeksu.
  Najprostsze to gªówne'', podrz¦dne'', pod-podrz¦dne'' które s¡ makrami z
  jednym, dwoma lub trzema argumentami odpowiednio. Wpisy typu podrz¦dnego''
  i pod-podrz¦dnego'' s¡ u»ywane aby ustawi¢ hierarchi¦ pozycji indeksu.

  Zªo»one hasªo indeksu przyjmuje cztery argumenty. Pierwszy to klucz pod
  jakim jest przechowywane i musi to by¢ tuple'' (tworzone przez <key|inactive
  \<less\>>) którego pierwszym skªadnikiem jest gªówna kategoria, drugim
  podkategoria itd. Drugi argument zªo»onego hasªa jest albo pusty lub
  strong'', w przypadku którego numer strony zostanie napisany tªust¡
  czcionk¡. Trzeci argument jest zwykle pusty, ale po stworzeniu dwóch haseª
  z tym samym nie pustym trzecim argumentem powstanie hasªo z zakresem''
  stron. Czwartym argumentem, którym znowu jest tuple'', jest hasªo.

  Jest równie» mo»liwe stworzenie hasªa indeksu bez numeru strony przy u»yciu
  W miejsce'' w <menu|Insert|Link|Index entry>. Pierwszym argumentem jest
  klucz do sortowania indeksu. Drugi argument zawiera wªa±ciwy tekst. Ta
  konstrukcja mo»e by¢ przydatna do tworzenia ró»nych sekcji A'', B'' itp.
  w indeksie.

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