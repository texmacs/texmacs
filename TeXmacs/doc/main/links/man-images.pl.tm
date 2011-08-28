<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Wstawianie obrazków>

  Obrazki mo»na doª¡cza¢ poprzez <menu|Insert|Image>. Obecnie <TeXmacs>
  rozpoznaje formaty <verbatim|ps>, <verbatim|eps>, <verbatim|tif>,
  <verbatim|pdf>, <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>,
  <verbatim|xpm> i <verbatim|fig>. Do wy±wietlania obrazków w formacie
  postscript u»ywany jest <verbatim|gs> (t.j. ghostscript). Je±li nie jest
  zainstalowany w systemie mo»na go uzyska¢ z

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  Inne formaty plików s¡ konwertowane do postscriptu przy u»yciu skryptów
  <verbatim|tiff2ps>, <verbatim|pdf2ps>, <verbatim|pnmtops>,
  <verbatim|giftopnm>, <verbatim|ppmtogif>, <verbatim|xpmtoppm>. Powinny one
  znajdowa¢ si¦ w systemie.

  Domy±lnie obrazki s¡ wy±wietlane zgodnie z ich zaplanowanym rozmiarem.
  Nast¦puj¡ce operacje s¡ mo»liwe podczas wstawiona grafiki:

  <\itemize>
    <item>Wyci¦cie prostok¡tnego fragmentu.

    <item>Zmiana rozmiaru obrazka. Je±li podana zostanie tylko wysoko±¢, bez
    szeroko±ci (lub na odwrót) to zmiana rozmiaru b¦dzie tak wykonana aby
    zachowa¢ proporcje.

    <item>Powi¦kszanie obrazka. Alternatywna metoda zmiany rozmiaru jako
    okre±lenie mno»nika dla wysoko±ci i szeroko±ci.
  </itemize>

  Zaª¡czony jest skrypt do konwersji obrazów, z formuªami <LaTeX>. Aby
  doª¡czy¢ formuª¦ <LaTeX> w obrazku <verbatim|xfig> nale»y tre±¢ formuªy
  wprowadzi¢ jako tekst, zaznaczaj¡c czcionk¦ <LaTeX> i ustawiaj¡c specjaln¡
  flag¦ w polu tekstowym.

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