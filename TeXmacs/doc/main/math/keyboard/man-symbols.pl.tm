<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Wprowadzanie symboli matematycznych>

  Grecki alfabet jest uzyskiwany w <TeXmacs> poprzez ª¡czenie modyfikatora
  hyper'' <prefix|M-A-> z liter¡. Na <no-break>przykªad, <key|H-a> daje
  <with|mode|math|\<alpha\>> a <key|H-G> <with|mode|math|\<Gamma\>>. Jak to
  <hyper-link|wcze±niej|../../start/man-conventions.pl.tm> napisano <prefix|math:greek>
  jest równowa»ny z <prefix|M-A->, zatem <with|mode|math|\<rho\>> mo»e by¢ równie»
  uzyskane jako <key|F5 r>. Dodatkowo <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> i
  <prefix|math:bbb> mog¡ by¢ u»yte do wpisania: znaku pogrubionego, kaligraficznego,
  fraktur i blackboard odpowiednio. Na przykªad, <shortcut|\<frak-m\>> daje
  <with|mode|math|\<frak-m\>>, <key|S-F6 R> to <with|mode|math|\<bbb-R\>> a
  <shortcut|\<b-cal-Z\>> oznacza <with|mode|math|\<b-cal-Z\>>. Niestety aby uzyska¢
  <with|mode|math|<with|math-font|Bbb*|1>> trzeba u»y¢ czcionki blackboard
  (<key|M-A-B>).

  Greckie litery mog¡ by¢ równie» uzyskane jako wariant'' alfabetu
  ªaci«skiego przy u»yciu klawisza <key|tab>. Czyli, <key|p tab> daje
  <with|mode|math|\<pi\>>. Klawisz <key|tab> pozwala uzyska¢ warianty
  greckich liter. Na <no-break>przykªad oba <key|H-p tab tab> i <key|p tab
  tab tab> oznaczaj¡ <with|mode|math|\<varpi\>>.

  Wiele innych symboli matematycznych uzyskiwanych jest poprzez naturalne''
  kombinacje klawiszy. Na przykªad, <key|- \<gtr\>> daje
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> daje
  <with|mode|math|\<longrightarrow\>>i <key|\<gtr\> =>
  wy±wietla<with|mode|math|\<geqslant\>>. Podobnie, <key|\| -> daje
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> wy±wietla
  <with|mode|math|\<mapsto\>>a <key|- \<gtr\> \<less\> -> daje
  <with|mode|math|\<rightleftarrows\>>. Ogólne zasady zuzyskiwania odmian
  symboli:

  <\description>
    <item*|<key|tab>>jest gªównym klawiszem przy uzyskiwaniu wariantów. Na
    przykªad, <key|\<gtr\> => oznacza <with|mode|math|\<geqslant\>>, ale
    <key|\<gtr\> = tab> wy±wietla <with|mode|math|\<geq\>>. Podobnie,
    <key|\<less\> tab> daje <with|mode|math|\<prec\>>, <key|\<less\> tab =>
    oznacza <with|mode|math|\<preccurlyeq\>> i <key|\<less\> tab = tab> daje
    <with|mode|math|\<preceq\>>. Równie» <key|P tab tab> daje
    <with|mode|math|\<wp\>> i <key|e tab tab> wy±wietla staª¡
    <with|mode|math|\<mathe\>=exp<left|(>1<right|)>.> Mo»na kr¡»y¢ w tyª''
    u»ywaj¡c <key|S-tab>.

    <item*|<key|@>>jest u»ywany do wrzucania symboli do kóª lub kwadratów. Na
    przykªad, <key|@ +> daje <with|mode|math|\<oplus\>>, a <no-break><key|@
    x> oznacza <with|mode|math|\<otimes\>>. Analogicznie, <key|@ tab +> daje
    <with|mode|math|\<boxplus\>>.

    <item*|<key|/>>jest u»ywany do negacji. Na przykªad, <key|= /> oznacza
    <with|mode|math|\<neq\>> i <key|\<less\> = /> daje
    <with|mode|math|\<nleqslant\>>. Zauwa»my i» <key|\<less\> = tab tab /> to
    <with|mode|math|\<nleqq\>>, podczas gdy <key|\<less\> = tab tab / tab>
    daje <with|mode|math|\<lneqq\>>.

    <item*|<key|!>>jest u»ywany po strzaªkach aby wymusi¢ indeksy nad lub pod
    strzaªk¡. Na przykªad, <key|- - \<gtr\> ^ x> oznacza
    <with|mode|math|\<longrightarrow\><rsup|x>>, ale <key|- - \<gtr\> ! ^ x>
    daje <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Par¦ symboli, które nie mog¡ by¢ wprowadzone naturalnie powy»szymi metodami
  jest uzyskiwanych poprzez prefiks <prefix|symbol>. Poni»ej krótka tabelka takich
  znaków:

  <big-table|<descriptive-table|<tformat|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-rborder|1ln>|<table|<row|<cell|Skrót>|<cell|Symbol>|<cell|Skrót>|<cell|Symbol>>|<row|<cell|<key|symbol a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<key|symbol n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<key|symbol u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<key|symbol v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<key|symbol w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Niektóre
  symbole bez naturalnej metody wprowadzenia.>

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