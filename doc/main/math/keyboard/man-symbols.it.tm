<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Simboli matematici>

  In <TeXmacs>, i caratteri greci si ottengono combinando il tasto
  modificatore <key|H-> con una lettera. Per esempio, <key|H-a> genera
  <with|mode|math|\<alpha\>> e <key|H-G> genera <with|mode|math|\<Gamma\>>.
  <apply|hyper-link|Ricordiamo|../../start/man-conventions.it.tm> che il
  tasto <key|F5> è equivalente a <key|H->, quindi <with|mode|math|\<rho\>>
  può anche essere ottenuto battendo i tasti <key|F5 r>. Analogamente,
  <key|F6>, <key|F7>, <key|F8> e <key|S-F6> possono essere utilizzati per
  scrivere caratteri in grassetto, calligrafici, fraktur e lavagna grassetto.
  Per esempio, <key|F8 m> produce <with|mode|math|\<frak-m\>>, <key|S-F6 R>
  produce <format|no line break><with|mode|math|\<bbb-R\>> e <key|F6 F7 Z>
  produce <with|mode|math|\<b-cal-Z\>>.

  I caratteri greci si possono ottenere anche come ``varianti'' dei caratteri
  latini utilizzando il tasto <key|<expand|key-variant>>. Per esempio, <key|p
  <expand|key-variant>> produce <with|mode|math|\<pi\>>. Il tasto
  <key|<expand|key-variant>> viene utilizzato anche per ottenere delle
  varianti degli stessi caratteri greci. Per esempio, sia <key|H-p
  <expand|key-variant> tab> che <key|p <expand|key-variant>
  <expand|key-variant> tab> generano <with|mode|math|\<varpi\>>.

  Molti altri simboli matematici si ottengono con ``naturali'' combinazioni
  di tasti. Per esempio, <key|- \<gtr\>> genera
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> produce
  <with|mode|math|\<longrightarrow\>> e <key|\<gtr\> =>
  <with|mode|math|\<geqslant\>>. Analogamente, <key|\| -> genera
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> genera
  <with|mode|math|\<mapsto\>> e <key|- \<gtr\> \<less\> -> genera
  <with|mode|math|\<rightleftarrows\>>. Qui di seguito riportiamo alcune
  regole generali che permettono di ottenere delle varianti dei simboli:

  <\description>
    <expand|item*|<key|tab>>è il tasto principale per ottenere delle
    varianti. Per esempio, <key|\<gtr\> => produce
    <with|mode|math|\<geqslant\>>, ma <key|\<gtr\> = <expand|key-variant>>
    produce <format|no line break><with|mode|math|\<geq\>>. Analogamente,
    <key|\<less\> <expand|key-variant>> produce <with|mode|math|\<prec\>>,
    <key|\<less\> <expand|key-variant> => produce
    <with|mode|math|\<preccurlyeq\>> e <key|\<less\> <expand|key-variant> =
    <expand|key-variant>> genera <with|mode|math|\<preceq\>>. Inoltre, <key|P
    <expand|key-variant> tab> produce <with|mode|math|\<wp\>> e <key|e
    <expand|key-variant> tab> genera la costante
    <with|mode|math|\<mathe\>=exp(1)>. Si può ``tornare indietro''
    utilizzando <key|S-<expand|key-variant>>.

    <expand|item*|<key|@>>viene utilizzato per inserire i simboli in
    cerchietti o in quadratini. Per esempio, <key|@ +> produce
    <with|mode|math|\<oplus\>> e <key|@ x> produce
    <with|mode|math|\<otimes\>>. Analogamente, <key|@ <expand|key-variant> +>
    genera <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>permette di ottenere le negazioni. Per esempio,
    <key|= /> produce <with|mode|math|\<neq\>> e <key|\<less\> = /> produce
    <with|mode|math|<neg|\<leqslant\>>>. Si noti che <key|\<less\> =
    <expand|key-variant> <expand|key-variant> /> genera
    <with|mode|math|\<nleqq\>>, mentre <key|\<less\> = <expand|key-variant>
    <expand|key-variant> / <expand|key-variant>> produce
    <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>si utilizza dopo le frecce per forzare il
    posizionamento dei caratteri seguenti al di sopra o al di sotto delle
    frecce stesse. Per esempio, <key|- - \<gtr\> ^ x> produce
    <with|mode|math|\<longrightarrow\><rsup|x> >, ma <key|- - \<gtr\> ! ^ x>
    produce <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Molti altri simboli, che non possono essere inseriti in modo naturale come
  nei casi descritti qui sopra, si possono ottenere utilizzando il prefisso
  <key|S-F5>. Qui di seguito riportiamo una piccola tabella con alcuni di
  tali simboli:

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Abbreviazione>|<cell|Simbolo>|<cell|Abbreviazione>|<cell|Simbolo>>|<row|<cell|<expand|kbd-symb|a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<expand|kbd-symb|n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<expand|kbd-symb|u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<expand|kbd-symb|v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<expand|kbd-symb|w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Alcuni
  simboli che non possono essere ottenuti utilizzando in modo naturale le
  regole generali.>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia
  Gecchelin|Andrea Centomo>

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
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Alcuni simboli che non possono essere ottenuti
      utilizzando in modo naturale le regole generali.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
