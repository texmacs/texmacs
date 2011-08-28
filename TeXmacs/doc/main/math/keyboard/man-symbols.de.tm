<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematische Symbole eingeben>

  Die griechischen Buchstaben können in <TeXmacs> eingegeben werden, indem
  die <em|Hyper-Modifiertaste> <prefix|M-A-> mit einem Buchstaben kombiniert wird.
  Beispielsweise ergibt <key|H-a> \ <with|mode|math|\<alpha\>> und <key|H-G>
  gibt <with|mode|math|\<Gamma\>>. <hyper-link|Erinnern Sie
  sich|../../start/man-conventions.de.tm>, dass <prefix|math:greek> zu <prefix|M-A->
  äquivalent ist, so dass man <with|mode|math|\<rho\>> mit <key|F5 r>
  erhalten kann.

  In ähnlicher Weise können <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> und <prefix|math:bbb>
  benutzt werden, um in dieser Reihenfolge,
  <with|mode|math|\<b-f\>\<b-e\>\<b-t\>\<b-t\>\<b-e\>\<b-n\>
  \<b-T\>\<b-e\>\<b-x\>\<b-t\>>, <with|mode|math|\<cal-K\>\<cal-A\>\<cal-L\>\<cal-L\>\<cal-I\>\<cal-G\>\<cal-R\>\<cal-A\>\<cal-F\>\<cal-I\>\<cal-S\>\<cal-C\>\<cal-H\>\<cal-E\>\<cal-N\>
  \ \ \ \ \<cal-T\>\<cal-E\>\<cal-X\>\<cal-T\>>,
  <with|mode|math|\<frak-F\>\<frak-r\>\<frak-a\>\<frak-k\>\<frak-t\>\<frak-u\>\<frak-r\>>
  und <with|mode|math|\<bbb-F\>\<bbb-E\>\<bbb-T\>\<bbb-T\>\<bbb-E\>
  \ \ \ \ \ \<bbb-T\>\<bbb-A\>\<bbb-F\>\<bbb-E\>\<bbb-L\>\<bbb-S\>\<bbb-C\>\<bbb-H\>\<bbb-R\>\<bbb-F\>\<bbb-T\>>
  zu erzeugen. \ <shortcut|\<frak-m\>> gibt <with|mode|math|\<frak-m\>>, <key|S-F6 R>
  gibt <no-break><with|mode|math|\<bbb-R\>> und <shortcut|\<b-cal-Z\>> ergibt
  <with|mode|math|\<b-cal-Z\>>.

  Griechische Buchstaben können auch als \RVarianten`` von lateinischen
  Buchstaben mit der \ <key|var>-Taste erzeugt werden.
  Beispielsweise liefert <key|p var> im Mathematik-Modus
  <with|mode|math|\<pi\>>. Die <key|var>-Taste kann auch dazu
  gebraucht werden, Varianten von griechischen Buchstaben zu erzeugen. Z.B.
  <key|H-p tab var> und <key|p var var tab>
  ergeben <with|mode|math|\<varpi\> >, <key|H-p var> und <key|p
  var tab> \ <with|mode|math|\<mathpi\>>, \ <key|H-p> und <key|p
  tab> \ <with|mode|math|\<pi\>>.

  \;

  Viele andere Buchstaben erhält man durch \Rnahe liegende``
  Tastenkombinationen. Beispielsweise ergibt <key|- \<gtr\>>
  \ <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>>
  \ <with|mode|math|\<longrightarrow\>> und <key|\<gtr\> =>
  \ <with|mode|math|\<geqslant\>>. Oder <key|\| -> ergibt
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>>
  \ <with|mode|math|\<mapsto\>> und <key|- \<gtr\> \<less\> ->
  \ <with|mode|math|\<rightleftarrows\>>.\ 

  Es gibt einige Regeln:

  <\description>
    <item*|<key|tab>>ist die wichtigste Taste zur Erzeugung von Varianten.
    Z.B. <key|\<gtr\> => yields <with|mode|math|\<geqslant\>>, aber
    <key|\<gtr\> = var> erzeugt <with|mode|math|\<geq\>>.
    \ <key|\<less\> var> gibt <with|mode|math|\<prec\>>,
    <key|\<less\> var => gibt <with|mode|math|\<preccurlyeq\>> und
    <key|\<less\> var = var> gibt
    <with|mode|math|\<preceq\>>. Also, <key|P var tab> erzeugt
    <with|mode|math|\<wp\>> und <key|e var tab> ergiebt die
    Konstante <with|mode|math|\<mathe\>=exp(1)>. Sie könne mit
    <key|S-var> wieder zurückgehen.

    <item*|<key|@>>liefert Zeichen in Kreisen und Quadraten. Z.B. <key|@ +>
    \ ergibt <with|mode|math|\<oplus\>>, <key|@ x>
    \ <with|mode|math|\<otimes\>>und <key|@ var +> gibt
    <with|mode|math|\<boxplus\>>.

    <item*|<key|/>>wird für Negatioen benutzt. <key|= /> ergibt
    <with|mode|math|\<neq\>> und <key|\<less\> = />
    \ <with|mode|math|<neg|\<leqslant\>>>. Beachte, dass \ <key|\<less\> =
    var var /> <with|mode|math|\<nleqq\>> ergibt, während
    <key|\<less\> = var var / var>
    \ <with|mode|math|\<lneqq\>> erzeugt.

    <item*|<key|!>>wird nach Pfeilen benutzt, um untere und obere Indices
    direkt ober- bzw. unterhalb der Pfeile zu plazieren: \ <key|- - \<gtr\> ^
    x> ergibt <with|mode|math|\<longrightarrow\><rsup|x> >, aber <key|- -
    \<gtr\> ! ^ x> ergibt <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Einige andere Symbole, die nicht durch \Rnahe liegende``
  Tastenkombinationen erzeugt werden können, werden mit dem Präfix <prefix|symbol>
  eingegeben:

  <big-table|<descriptive-table|<tformat|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-rborder|1ln>|<table|<row|<cell|Kurzbefehl>|<cell|Symbol>|<cell|Kurzbefehl>|<cell|Symbol>>|<row|<cell|<key|symbol a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<key|symbol n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<key|symbol u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<key|symbol v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<key|symbol w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Einige
  Symbole, die nicht auf naheliegende Weise mit Tastenkombinationen erzeugt
  werden können.>

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
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>