<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Boolesche Operatoren>

  <\explain>
    <explain-macro|or|expr-1|<with|mode|math|\<cdots\>>|expr-n>

    <explain-macro|and|expr-1|<with|mode|math|\<cdots\>>|expr-n>
  <|explain>
    Gibt des Ergebnis einer Oder- bzw. Und-Verknüpfung, or/and, der Ausdrücke
    <src-arg|expr-1> bis <src-arg|expr-<no-break>n> zurück. Z.B. ergibt
    <inactive*|<or|false|<equal|1|1>|false>> <or|false|<equal|1|1>|false>.
  </explain>

  <\explain>
    <explain-macro|xor|expr-1|expr-2>
  <|explain>
    Exklusives Oder zweier Ausdrücke <src-arg|expr-1> und <src-arg|expr-2>.
    <inactive*|<xor|true|true>> ergibt <xor|true|true>.
  </explain>

  <\explain>
    <explain-macro|not|expr>
  <|explain>
    Negation von <src-arg|expr>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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
  </collection>
</initial>