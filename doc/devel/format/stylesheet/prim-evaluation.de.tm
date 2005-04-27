<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Steuerung der Evaluierung>

  Dieser Abschnitt beschreibt verschiedene Konstrukte, die steuern, wie
  Ausdrücke in der Stil-Definitions-Sprache evaluiert werden. Die Konstrukte
  sind Analoga zu den <value|scheme>-Befehlen <verbatim|eval>,
  <verbatim|quote>, <verbatim|quasiquote>, <abbr| usw.>, wobei die
  <TeXmacs>-Konventionen sich etwas von normalen funktionellen Sprachen wie
  <name|Scheme> <hlink|unterscheiden|../../style/design/style-eval.de.tm>.

  <\explain>
    <explain-macro|eval|expr><explain-synopsis|Evaluierung erzwingen>
  <|explain>
    Erzeuge Schriftsatz für das Resultat der Evaluierung von <src-arg|expr>.
    Dieses Konstrukt wird meist kombiniert mit <markup|quote> oder
    <markup|quasiquote>, die die Evaluierung verzögern.
  </explain>

  <\explain>
    <explain-macro|quote|expr><explain-synopsis|verzögerte Evaluierung>
  <|explain>
    Evaluierung von <explain-macro|quote|expr> liefert <src-arg|expr> zurück.
    Diese Art der verzögerten Evaluierung wir oft zusammen mit <markup|eval>
    gebraucht.
  </explain>

  <\explain>
    <explain-macro|quasiquote|expr><explain-synopsis|Substitution mit
    verzögerter Evaluierung>
  <|explain>
    Dies ist eine Variante von <markup|quote>, bei der alle Unter-Ausdrücke
    der Form <explain-macro|unquote|subexpr> durch die Evaluierungen von
    <src-arg|subexpr> substituiert wurden. So definiert z.B.

    <\tm-fragment>
      <inactive*|<assign|hello|<quasiquote|<macro|name|<unquote|<localize|Hello>>
      <arg|name>.>>>>
    </tm-fragment>

    ein Makro <markup|hello>, dessen Wert in der aktuellen Sprache ausgegeben
    wird. In einem französischen Dokument würde dies äquivalent zu

    <\tm-fragment>
      <inactive*|<assign|hello|<macro|name|Bonjour <arg|name>.>>>
    </tm-fragment>

    sein.

    Beachten Sie bitte, dass es für solche Anwendungsfälle besser ist,
    <markup|quasiquote> nicht zu verwenden, denn wenn einfach

    <\tm-fragment>
      <inactive*|<assign|hello|<macro|name|<localize|Hello> <arg|name>.>>>
    </tm-fragment>

    verwendet wird, passt sich der Schriftsatz von <inactive*|<hello|Name>>
    automatisch der Sprache an, während in obigen Version immer die Sprache
    verwendet werden, die bei der Makrodefinition aktuell war. Dennoch hat
    die Verwendung von quasiquote den Vorteil, dass die Sprachanpassung von
    \RHello'' nur einmal erfolgen muss. Daher kann <markup|quasiquote>
    manchmal zur Steigerung der Effizienz eingesetzt werden.
  </explain>

  <\explain>
    <explain-macro|unquote|subexpr><explain-synopsis|Evaluierung zulassen>
  <|explain>
    Dieses Konstrukt wird meist zusammen mit <markup|quasiquote> und
    <markup|quasi> verwendet, um Ausdrücke zu markieren, die evaluiert werden
    sollen.
  </explain>

  <\explain>
    <explain-macro|unquote*|subexprs><explain-synopsis|Evaluierung von Listen
    zulassen>
  <|explain>
    Dies ähnelt dem <markup|unquote>, nur dass das Argument
    <src-arg|subexprs> zu einer Liste von Unter-ausdrücken evaluiert, die in
    die Argumente des Elternknotens substituiert werden. So liefert der
    Schriftsatz von

    <\tm-fragment>
      <inactive*|<assign|fun|<xmacro|x|<style-with|src-compact|none|<quasi|<tree|dup|<unquote*|<quote-arg|x>>|<unquote*|<quote-arg|x>>>>>>>>
    </tm-fragment>

    mit <inactive*|<fun|a|b|c>> folgendes

    <\equation*>
      <with|fun|<xmacro|x|<quasi|<tree|dup|<unquote*|<quote-arg|x>>|<unquote*|<quote-arg|x>>>>>|<fun|a|b|c>>
    </equation*>
  </explain>

  <\explain>
    <explain-macro|quasi|expr><explain-synopsis|Substitution>
  <|explain>
    Dies ist eine Abkürzung von <explain-macro|eval|<with|font-shape|right|<explain-macro|quasiquote|expr>>>.
    Dieses Konstrukt wird oft in <TeXmacs>-Stil-Definitionen verwendet,
    Makros zu schreiben, die eine Liste von Makros definieren. So können mit
    dem Makro

    <\tm-fragment>
      <inactive*|<assign|new-theorem|<macro|name|text|<quasi|<assign|<unquote|name>|<macro|body|<surround|<no-indent><strong|<unquote|<arg|text>>.
      >|<right-flush>|<arg|body>>>>>>>>
    </tm-fragment>

    Theorem-ähnliche Kontexte erzeugt werden.
  </explain>

  <\explain>
    <explain-macro|quote-value|var><explain-synopsis|nicht evaluierter Wert>
  <|explain>
    Normalerweise, wenn man den Wert einer Kontextvariablen <src-arg|var>
    erfahren möchte, ist man an daran interessiert, wie sie im Schriftsatz
    erscheint und wie ihn <explain-macro|value|var> erzeugt. Manchmal möchte
    man den wahren, nicht evaluierten Wert haben, was mit
    <explain-macro|quote-value|var> geschehen kann.
  </explain>

  <\explain>
    <explain-macro|quote-arg|var|index-1|<with|mode|math|\<cdots\>>|index-n><explain-synopsis|nicht
    evaluiertes Argument>
  <|explain>
    Normalerweise, wenn man den Wert eines Unter-Ausdrucks eines
    Makro-Arguments <src-arg|var> erfahren möchte, ist man an daran
    interessiert, wie diese im Schriftsatz erscheinen, in eben der Form, die
    <explain-macro|arg|var|index-1|<with|mode|math|\<cdots\>>|index-n>
    erzeugt. Manchmal möchte man die wahren, nicht evaluierten Werte haben,
    was mit<explain-macro|quote-arg|var|index-1|<with|mode|math|\<cdots\>>|index-n>
    geschehen kann.
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