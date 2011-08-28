<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard-Kontexte>

  In <tmdtd|std-markup> wird eine gröÿere Anzahl von unterschiedlichen
  Kontexten definiert. Die folgenden Text-Befehle haben alle ein Argument.
  Die meisten können im Menü <menu|Insert|Content tag> gefunden werden.

  <\explain|<explain-macro|strong|content> <strong|= <localize|strong>>>
    Geeignet zur Hervorhebung von \ <strong|wichtigen> Textstellen. Sie
    können diesen Tag mit <menu|Insert|Content tag|Strong> einfügen.
  </explain>

  <\explain|<explain-macro|em|content> <strong|= <localize|em>>>
    Hebt Text mit der Schriftart <em|Italic> hervor. Entspricht dem
    Menü-Eintrag <menu|Insert|Content tag|Emphasize>.
  </explain>

  <\explain|<explain-macro|dfn|content> <strong|= <localize|dfn>>>
    Für <dfn|Definitionen>. \ Entspricht dem Menü-Eintrag <menu|Insert|Content
    tag|Definition>.
  </explain>

  <\explain|<explain-macro|samp|content> <strong|= <localize|samp>>>
    Eine Sequenz von Zeichen wie z.B. \ <samp|ae> ligature æ. \ Entspricht
    dem Menü-Eintrag <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<explain-macro|name|content> <strong|= <localize|name>>>
    Der Name, die Bezeichnung von etwas (nicht einer Person), z.B.
    <name|Linux>. \ Entspricht dem Menü-Eintrag <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<explain-macro|person|content><strong|= <localize|person>>>
    Der Name einer person, z.B. \ <name|Joris>. Entspricht dem Menü-Eintrag
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<explain-macro|cite*|content><strong|= <localize|cite*>>>
    Ein bibliographisches Zitat im Text (Hinweis auf ein Buch oder
    Zeitschrift). Z.B. \ Melville's <cite*|Moby Dick>. Entspricht dem
    Menü-Eintrag <menu|Insert|Content tag|Cite>. <strong|Achtung!>, dies sollte
    nicht mit <markup|cite> verwechselt werden. Letzteres ist dient zum
    Referenzieren von Literaturzitaten, die sich in einer besonderen Datei
    befinden.
  </explain>

  <\explain|<explain-macro|abbr|content> <strong|= <localize|abbr>>>
    Eine Abkürzung, z.B. Ich arbeite am <abbr|C.N.R.S.> Entspricht dem
    Menü-Eintrag <menu|Insert|Content tag|Abbreviation>, Kann auch mit
    <key|text a> eingefügt werden.
  </explain>

  <\explain|<explain-macro|acronym|content> <strong|= <localize|acronym>>>
    Ein Akronym ist eine Abkürzung, die aus den ersten Buchstaben jedes
    Wortes eines Namens oder eines Satzes besteht, z.B. \ <acronym|HTML> oder
    <acronym|IBM>. Die Buchstaben werden nicht durch Punkte getrennt.
    \ Entspricht dem Menü-Eintrag <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<explain-macro|verbatim|content> <strong|= <localize|verbatim>>>
    Wörtlicher Text, wie z.B. die Ausgabe eines anderen Computer-Programm:
    Das Programm sagte: <verbatim|Hallo>. Entspricht dem Menü-Eintrag
    <menu|Insert|Content tag|Verbatim>. Der Befehl kann auch als Kontext für
    Text benutzt werden, der aus mehreren Absätzen besteht.
  </explain>

  <\explain|<explain-macro|kbd|content> <strong|= <localize|kbd>>>
    Text, der über die Tastatur eingegeben werden soll. Bitte geben Sie
    \R<kbd|end>`` ein. Entspricht dem Menü-Eintrag <menu|Insert|Content
    tag|Keyboard>.
  </explain>

  <\explain|<explain-macro|code*|content> <strong|= <localize|code*>>>
    Quellcode von Programmiersprachen, z.B. \R<code*|cout \<less\>\<less\>
    1+1;> gibt <verbatim|2>''. Entspricht dem Menü-Eintrag <menu|Insert|Content
    tag|Code>. Für mehrzeiligen Quellcode sollte <markup|code> benutzt
    werden.
  </explain>

  <\explain|<explain-macro|var|content> <strong|= <localize|var>>>
    Variablen in einem Computerprogramm, wie z.B. in \R<verbatim|cp
    <var|src-datei> <var|ziel-datei>>``. Entspricht dem Menü-Eintrag
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<explain-macro|math|content> <strong|= <localize|math>>>
    Dieser Befehl wird in Zukunft für Mathematik innerhalb von normalem Text
    Verwendung finden. Beispiel: Die Formel <math|sin<rsup|2> x+cos<rsup|2>
    x=1> ist gut bekannt.
  </explain>

  <\explain|<explain-macro|op|content> <strong|= <localize|op>>>
    <strong|<localize|op>> dient dazu Operatoren innerhalb von mathematischen
    Texten benutzt werden, um hervorzuheben, dass dieser Operator allein,
    ohne Argumente steht wie z.B. <with|mode|math|\<bbb-R\>> in: \RDie
    <math|<op|+>> Operation ist eine Funktion von
    <with|mode|math|\<bbb-R\><rsup|2>> in <with|mode|math|\<bbb-R\>>``.
    Dieser Kontext wird möglicherweise überflüssig.
  </explain>

  <\explain|<explain-macro|tt|content> <strong|= <localize|tt>>>
    Hiermit wird die Schrift auf <with|font-family|tt|<localize|typewriter>>
    umgestellt. Es wird zur Kompatibilität mit <name|HTML> gebraucht, wir
    raten von der Verwendung ab. Kann über das Menü
    <menu|Format|Variant|Typewriter> eingestellt werden.
  </explain>

  Standard-Kontexte:

  <\explain|<explain-macro|verbatim|body> <strong|= <localize|verbatim>>>
    Bereits oben beschrieben.
  </explain>

  <\explain|<explain-macro|code|body> <strong|= <localize|code>>>
    Ähnlich <markup|code*>, aber für mehrzeiligen Quellcode.
  </explain>

  <\explain|<explain-macro|quote-env|body><strong| = quote-env>>
    Formatierung von Zitaten, die aus einem einzigen Absatz bestehen.
  </explain>

  <\explain|<explain-macro|quotation|body> <strong|= <localize|quotation>>>
    Formatierung von Zitaten, die aus mehreren Absätzen bestehen.
  </explain>

  <\explain|<explain-macro|verse|body> <strong|= <localize|verse>>>
    Formatierung von Poesie.
  </explain>

  <\explain|<explain-macro|center|body> <strong|= <localize|center>>>
    Damit kann eine oder auch mehrere Zeilen zentriert dargestellt werden. Es
    dient der Kompatibilität mit <name|HTML>, wir raten von der Verwendung
    ab. \ Kann über das Menü <menu|Format|Alignment|Centered> eingestellt
    werden.
  </explain>

  Einige Standard Tabellen-Formate:

  Links ausgerichtete (normale) Tabelle ohne sichtbare Zellränder (Gitter).
  Tabellen können mit dem Menü <menu|Insert|Table|Plain tabular> so
  formatiert werden.

  <\explain|<explain-macro|tabular*|table> <strong|= tabular>*>
    Zentrierte Tabelle ohne sichtbare Zellränder (Gitter). Tabellen können
    mit dem Menü <menu|Insert|Table|Centered tabular> so formatiert werden.
  </explain>

  <\explain>
    <explain-macro|block|table> <strong|= block>
  <|explain>
    Links ausgerichtete Tabelle mit sichtbaren Standard-Zellrändern (Gitter,
    <verbatim|1ln>). Tabellen können mit dem Menü <menu|Insert|Table|Plain
    block> so formatiert werden.
  </explain>

  <\explain|<explain-macro|block*|table> <strong|= block>*>
    Zentrierte Tabelle mit sichtbaren Standard-Zell-Rändern (Gitter,
    <verbatim|1ln>). \ Tabellen können mit dem Menü
    <menu|Insert|Table|Centered block> so formatiert werden.
  </explain>

  Folgende Tags haben keine Argumente:

  <explain|<explain-macro|TeXmacs>|Das <TeXmacs>-Logo.>

  <explain|<explain-macro|TeXmacs-version>|Diese Version von <TeXmacs>,
  (z.B.: <TeXmacs-version>).>

  <explain|<explain-macro|TeX>|Das <TeX>-Logo.>

  <explain|<explain-macro|LaTeX>|Das <LaTeX>-Logo.>

  <\explain|<explain-macro|hflush>, <explain-macro|left-flush>,
  <explain-macro|right-flush>>
    Wird von Entwicklern gebraucht, um bei der Kontext-Entwicklung
    Randmarkierungen/Hilfslinien anzupassen.
  </explain>

  <\explain|<explain-macro|hrule>>
    Ein horizontaler Strich auf der Basislinie wie der da unten:

    <hrule>
  </explain>

  Die folgenden Tags haben ein oder mehrere Argumente:

  <\explain|<explain-macro|overline|content> <strong|= <localize|overline>>>
    <overline|Versieht den Inhalt mit einem Überstrich, der mehrere Zeilen
    überstreichen kann. Kann mit <menu|Insert|Presentation tag|Overline>
    eingefügt werden.>

    \;
  </explain>

  <\explain|<explain-macro|underline|content> <strong|=
  <localize|underline>>>
    <underline|Versieht den Inhalt mit einem Unterstrich, der mehrere Zeilen
    überstreichen kann. Kann mit <menu|Insert|Presentation tag|Underline>
    eingefügt werden.>

    \;
  </explain>

  <\explain|<explain-macro|fold|summary|body> <strong|= <localize|fold>>>
    <src-arg|summary> wird auf dem Bildschirm dargestellt und <src-arg|body>
    verborgen. Mit <menu|Insert|Switches|Unfold> kann der versteckte Text
    <src-arg|body> sichtbar gemacht werden. \ Mit der Tastenkombination
    <key|M-A-Bild_nachunten> kann das gleiche erreicht werden. Ganz
    entsprechend verbirgt \ <key|M-A-Bild_nachunten> den Text.
    <src-arg|summary> ist meist eine kurze Überschrift oder Feststellung,
    <src-arg|body> eine kurzer Text oder eine Erläuterung. Wird meist in
    Präsentationen benutzt.
  </explain>

  <\explain|<explain-macro|unfold|summary|body> <strong|= <localize|unfold>>>
    Die Darstellung von <explain-macro|fold|summary|body>, in der der gesamte
    Text (<src-arg|summary> und <src-arg|body>) gezeigt wird. Das zweite
    Argument <src-arg|body> kann mit <menu|Insert|Switches|Fold> unsichtbar
    gemacht werden. Genaueres siehe oben.
  </explain>

  <\explain|<explain-macro|switch|current|alternatives> <strong|=
  <localize|switch>>>
    Mehrere Textebenen (Text-Alternativen), von denen jeweils nur eine
    gezeigt wird. <src-arg|current> ist die sichtbare Ebene.
    <src-arg|alternative>s hat die Form: <verbatim|<tuple|<tmarker>|<inactive|<compound|document|alt2>>|<inactive|<compound|document|alt3>>>>,
    wobei angenommen wurde, dass <src-arg|current> die erste Alternative ist.
    Die Tastenkombination <key|M-A-nachoben> schaltet zur ersten Ebene,
    \ <key|M-A-nachunten> \ zur letzten, <key|M-A-left> um eine nach vorne
    und \ <key|M-A-right> um eine nach hinten. Es können auch die
    Menübefehle <menu|Insert|Switches|Switch to previous> bis
    \ <menu|Insert|Switches|Switch to last> benutzt werden.\ 

    Zur Erzeugung einer ersten Ebene dient <menu|Insert|Switches|Switch>, die
    dann mit Text versehen werden kann. Wenn man in der Ebene ist, lassen
    sich weitere mit <menu|Insert|Switches|Add switch before> und
    \ <menu|Insert|Switches|Add switch after> oder mit
    <menu|Insert|Switches|Remove this switch> entfernen.
  </explain>

  <\explain>
    <explain-macro|superpose|text1|text2> <strong|= <localize|superpose>>\ 
  <|explain>
    <src-arg|text1>wird von <src-arg|text2> überlagert, z.B.
    \R<inactive|<superpose|<with|font-family|tt| OOO
    >|<with|font-family|tt|+++++>>>`` gibt \R<superpose|<with|font-family|tt|
    OOO >|<with|font-family|tt|+++++>>``.
  </explain>

  <\explain|<explain-macro|phantom|content> <strong|= <localize|phantom>>>
    Dieses Makro setzt anstelle des Inhaltes <src-arg|content> einen
    gleichlangen Leerraum - der Inhalt wird also nicht gezeigt oder gedruckt,
    ergibt \R<inactive*|<phantom|phantom>>`` das: \R<phantom|phantom>''.
  </explain>

  <\explain|<explain-macro|set-header|header-text> <strong|=
  <localize|set-header>>>
    Ein Makro, mit dem die Kopfzeile permanent geändert werden kann.
    Bestimmte Konstrukte in Basis-Stil-Definitionen haben Vorrang diesem
    Befehl.
  </explain>

  <\explain|<explain-macro|set-footer|footer-text> <strong|=
  <localize|set-footer>>>
    Ein Makro, mit dem die Fuÿ-Zeile permanent geändert werden kann.
    Bestimmte Konstrukte in Basis-Stil-Definitionen haben Vorrang diesem
    Befehl.
  </explain>

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
    <associate|src-close|long>
  </collection>
</initial>