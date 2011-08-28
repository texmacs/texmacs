<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematische Formeln>

  Um mathematische Formeln eingeben zu können, müssen Sie erst in den
  Mathematik-Modus wechseln. Das ist eine spezielle Text-Eigenschaft, die in
  Objekten aktiviert wird, die mit den Befehlen in dem Menü
  <menu|Insert|Mathematics> aktiviert wird. Wenn Sie im Mathematik-Modus sind
  und <menu|Edit|Preferences|View|Context dependent icons> aktiviert haben,
  sehen Sie eine gröÿere Anzahl von Icons, die Ihnen Zugang zu den meisten
  mathematischen Optionen verschaffen.

  <\description>
    <item*|<localize|Formula> <key|$>>wird benutzt, wenn in Flieÿtext
    kleinere mathematische Formeln eingefügt werden sollen. Dazu dient der
    Menübefehl <menu|Insert|Mathematics|Formula>.

    Formeln werden in einem speziellen Schriftsatz gesetzt, damit sie nicht
    zu viel vertikalen Platz einnehmen und dennoch lesbar bleiben.
    Beispielsweise werden obere und untere Schranken deshalb immer links
    platziert und nicht ober- bzw. unterhalb. Das kann aber erzwungen werden,
    indem der Stil für eigenständige Formeln aktiviert wird. Dazu dient der
    Menübefehl <menu|Format|Formula style|On>. Für Formeln im Flieÿtext ist
    dieser Stil vorgabemäÿig deaktiviert.

    <item*|<localize|Equation> <shortcut|(make-equation*)>>ist die Struktur für eigenständige
    mathematische Formeln, die in einen eigenen Block gesetzt werden. Sie
    werden mit dem Menübefehl <menu|Insert|Mathematics|Equation> eingefügt.

    <item*|<localize|Equations> <shortcut|(make-eqnarray*)>>erzeugt einen Block für mehrere
    mathematische Ausdrücke, eine dreispaltige Tabelle, <markup|eqnarray*>.
    Zur Erzeugung dient der Menübefehl <menu|Insert|Mathematics|Equations>.
    Siehe auch <hyper-link|Tabellen erzeugen|../table/man-create-table.en.tm>.

    Dieser Kontext wurde für mehrstufige Berechnungen konzipiert, bei denen
    eine Relation der anderen folgt. Die erste Spalte ist für die linke
    Seite, die mittlere für das Relationssymbol, z.B. \R='', die rechte
    Spalte für die rechte Seite der Relation.
  </description>

  Im Mathematik-Modus gibt es spezielle Befehle und Kurzbefehle, um
  mathematische Formeln einzugeben. Z.B. kann die Sondertaste <prefix|M-A-> zur
  Eingabe griechischer Buchstaben benutzt werden. Denken Sie bitte daran,
  dass <prefix|M-A-> äquivalent zu <prefix|math:greek> ist und auch mit <key|escape
  escape escape> oder <prefix|A-C-> eingegeben werden kann.

  Der Editor favorisiert mathematische Eingabe mit einer bestimmten
  Bedeutung. Diese Funktion soll in Zukunft weiter ausgebaut werden, denn es
  erleichtert die Kommunikation mit Computer-Algebra-Paketen. Momentan
  sollten Sie das Multiplikationssymbol <key|*> zwischen Symbolen explizit
  eingeben, denn die Eingabe von \ <key|a b> wird vom Editor als
  \R<with|mode|math|mode|text|ab>`` und nicht als \R<with|mode|math|a*b>``
  interpretiert.

  <\traverse>
    <branch|Die wichtigsten mathematischen
    Konstrukte|keyboard/man-main.de.tm>

    <branch|Mathematische Symbole eingeben|keyboard/man-symbols.de.tm>

    <branch|Groÿe Operatorsymbole|keyboard/man-big.de.tm>

    <branch|Groÿe Klammern|keyboard/man-large.de.tm>

    <branch|Breite mathematische Akzente|keyboard/man-wide.de.tm>
  </traverse>

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