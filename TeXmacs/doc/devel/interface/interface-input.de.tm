<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematische und angepasste Eingabe>

  Das <TeXmacs> Meta-Format ermöglicht, dass Anwendungen strukturierten Text
  wie z.B. mathematische Formeln an <TeXmacs> übergeben können. Ganz
  entsprechend können Sie allgemeine <TeXmacs>-Inhalte als Eingabe an
  Anwendungen schicken. Vorgabemäÿig wird nur der Textinhalt an die Anwendung
  in Form einer Zeichenkette geschickt. Dabei werden alle ASCII-Zeichen im
  Bereich 0 - 31 ignoriert, auÿer <verbatim|'\\t'> und <verbatim|'\\n'>, die
  als Leerstellen interpretiert werden. Es gibt zwei Methoden, um die Art und
  Weise, wie die Eingabe an die Anwendung geschickt wird, zu beeinflussen.
  Die Konfigurations-Option

  <\scheme-fragment>
    (:serializer ,<em|routine>)
  </scheme-fragment>

  spezifiziert eine <with|font-shape|small-caps|Scheme>-Funktion um
  <TeXmacs>-Bäume in eine Zeichenkette als Eingabe für die Anwendung
  umzuformen. Eine solche Umformung hat Vorrang vor der voreingestellten
  Methode. Auf diese Weise können zum Beispiel mehrzeilige Eingaben in
  besonderer Weise behandelt werden oder <TeXmacs>-Bäume transformiert
  werden.

  Die <verbatim|:serialize> Option ist sehr mächtig. Sie ist aber auch eine
  sehr abstrakte Weise der Anpassung von Eingaben in eine Anwendung. Man ist
  gezwungen, eine komplette Eingabe-Transformations-Funktion zu schreiben.
  Häufig will der Anwender aber nur kleinere Anpassungen durchführen, z.B.
  zwei-dimensionale mathematische Eingabe etwa der Form I
  <no-break><with|mode|math|<frac|a|b>> umformen in die gebräuchlichere Form
  <verbatim|((a)/(b))>. Deshalb gibt es noch einen zweiten Weg. Man benutzt
  den Befehl

  <\scheme-fragment>
    \ (plugin-input-converters <em|myplugin>

    \ \ \ <em|rules>)
  </scheme-fragment>

  Dieser Befehl legt Eingabe--Konvertierungsregeln in Bezug auf
  <verbatim|<em|myplugin>> für \ \Rmathematische Eingabe'' fest. Dafür hat
  <TeXmacs> bereits vernünftig erscheinende Vorgaben. Die Regeln haben ein
  von zwei möglichen Formen:

  <\description>
    <item*|Transformationsregel für Blätter>

    Mit den beiden Zeichenketten <verbatim|<em|symbol>> und
    <verbatim|<em|conversion>> besagt die Regel

    <\scheme-fragment>
      (<verbatim|<em|symbol>> <verbatim|<em|conversion>>)
    </scheme-fragment>

    dass das <TeXmacs> Symbol <verbatim|<em|symbol>> durch die Zeichenkette
    <verbatim|<em|conversion>> ersetzt werden soll.

    <item*|Transformationsregeln für Tags>

    Mit einen Symbol <verbatim|<em|tag>> und einer <value|scheme>-Funktion
    <verbatim|<em|routine>>, besagt die Regel

    <\scheme-fragment>
      (<em|tag> <em|routine>)
    </scheme-fragment>

    dass <verbatim|<em|routine>> anstelle von <verbatim|<em|tag>> zu benutzen
    ist. Diese <value|scheme>-Funktion sollte eine Zeichenkette an die
    Standard-Ausgabe schicken. Die <value|scheme>-Funktion
    <scheme-code|plugin-input> kann zur rekursiven Transformation der
    Argumente des Tag benutzt werden.
  </description>

  <paragraph*|Das <verbatim|input> plugin>

  Das Beispiel <verbatim|input> demonstriert, wie man mathematische Eingabe
  anpasst und verwendet. Es besteht aus den folgenden Dateien:

  <\verbatim>
    \ \ \ \ <example-plugin-link|input/Makefile>

    \ \ \ \ <example-plugin-link|input/packages/session/input.ts>

    \ \ \ \ <example-plugin-link|input/progs/init-input.scm>

    \ \ \ \ <example-plugin-link|input/progs/input-input.scm>

    \ \ \ \ <example-plugin-link|input/src/input.cpp>
  </verbatim>

  Der <value|scheme>-Code zur Konfiguration in der Datei
  <verbatim|init-input.scm> ist der folgende

  <\scheme-fragment>
    (plugin-configure input

    \ \ (:require (url-exists-in-path? "input.bin"))

    \ \ (:initialize (input-initialize))

    \ \ (:launch "input.bin")

    \ \ (:session "Input"))
  </scheme-fragment>

  Hier ist <verbatim|input-initialize> eine Initialisierungs-Routine, die auf
  einfache Weise neue Konversions-Routinen einführt.

  <\scheme-fragment>
    (define (input-initialize)

    \ \ (import-from (texmacs plugin plugin-convert))

    \ \ (lazy-input-converter (input-input) input))
  </scheme-fragment>

  Mit anderen Worten, das Modul <verbatim|input-input.scm> wird nur dann
  geladen, wenn wir explizit verlangen, dass eine Konversion durchgeführt
  werden soll. Die Konversionsregeln in <verbatim|input-input.scm> lauten so

  <\scheme-fragment>
    (plugin-input-converters input

    \ \ (frac input-input-frac)

    \ \ (special input-input-special)

    \ \ ("\<less\>vee\<gtr\>" "\|\|")

    \ \ ("\<less\>wedge\<gtr\>" "&&"))
  </scheme-fragment>

  Das führt dazu, dass <with|mode|math|\<vee\>> und
  <with|mode|math|\<wedge\>> zu <verbatim|\|\|> und <verbatim|&&>
  umgeschrieben werden. Brüche <with|mode|math|<frac|a|b>> werden zu
  <verbatim|((a):(b))> mit

  <\scheme-fragment>
    (define (input-input-frac t)

    \ \ (display "((")

    \ \ (plugin-input (car t))

    \ \ (display "):(")

    \ \ (plugin-input (cadr t))

    \ \ (display "))"))
  </scheme-fragment>

  In den zusätzlichen Stil <verbatim|input.ts> definieren wir ein
  zusätzliches Makro <markup|special>:

  <\tm-fragment>
    <with|preamble|true|<inactive|<assign|special|<macro|body|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
    green>|<table|<row|<cell|<arg|body>>>>>>>>>>
  </tm-fragment>

  Dieser Tag wird mit der speziellen Konversionsregel umgeschrieben

  <\scheme-fragment>
    (define (input-input-special t)

    \ \ (display "[[[SPECIAL:")

    \ \ (plugin-input (car t))

    \ \ (display "]]]"))
  </scheme-fragment>

  Der folgende <value|cpp> Code in <verbatim|input.cpp>\ 

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "command:(session-use-math-input #t)"

    \ \ \ \ \ \<less\>\<less\> DATA_END;

    cout \<less\>\<less\> "Convert mathematical input into plain text";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  versetzt die Sitzung automatisch in den mathematischen Eingabe-Modus und
  gibt den Begrüÿungstext aus. In der Hauptschleife begnügen wir uns damit,
  die Eingabe als Ausgabe zurückzuschicken:

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

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
  </collection>
</initial>