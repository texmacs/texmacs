<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Konvertierer und neue Datenformate erstellen>

  Mir der <name|Guile>/<name|Scheme>-Sprache kann man, neue Datenformate und
  Konvertierer zu <TeXmacs> als Module hinzufügen. Normalerweise werden die
  zusätzlichen Formate und Konvertierer in ihrer persönlichen
  Initialisierungs-Datei <verbatim|~/.TeXmacs/progs/my-init-texmacs.scm>
  definiert oder in einem speziellen Plugin. Einige Beispiele finden Sie in
  einem Unterverzeichnis <verbatim|progs/convert> ihres
  <TeXmacs>-Verzeichnisses, wie z.B. <hlink|init-html.scm|$TEXMACS_PATH/progs/convert/html/init-html.scm>.

  <paragraph*|neue formate definieren>

  Ein neues Format kann mit dem Befehl

  <\scheme-fragment>
    (define-format <em|format>

    \ \ (:name <em|format-name>)

    \ \ <em|options>)
  </scheme-fragment>

  erzeugt werden. <verbatim|<em|format>> ist ein Symbol, dass für das Format
  steht und <verbatim|<em|format-name>> eine Zeichenkette, die in Menüs
  benutzt werden kann. Tatsächlich gibt es ein Datenformat meist in mehreren
  Varianten: ein Format <verbatim|<em|format>-file> für Dateien, ein Format
  <verbatim|<em|format>-document> für ganze Dokumente und ein Format
  <verbatim|<em|format>-snippet> für kurze Zeichenketten, wie z.B. Auswahlen
  (selections), und schlieÿlich <verbatim|<em|format>-object> für die
  bevorzugte interne <name|Scheme>-Repräsentation bei Konversionen (dies ist
  Parser-Variante des Formats). Konvertierer von <verbatim|<em|format>-file>
  nach <verbatim|<em|format>-document> und umgekehrt werden automatisch
  erzeugt.

  Der Anwender kann zusätzliche Optionen zur automatischen Erkennung von
  Formaten mit Hilfe von Datei-Suffixen oder -Inhalten spezifizieren. Z.B.
  können die erlaubten Suffixe eines Daten-Formats mit der Voreinstellung als
  erstem durch

  <\scheme-fragment>
    (:suffix <em|default-suffix> <em|other-suffix-1> ... <em|other-suffix-n>)
  </scheme-fragment>

  angegeben werden. Eine (heuristische) Routine, um festzustellen, ob ein
  Dokument zu einem bestimmten Format gehört, kann mit

  <\scheme-fragment>
    (:recognize <em|predicate>)

    (:must-recognize <em|predicate>)
  </scheme-fragment>

  erreicht werden. Im ersten Fall hat die Suffix-Erkennung Vorrang vor der
  heuristischen Erkennung. Im zweiten Fall ist nur die heuristische Erkennung
  durch das Prädikat, predicate, maÿgeblich.

  <paragraph*|neue konvertierer erzeugen>

  Neue Konvertierer können mit

  <\scheme-fragment>
    (converter <em|from> <em|to>

    \ \ <em|options>)
  </scheme-fragment>

  erzeugt werden. Der eigentliche Konvertierer wird durch eine der folgenden
  Optionen spezifiziert:

  <\scheme-fragment>
    (:function <em|converter>)

    (:function-with-options <em|converter-with-options>)

    (:shell <em|prog> <em|prog-pre-args> from <em|progs-infix-args> to
    <em|prog-post-args>)
  </scheme-fragment>

  Im ersten Fall ist der Konvertierer <verbatim|<em|converter>> eine Routine
  die ein Objekt im Daten-Format <verbatim|<em|from>> \ übernimmt und ein
  Objekt im Daten-Format <verbatim|<em|to>> zurückgibt. Im zweiten Fall
  übernimmt der <verbatim|<em|converter>> eine assoziative Liste als zweites
  Argument und Optionen für den Konvertierer. Im letzten Fall wird ein
  <em|shell>-Befehl angegeben, der die Konvertierung zwischen den beiden
  Datei-Formaten durchführt. Der Konvertierer wird dann und nur dann
  aktiviert, wenn das Programm <verbatim|<em|prog>> gefunden wird.
  Hilfsdateien können automatisch erzeugt und wieder gelöscht werden.

  <TeXmacs> berechnet automatisch die transitive \RClosure'' aller
  Konvertierer, indem einen \Rkürzesten Pfad Algorithmus'' benutzt. Mit
  anderen Worten, wenn es einen Konvertierer für <with|mode|math|x> nach
  <with|mode|math|y> gibt und einen Konvertierer von <with|mode|math|y> nach
  <with|mode|math|z>, dann hat man auch einen von <with|mode|math|x> nach
  <with|mode|math|z>. Es für jede Konvertierung kann eine \RStrafe/Abstand
  zwischen den Formaten`` mit

  <\scheme-fragment>
    (:penalty <em|floating-point-distance>)
  </scheme-fragment>

  angeben werden, um so Hinweise zum automatischen Finden eines optimalen
  Weges für die Konvertierung zu geben.

  Weitere Optionen für Konvertierer sind:

  <\scheme-fragment>
    (:require <em|cond>)

    (:option <em|option> <em|default-value>)
  </scheme-fragment>

  Die erste Option spezifiziert eine Bedingung, die erfüllt sein muss, damit
  der Konvertierer benutzt werden kann. Diese Option sollte als erste oder
  zweite Option spezifiziert werden und immer nach der <verbatim|:penalty>
  Option. Die <verbatim|:option> Option spezifiziert eine Option für den
  Konvertierer, mit einer Voreinstellung. Diese Option wird automatisch an
  alle Konvertierer mit Optionen weitergegeben.

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