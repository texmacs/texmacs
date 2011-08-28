<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Makro-Konstrukte>

  Makros können zur Definition von neuen Befehlen, Konstrukten, Tags benutzt
  werden und zur Konstruktion von abstrakten Prozeduren in Stil-Definitionen.

  Ältere Versionen von <TeXmacs> machten einen Unterschied zwischen Makros,
  bei denen alle Kinder erreichbar waren und Funktionen, die Kinder nicht
  erreichen konnten. Das jetzige <TeXmacs> kennt nur Makros: die
  Erreichbarkeit von Kindern wird heuristisch ermittelt und kann durch
  \ <markup|drd-props> gesteuert werden.<htab|5mm>

  <\explain>
    <explain-macro|macro|var-1|<with|mode|math|\<cdots\>>|var-n|body><explain-synopsis|Makro
    mit festgelegter Argumentanzahl>
  <|explain>
    Dieses Konstrukt erzeugt ein Makro (das <TeXmacs>-Analogon eines
    <with|mode|math|\<lambda\>>-Ausdrucks) mit <with|mode|math|n> Argumenten
    mit den Namen der Zeichenkette <src-arg|var-1> bis <src-arg|var-n>.

    Neue Tags werden definiert, indem die Makros im Kontext gespeichert
    werden. Meistens werden Makros global gespeichert mit <markup|assign>,
    aber manchmal ist es besser, einem Tag lokal zu definieren mit
    <markup|with>. Beispielsweise definieren nummerierte Kontexte die
    Variable <markup|item> lokal.

    <\example>
      Definition einer <markup|Abkürzung>

      <\tm-fragment>
        <inactive*|<assign|Abkürzung|<macro|x|<group|<arg|x>>>>>
      </tm-fragment>
    </example>

    Wenn man ein Makro <markup|macro> im Kontext speichert, wird ein Tag mit
    einer vorgegebenen Anzahl von Argumenten definiert.
  </explain>

  <\explain>
    <explain-macro|arg|var|index-1|<with|mode|math|\<cdots\>>|index-n><explain-synopsis|Makro-Argumente
    ermitteln>
  <|explain>
    Dieses Konstrukt dient dazu, Werte von Variablen innerhalb eines
    Makro-Rumpfes zu ermitteln. Z.B. evaluiert <explain-macro|arg|var> zu dem
    aktuellen Wert von <src-arg|var> (Zeichenkette). Natürlich muss dieses
    Argument davor in dem Makro, <markup|macro>, definiert sein, das den
    <markup|arg>-Konstrukt enthält.

    Dieses Konstrukt ähnelt <markup|value>, verhält sich aber in einigen
    wichtigen Punkten anders:

    <\itemize>
      <item>Der Argument-Namensraum unterscheidet sich vom Kontext, daher
      liefern <explain-macro|arg|var> und <explain-macro|value|var> in der
      Regel unterschiedliche Werte. (Darauf sollte man sich aber nicht
      verlassen.)

      <item>Der Wert von <markup|arg> verbleibt an der Stelle des
      Makro-Arguments in dem Dokument-Baum. Deshalb können die Argumente
      eines Makro-Konstrukts im aktiven Zustand geändert werden.
    </itemize>

    Wenn mehr als Argument vorhanden ist, dann expandiert
    <explain-macro|arg|var|index-1|<with|mode|math|\<cdots\>>|index-n> zu
    einem Unterbaus des Arguments <src-arg|var>. Der Wert von <src-arg|var>
    muss ein <translate|compound|english|german>, compound, sein, also keine
    Zeichenkette. Alle Argumente, <src-arg|var> bis <src-arg|index-n>, müssen
    zu positiven Ganzzahlen evaluieren und den Pfad zu einem Unterbaus des
    Makro-Arguments zeigen.\ 
  </explain>

  <\explain>
    <explain-macro|xmacro|var|body><explain-synopsis|Makro mit beliebiger
    Argumentanzahl>
  <|explain>
    Dieses Konstrukt definiert ein Makro (das <TeXmacs>-Analogon eines
    <with|mode|math|\<lambda\>>-Ausdrucks), welches eine beliebige Anzahl von
    Argumenten annehmen kann. Die Argumente werden in der Makro-Variablen mit
    Namen <src-arg|var> (einer Zeichenkette) während der Evaluierung des
    Rumpfes, <src-arg|body>, gespeichert. Der Wert des <with|mode|math|i>-ten
    Arguments kann dann mit <explain-macro|arg|var|i> erhalten werden.
  </explain>

  <\explain>
    <explain-macro|map-args|foo|root|var>

    <explain-macro|map-args|foo|root|var|first>

    <explain-macro|map-args|foo|root|var|first|last><explain-synopsis|ein
    Makro auf alle Kinder eines Baums anwenden>
  <|explain>
    Dieses Konstrukt evaluiert ein Baum, dessen Wurzel <src-arg|root> ist und
    dessen Kinder durch Anwendung eines Makros <src-arg|foo> auf die Kinder
    des Makro-Arguments mit Namen <src-arg|var> entstehen.

    Entsprechend der Vorgabe wird <src-arg|foo> auf auf alle Kinder
    angewandt. Wenn <src-arg|first> spezifiziert wurde, dann wird mit dem
    <with|mode|math|i>-ten Kind begonnen, wenn <with|mode|math|i> das
    Resultat der Evaluierung von <src-arg|first> ist. Mit <src-arg|last> wird
    die Berechnung beim <with|mode|math|j>-ten Kind von <src-arg|var>, wobei
    das j-te Kind nicht einbezogen wird, wenn <with|mode|math|j> das Ergebnis
    der Evaluierung von <src-arg|last> ist. Die Dimension des Baumes ist also
    <with|mode|math|j-i>.

    Anders ausgedrückt: <markup|map-args> wendet <src-arg|foo> auf alle
    Unter-Bäume oder auf ein Intervall von Unter-Bäumen an (falls
    <src-arg|first> und <src-arg|last> angegeben wurde) und sammelt das
    Ergebnis in einem Baum mit Namen <src-arg|root>.

    <markup|map-args> ist das Analogon zur <value|scheme>-Funktion
    <verbatim|map>. Weil <TeXmacs> aber Bäume mit Labeln verwendet, muss das
    Label für das Ergebnis mit übergeben werden.

    <\example>
      Komma-separierte Listen.

      Das <markup|comma-separated>-Makro hat eine Dimension, auch wenn dies
      keinen Sinn macht bei der Dimension 0. Der Schriftsatz erfolgt so, dass
      seine Argumente durch Kommata getrennt werden.

      <\tm-fragment>
        <inactive*|<assign|comma-extra|<macro|x|, <arg|x>>>>

        <inactive*|<assign|comma-separated|<xmacro|args|<style-with|src-compact|none|<arg|args|0><map-args|comma-extra|concat|args|1>>>>>
      </tm-fragment>
    </example>
  </explain>

  <\explain>
    <explain-macro|eval-args|var><explain-synopsis|Makro mit beliebiger
    Dimension>
  <|explain>
    Dieses Konstrukt evaluiert zu einem Baum mit dem dem gleichen Label wie
    die Expansion des Arguments <src-arg|var>, dessen Unter-Bäume das
    Resultat der Evaluierung der Unter-Bäume von <src-arg|var> sind.
  </explain>

  <\explain>
    <explain-macro|compound|foo|arg-1|<with|mode|math|\<cdots\>>|arg-n><explain-synopsis|unbenanntes
    Makro>
  <|explain>
    Dieses Konstrukt ist besonders nützlich, wenn es darum geht, Makros zu
    expandieren, die selbst das Ergebnis einer Berechnung sind: Er wendet das
    Makro, welches das Resultat der Evaluierung von <src-arg|foo> ist, auf
    die Argumente <src-arg|arg-1> bis <src-arg|arg-n> an. <markup|compound>
    wird vor allem verwendet, wenn einem Makro ein anderes Makro als Argument
    übergeben wird, welches es dann beim Vorliegen bestimmter Bedingungen
    verwendet.

    Allerdings kann in der derzeitigen Implementierung <src-arg|foo> entweder
    zu einem Makro evaluieren oder zu einer Zeichenkette, die dann dem Makro
    ihren Namen gibt. Wir empfehlen Vorsicht bei der zweiten Variante.

    <\example>
      Lambda-Programmierung mit Makros.

      Im unten stehenden Code erwartet <explain-macro|filter|pred|t> ein
      Makro <src-arg|pred> und ein Tupel <src-arg|t> als Argumente und
      liefert ein Tupel zurück, das aus denjenigen Elemente von <src-arg|t>
      besteht, für die <src-arg|pred> den Wert <verbatim|true> ergibt.

      <\tm-fragment>
        <inactive*|<assign|filter|<macro|pred|t|<style-with|src-compact|none|<if|<equal|<length|<arg|t>>|0>|<tuple>|<style-with|src-compact|none|<merge|<style-with|src-compact|none|<if|<compound|<arg|pred>|<look-up|<arg|t>|0>>|<tuple|<look-up|<arg|t>|0>>|<tuple>>>|<filter|<arg|pred>|<range|<arg|t>|1|<length|<arg|t>>>>>>>>>>>
      </tm-fragment>

      Das lässt sich z.B. in einem Makro <explain-macro|evens|t> verwenden,
      das aus ein Tupel von Ganzzahlen <src-arg|t> die geraden Zahlen
      extrahiert.

      <\tm-fragment>
        <inactive*|<assign|evens|<macro|t|<filter|<macro|x|<equal|<mod|<arg|x>|2>|0>>|<arg|t>>>>>
      </tm-fragment>
    </example>
  </explain>

  <\explain>
    <explain-macro|drd-props|var|prop-1|val-1|<with|mode|math|\<cdots\>>|prop-n|val-n><explain-synopsis|setze
    <abbr|D.R.D.>-Eigenschaften>
  <|explain>
    Die Dimension und die Erreichbarkeit von Kindern wird normalerweise
    heuristisch ermittelt. Das <markup|drd-props>-Konstrukt ändert die
    Voreinstellung für die Kontextvariable (normalerweise ein Makro) mit dem
    Namen <src-arg|var>. Zur Zeit werden die folgenden Paarungen unterstützt:

    <\description-dash>
      <item*|(arity, <with|mode|math|n>)<verbatim|>>(Dimension, n) setzt die
      Dimension auf einen festen Wert <with|mode|math|n> (Ganzzahl).

      <item*|(accessible, all)>(erreichbar, alle) Das Konstrukt kann dann im
      Editor nicht deaktiviert werden. Kinder, die nicht erreichbar sind,
      können nicht editiert werden.

      <item*|(accessible, none)>Verhindert, dass der Cursor innerhalb eines
      Konstrukts positioniert wird, solange das Konstrukt aktiv ist. Kinder
      können also nur editiert werden, wenn das Konstrukt deaktiviert wird.
    </description-dash>
  </explain>

  <\explain>
    <explain-macro|get-label|expression><explain-synopsis|Label eines
    Ausdrucks>
  <|explain>
    Gibt den Label des Baumes zurück, wenn der Ausdruck,
    <src-arg|expression>, evaluiert wird.
  </explain>

  <\explain>
    <explain-macro|get-arity|expression><explain-synopsis|Dimension eines
    Ausdrucks>
  <|explain>
    Gibt die Dimension des Baumes zurück, wenn der Ausdruck,
    <src-arg|expression>, evaluiert wird.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

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