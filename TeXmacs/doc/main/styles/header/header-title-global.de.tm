<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Die globale Darstellung von Titeln anpassen>

  Abhängig von der Art der Attribute haben komplexe Titel oft gleichzeitig
  mehrere verschiedene Darstellungs-Stile. Genauer gesagt, ein Titel besteht
  normalerweise aus folgenden Teilen:

  <\itemize>
    <item>Einem gut sichtbaren besonders hervorgehobenen Teil ganz oben auf
    der Seite.

    <item>Zusätzliche Anmerkungen, die in der Fuÿ-Zeile erscheinen sollen.

    <item>Einen Teil der möglicherweise nicht sichtbar sein soll, wie
    laufende Titel oder Autoren.

    <item>Ein zurückgestellter Teil, der nur in der Zusammenfassung
    (abstract) dargestellt werden soll.
  </itemize>

  Sind mehrere Autoren vorhanden, dann \ kann der jeder individuelle auch
  einen Hauptteil haben, de im eigentlichen Titel dargestellt werden soll und
  zusätzliche Anmerkungen, die als Fuÿnote erscheinen sollen. Auÿerdem ändern
  sich oft das Layout mit dem Autor.

  Der Mechanismus, der in <TeXmacs> Titel zur Darstellung bringt, besitzt
  daher eine Anzahl von Makros die die notwendigen Informationen für die
  einzelnen Teile heraus filtert. Dieser Prozess kann auÿerdem
  Sortiervorgänge enthalten, wie z.B. den Autor vor das Datum stellen oder
  umgekehrt. In einer zweien Stufe wird dann die herausgefilterte Information
  an Darstellungs-Makros weitergereicht.

  <underline|Die folgenden Makros dienen zur Extraktion von
  Titel-Informationen:>

  <\explain>
    <explain-macro|doc-data-main|data-1|<with|mode|math|\<cdots\>>|data-n>

    <explain-macro|doc-data-main*|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die im eigentlichen Titel
    erscheinen sollen. Die <markup|doc-data-main*> Variante wird benötigt,
    wenn mehr als ein Autor vorhanden ist.
  </explain>

  <\explain>
    <explain-macro|doc-data-note|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die in der Fuÿnote erscheinen
    sollen.
  </explain>

  <\explain>
    <explain-macro|doc-data-abstract|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die in der Zusammenfassung
    erscheinen sollen.
  </explain>

  <\explain>
    <explain-macro|doc-data-hidden|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die möglicherweise nicht
    sichtbar sein sollen oder die keinesfalls sichtbar erscheinen sollen.
  </explain>

  <underline|In ähnlicher Weise extrahieren die folgenden Makros
  Autor-Informationen:>

  <\explain>
    <explain-macro|doc-author-main|<with|font-shape|right|<explain-macro|doc-author-data|data-1|<with|mode|math|\<cdots\>>|data-n>>>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die in der Fuÿnote erscheinen
    sollen.
  </explain>

  <\explain>
    <explain-macro|doc-author-note|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    Dieses Makro sammelt und sortiert Daten, die in der Fuÿnote erscheinen
    sollen.
  </explain>

  Jedes der oben genannten Makros liefert ein <markup|document>-Konstrukt
  zurück mit den gesammelten Daten als Kinder. Z.B.\ 

  <\tm-fragment>
    <inactive*|<style-with|src-compact|none|<doc-author-main|<author-address|Somewhere
    in Africa>|<author-name|The big GNU>|<author-note|Very hairy indeed!>>>>
  </tm-fragment>

  gibt typischerweise

  <\tm-fragment>
    <with|src-special|raw|<inactive*|<\style-with|src-compact|none>
      <author-address|Somewhere in Africa>

      <author-name|The big GNU>
    </style-with>>>
  </tm-fragment>

  zurück. Die einzige Ausnahmen ist <markup|doc-data-hidden>, welches ein
  <markup|concat>-Konstrukt zurückgibt.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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