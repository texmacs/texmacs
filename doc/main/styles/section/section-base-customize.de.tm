<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Abschnitt-Kontexte anpassen>

  Die <tmdtd|section-base> <abbr|D.T.D.> enthält auÿerdem Befehle zur
  Darstellung von Abschnitten und zur Steuerung des Verhaltens von
  Abschnitten.

  <underline|Die folgenden beiden Befehle wirken auf alle Abschnitte:>

  <\explain|<explain-macro|sectional-sep>>
    Ein Makro zur Festlegung des Trennzeichens zwischen der Nummer eines
    Abschnitts und seinem Titel. Als Vorgabe verwenden wir zwei Leerzeichen.
  </explain>

  <\explain|<explain-macro|sectional-short-style>>
    Ein Prädikat, das abfragt, ob beabsichtigt ist, ein als kurzes oder
    langes Dokument zu schreiben. Wenn das Prädikat
    <markup|sectional-short-style> wahr, <verbatim|true>, zurückgibt, dann
    werden Anhänge, Bibliographien usw. als spezielle Arten von Abschnitten
    behandelt. Andernfalls sind sie spezielle Kapitel.
  </explain>

  Für jeden Abschnitts-Befehl <markup|<em|x>>, gibt es die folgenden Befehle
  zur Anpassung:

  <\explain|<explain-macro|<em|x>-title|title>>
    Ein Makro zur Darstellung des unnummerierten Abschnitts-Titels.
  </explain>

  <\explain|<explain-macro|<em|x>-numbered-title|title>>
    Ein Makro zur Darstellung des nummerierten Abschnitts-Titels.
  </explain>

  <\explain|<explain-macro|<em|x>-display-numbers>>
    Ein Prädikat welches spezifiziert, ob Nummern wirklich auf dem Bildschirm
    oder im Druck dargestellt werden sollen. Im Fall eines Absatzes,
    <markup|paragraph>, evaluiert das Makro zu \Rfalse`` und, obwohl
    <markup|<em|x>-numbered-title> den nummerierten Titel in der Tat
    <em|darstellt>, werden die Absatz-Titel unnummeriert dargestellt, denn
    das Haupt-Makro <markup|<em|x>> ruft in diesem Fall <markup|<em|x>-title>
    und nicht <markup|<em|x>-<no-break>numbered-title> auf.
  </explain>

  <\explain|<explain-macro|<em|x>-sep>>
    Ein Makro zur Festlegung des Trennzeichens zwischen der Nummer eines
    Abschnitts und seinem Titel. Als Vorgabe wird <markup|sectional-sep>
    benutzt.
  </explain>

  <\explain|<explain-macro|<em|x>-clean>>
    Alle Unterzähler in dem Abschnitt werden zurückgesetzt.
  </explain>

  <\explain|<explain-macro|<em|x>-header|name>>
    Ändern den Seiten-Kopf.
  </explain>

  <\explain|<explain-macro|<em|x>-toc|name>>
    Erzeugt einen Eintrag in das Inhaltsverzeichnis.
  </explain>

  Schlieÿlich hat die <tmdtd|section-base> <abbr|D.T.D.> noch Makros zur
  Darstellung von automatisch erstellten Verzeichnissen
  <markup|render-table-of-contents>, <markup|render-bibliography>,
  <markup|render-index> und <markup|render-glossary>, die jeweils zwei
  Argumente haben, den Namen und den Rumpf.

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