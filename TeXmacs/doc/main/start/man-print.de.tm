<TeXmacs|1.99.12>

<style|<tuple|tmdoc|german|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Dokumente drucken>

  Sie können das aktuelle Dokument mit <menu|File|Print|Print all> als Ganzes
  ausdrucken. vollständig. Die Drucker-Voreinstellung von <TeXmacs> sind
  600dpi Drucker für A4 Papier. Diese Einstellungen können im Menü
  <menu|File|Page setup...> angepasst werden. Sie können auch in eine
  Postscript-Datei mit <menu|File|Print|Print all to file>. In diesem Fall
  werden die Drucker-Voreinstellungen für die Erstellung der Datei benutzt.
  Sie können alternativ auch <menu|File|Export|Postscript> benutzen, in
  diesem Fall werden die Voreinstellungen ignoriert.

  Sie können die aktuelle Datei in <acronym|PDF> umwandeln mit
  <menu|File|Export|Pdf>. Beachten Sie bitte, dass Sie den Schrifttyp
  <em|<name|Type 1>> mit <menu|Edit|Preferences|Printer|Font type|Type 1>,
  wenn Sie haben wollen, dass die so erzeugte Postscript- bzw.
  <acronym|PDF>-Datei \ <name|Type 1>-Schriften <index|pdf>verwendet.
  Allerdings lassen nur die CM-Schriftarten <name|Type 1> zu. Diese
  CM-Schriftarten haben eine etwas schlechtere Qualität als die
  EC-Schriftarten hauptsächlich bei Buchstaben mit Akzenten. Deshalb möchten
  Sie vielleicht lieber EC-Schriftarten verwenden, solange Sie nicht
  PDF-Dateien erzeugen wollen, die im <name|Acrobat Reader> gut ausschauen.

  Wenn Sie den <TeXmacs> richtig konfiguriert haben, dann ist die Darstellung
  im Editor garantiert <em|wysiwyg>: das Druckresultat ist genau das, was Sie
  auf dem Bildschirm sehen. Um das zu erreichen, müssen Sie vor allem die
  richtige Papiergröÿe mit <menu|Document|Page|Type|Paper> und
  <menu|Document|Page|Screen margins|Margins as on paper> einstellen. Sie
  sollten unbedingt sicherstellen, dass die mit <menu|Document|Font|Dpi>
  festzulegende Auflösung genau derjenigen Ihres Druckers entspricht.
  Momentan können geringfügige Unterschiede im Schriftsatz auftreten, wenn
  Sie die Auflösung ändern. Leider können diese geringen Unterschiede durch
  Zeilen- und Seitenumbrüche das ganze Dokument verändern. Dies wollen wir in
  Zukunft ändern.\ 

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
    <associate|page-medium|paper>
    <associate|preamble|false>
  </collection>
</initial>