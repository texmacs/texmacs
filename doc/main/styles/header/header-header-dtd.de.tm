<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard Kopfzeilen>
  
  

  Die <tmdtd|header> <abbr|D.T.D.> stellt Befehle für Kopfzeilen bereit. Die
  Optionen zur Anpassung basieren auf der Idee, dass man einen
  <em|Seitentext> für jede einzelne Seite spezifizieren können möchte. Dieser
  <em|Seitentext> kann ein laufender Titel sein oder der Name des jeweiligen
  Kapitels. Der <em|Seitentext> kann für gerade und ungerade Seiten
  verschieden sein und kann auch in einer anderen Aufmachung bei speziellen
  Seiten, wie z.B. dem Beginn neuer Kapitel erscheinen. Die folgenden Befehle
  steuern die Darstellung des Bildschirm- und Druckbildes verschiedener Typen
  von Seiten:

  <\explain|<explain-macro|start-page|page-text>>
    Dieses Makro spezifiziert das Layout der ersten Seite eines neuen
    Kapitels oder eines Abschnitts.
  </explain>

  <\explain|<explain-macro|odd-page-page|page-text>>
    Ähnlich <markup|start-page>, aber für ungerade normale Seiten.
  </explain>

  <\explain|<explain-macro|even-page-page|page-text>>
    Ähnlich <markup|start-page>, aber für gerade normale Seiten.
  </explain>

  Die folgenden Befehle steuern die logischen Kopfzeilen-Aktionen die zur
  Darstellung von Titel, Autor gebraucht werden, oder die benötigt werden, um
  einen neuen Abschnitt zu beginnen:

  <\explain|<explain-macro|header-title|title>>
    Dieses Makro dient zur Spezifizierung des Titels <src-arg|title> eines
    Dokuments in der Kopfzeile.
  </explain>

  <\explain|<explain-macro|header-author|author>>
    Dieses Makro dient zur Spezifizierung des/der Autors/Autoren
    <src-arg|author> eines Dokuments in der Kopfzeile.
  </explain>

  <\explain|<explain-macro|header-primary|section-title>>
    Dieses Makro dient zum Start eines neuen Haupt-Abschnitts, z.B. Kapitel
    <markup|chapter> im Buch-Basis-Stil oder Abschnitt <markup|section> in
    dem Artikel-Stil.
  </explain>

  <\explain|<explain-macro|header-secondary|section-title>>
    Dieses Makro dient zum Start eines neuen Sekundär-Abschnitts, z.B.
    Abschnitt <markup|chapter> im Buch-Basis-Stil oder Unter-Abschnitt
    <markup|subsection> in dem Artikel-Stil.
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
  </collection>
</initial>