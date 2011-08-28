<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Stichwortverzeichnisse>

  Die folgenden Makros dienen im Haupt-Text zur Einfügung von
  Stichwort-Einträgen:

  <\explain|<explain-macro|index|primär>>
    Trage <src-arg|primär> als den primären Eintrag in das
    Stichwortverzeichnis ein.
  </explain>

  <\explain|<explain-macro|subindex|primär|sekundär>>
    Trage <src-arg|sekundär> als den sekundären (Unter-)Eintrag von
    <src-arg|primär> das Stichwortverzeichnis ein.
  </explain>

  <\explain|<explain-macro|subsubindex|primär|sekundär|tertiär>>
    Ähnlich <markup|subindex> aber für den Unter-Unter-Eintrag
    <src-arg|tertiär>.
  </explain>

  <\explain|<explain-macro|index-complex|key|how|range|entry>>
    Trage einen komplexen Eintrag in das Stichwort-Register ein. Dies wird im
    Abschnitt <hyper-link|Stichwortverzeichnisse
    erzeugen|../../links/man-index.de.tm> genauer erklärt.
  </explain>

  <\explain|<explain-macro|index-line|key|entry>>
    Fügt den Eintrag <src-arg|entry> hinzu, sortiert nach <src-arg|key>.
  </explain>

  Die folgenden Makros können redefiniert werden, um die Darstellung
  anzupassen:

  <\explain>
    <explain-macro|index-1|entry|where>

    <explain-macro|index-2|entry|where>

    <explain-macro|index-3|entry|where>

    <explain-macro|index-4|entry|where>

    <explain-macro|index-5|entry|where>
  <|explain>
    Makro zur Darstellung eines Eintrags <src-arg|entry> auf der(n) Seite(n)
    <src-arg|where>. <markup|index-1> gehört zu Primär-Einträgen,
    <markup|index-2> zu Sekundär-Einträgen usw..
  </explain>

  <\explain>
    <explain-macro|index-1*|entry>

    <explain-macro|index-2*|entry>

    <explain-macro|index-3*|entry>

    <explain-macro|index-4*|entry>

    <explain-macro|index-5*|entry>
  <|explain>
    Ähnlich <markup|index-1> bis <markup|index-5>, aber ohne Seitenzahl(en).
  </explain>

  <\explain|<explain-macro|index-dots>>
    Makro zur Erzeugung der Punkte zwischen Glossar-Eintrag und der
    entsprechenden Seitenzahl(en).
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