<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Inhaltsverzeichnisse>

  Die folgenden Makros dienen im Haupt-Text zur Einfügung von Einträgen in
  das Inhaltsverzeichnis. Diese werden im allgemeinen von den
  Abschnitts-Makros automatisch erzeugt. Manchmal ist es aber erwünscht,
  zusätzlich eigene Einträge manuell vorzunehmen.

  <\explain>
    <explain-macro|toc-main-1|entry>

    <explain-macro|toc-main-2|entry>
  <|explain>
    Erzeuge einen Eintrag <src-arg|entry> der primären Gliederungsebene in
    dem Inhaltsverzeichnis. Das Makro <markup|toc-main-1> ist für besonders
    hervorzuhebende Gliederungsstufen zu verwenden, wie z.B. Buchteile oder
    Bücher einer Folge von Büchern. Es muss in der Regel manuell eingefügt
    werden. Das Makro <markup|toc-main-2> wird üblicherweise als oberste
    Gliederungsstufe, wie z.B. für Kapitel bei Büchern oder Abschnitte in
    Artikeln benutzt. Meist wird es <strong|fett> gesetzt.
  </explain>

  <\explain>
    <explain-macro|toc-normal-1|entry>

    <explain-macro|toc-normal-2|entry>

    <explain-macro|toc-normal-3|entry>
  <|explain>
    Diese sind für Einträge <src-arg|entry> der Gliederungsstufen unter der
    primären Gliederungsstufe gedacht. <markup|toc-normal-1> entspricht in
    Büchern den Abschnitten, <markup|toc-normal-2> Unter-Abschnitten und
    <markup|toc-normal-3> Unter-Unter-Abschnitten.
  </explain>

  <\explain>
    <explain-macro|toc-small-1|entry>

    <explain-macro|toc-small-2|entry>
  <|explain>
    Trägt Einträge <src-arg|entry> weiterer tieferer Gliederungsstufen ein,
    z.B. Absatz. <markup|toc-small-1> und <markup|toc-small-2> sind oft in
    den Basis-Stilen nicht vorgesehen und werden einfach ignoriert.
  </explain>

  Die folgenden Makros können redefiniert werden, um die Darstellung
  anzupassen:

  <\explain>
    <explain-macro|toc-strong-1|content|where>

    <explain-macro|toc-strong-2|content|where>
  <|explain>
    Diese Makros steuern die Darstellung von <markup|toc-main-1> <abbr|bzw.>
    <markup|toc-main-2> an der(n) Seitenzahl(en)
    <with|color|brown|<em|where>>.
  </explain>

  <\explain>
    <explain-macro|toc-1|content|where>

    <explain-macro|toc-2|content|where>

    <explain-macro|toc-3|content|where>

    <explain-macro|toc-4|content|where>

    <explain-macro|toc-5|content|where>
  <|explain>
    Diese Makros steuern die Darstellung von <markup|toc-normal-1>,
    <markup|toc-normal-2>, <markup|toc-<no-break>normal-<no-break>3>,
    <markup|toc-small-1> <abbr|bzw.> <markup|toc-small-2> an der(n)
    Seitenzahl(en) <with|color|brown|<em|where>>.
  </explain>

  <\explain|<explain-macro|toc-dots>>
    Die Trennzeichen zwischen einem Eintrag und der(den) Seitenzahl(en). Die
    Vorgabe sind horizontale Punkte.
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