<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Dokumente zwischen <TeXmacs> und Html portieren>

  Wir haben damit begonnen die Konversion zwischen HTML und <TeXmacs> zu
  implementieren. Im Moment gelingt nur der Import von HTML-Dokumenten mit
  dem Menü-Befehl <menu|File|Import|Html>. Das meiste HTML 2.0 und Teile von
  HTML 3.0 werden derzeit unterstützt. Browser-Fähigkeiten wurden noch nicht
  implementiert. Für die Zukunft planen wir den Import von Math-ML.

  Wenn HTML-Dokumente importiert werden, deren Namen mit <verbatim|http:>
  oder <verbatim|ftp:> beginnen, werden diese über das Netzwerk mit
  <verbatim|wget> heruntergeladen. Wenn Sie <TeXmacs> selbst kompiliert
  haben, können Sie <verbatim|wget> von

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  herunterladen. In den Binär-Distributionen ist <verbatim|wget> enthalten.

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