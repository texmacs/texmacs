<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Nummerierte Kontexte anpassen>

  Die folgenden Makros dienen zur Darstellung von Text-Kontexten. Sie können
  alle umdefiniert werden, um die Darstellung speziellen Bedürfnissen
  anzupassen.

  <\explain|<explain-macro|render-theorem|name|body>>
    Dieses Makro dient zur Darstellung von nummerierten, Theorem-artigen
    Kontexten. Das erste Argument Name <src-arg|name> gibt den Namen des
    \RTheorem`` (den kennzeichnenden Text), z.B. \RTheorem 1.2'' und das
    zweite Argument enthält den Rumpf. Dieser Kontext wird in Konstrukten
    gebraucht, die mit <markup|new-theorem> definiert wurden.
  </explain>

  <\explain|<explain-macro|render-remark|name|body>>
    Ähnlich <markup|render-theorem>, aber für <localize|remark>s-artige
    Kontexte.
  </explain>

  <\explain|<explain-macro|render-exercise|name|body>>
    Ähnlich <markup|render-theorem>, aber für <localize|exercise>-artige
    Kontexte.
  </explain>

  <\explain|<explain-macro|render-proof|name|body>>
    Ähnlich <markup|render-theorem>, aber für <localize|proof>. Dies wird
    hauptsächlich dazu benutzt, den Namen von eines Beweises anzupassen, z.B.
    wie in \REnde des Beweise von Satz 1.2''.
  </explain>

  Beachten Sie, dass Sie diese Makros dazu benutzen können, einen Kontext zu
  erzeugen, der nur sich im Namen unterscheidet, z.B. anstelle von Satz
  Korollar.

  Die folgenden Befehle geben weitere Möglichkeiten zur
  Darstellungs-Anpassung:

  <\explain|<explain-macro|theorem-name|name>>
    Diese Makro kontrolliert die Darstellung von Namen in Theorem-artigen und
    Bemerkungs-artigen Kontexten. Die meisten Basis-Stile benutzen
    <strong|fett> oder <with|font-shape|small-caps|Kapitälchen>.
  </explain>

  <\explain|<explain-macro|exercise-name|name>>
    Ähnlich <markup|theorem-name>, aber für <localize|exercise>.
  </explain>

  <\explain|<explain-macro|theorem-sep>>
    Das Trennzeichen zwischen dem Namen in einem Theorem-artigem und
    Bemerkungs-artigem Kontext und dem Rumpf. Die Voreinstellung ist ein
    Punkt mit anschlieÿendem Leerzeichen.\ 
  </explain>

  <\explain|<explain-macro|exercise-sep>>
    Ähnlich <markup|theorem-sep>, aber für <localize|exercise>.
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