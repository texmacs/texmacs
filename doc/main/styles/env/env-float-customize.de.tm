<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Kontexte für bewegliche Objekte anpassen>

  Die folgenden Makros dienen zur Anpassung der Darstellung von
  Abbildungs-artigen Kontexten:

  <\explain|<explain-macro|render-small-figure|aux|name|body|caption>>
    Dieses Makro steuert die Darstellung von kleinen Abbildungen
    (small-figure). Das erste Argument <src-arg|aux> spezifiziert einen
    \R<em|auxiliary channel>`` (wie z.B. \Rfigure'' oder \Rtable''), der dazu
    benutzt wird, die Beschriftung in die Liste der Abbildungen aufzunehmen.
    Das zweite Argument spezifizierte den Namen <src-arg|name>, den
    kennzeichnenden Textes, wie z.B. \RAbbildung 2.3'' oder \RTabelle
    <no-break>5''. Die letzten Argumente <src-arg|body> und <src-arg|caption>
    sind die Abbildung selbst und die Beschriftung.
  </explain>

  <\explain|<explain-macro|render-big-figure|aux|name|body|caption>>
    Ähnlich <markup|render-small-figure>, aber für groÿe Abbildungen.
  </explain>

  Die folgenden Makros werden benutzt, um den Text um Abbildungen, Tabellen
  und Fuÿnoten anzupassen:\ 

  <\explain|<explain-macro|figure-name|name>>
    Dieses Makro steuert das Aussehen des Textes \RAbbildung''. Die
    Voreinstellung ist <strong|fett>.
  </explain>

  <\explain|<explain-macro|figure-sep>>
    Dieses Makro definiert das Trennzeichen zwischen der Nummerierung und der
    Beschriftung. Die Voreinstellung ist ein Punkt mit einem nachfolgenden
    Leerzeichen.
  </explain>

  <\explain|<explain-macro|footnote-sep>>
    Dieses Makro definiert das Trennzeichen zwischen der Nummerierung und der
    Beschriftung. Die Voreinstellung ist ein Punkt mit einem nachfolgenden
    Leerzeichen.
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