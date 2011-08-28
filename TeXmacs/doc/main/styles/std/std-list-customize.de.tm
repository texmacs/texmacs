<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Listenkontexte anpassen>

  <tmdtd|std-list> stellt die folgenden redefinierbaren Makros zur
  Darstellung von Listen und Punkten in den Listen bereit:

  <\explain|<explain-macro|render-list|body>>
    Dieses Block-Konstrukt dient zur Darstellung des Rumpfes <src-arg|body>
    einer Liste. Normalerweise wird die Listen eingerückt und vertikaler
    Leerraum vor und dahinter eingefügt.
  </explain>

  <\explain|<explain-macro|aligned-item|item-text>>
    Dieses Zeilen-Konstrukt dient zur rechtsbündigen Darstellung von
    <src-arg|item-text>, der Trennmarke bzw. der Beschreibung. Der
    eigentliche Text erscheint dahinter linksbündig.
  </explain>

  <\explain|<explain-macro|compact-item|item-text>>
    Dieses Zeilen-Makro dient zur Darstellung von linksbündigem
    <src-arg|item-text> der Trennmarke bzw. der Beschreibung. Der eigentliche
    Text wird daher um die Breite von \ <src-arg|item-text> eingerückt. Es
    sei denn, dieser befindet sich in einem neuen Absatz.\ 
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
    <associate|language|english>
  </collection>
</initial>