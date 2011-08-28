<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Dynamische Objekte>

  Bestimmte etwas komplexere Objekte können verschiedene Zustände während der
  Editiervorgänge einnehmen. Beispiele für <em|dynamischen Objekte> sind
  Textmarken und Referenzen, weil ihre Darstellung von Zahlen abhängt, die
  sich laufend ändern können. Weitere Beispiele dafür finden sich
  <hlink|hier|../../../devel/style/style.de.tm>.\ 

  Wenn man ein dynamisches Objekt, wie z.B. eine Textmarke (label) mit
  <shortcut|(make-label)> einfügt, ist der Vorgabe-Zustand \R<em|inaktiv>''. In diesem
  inaktiven Zustand können notwendige Parameter eingegeben werden, wie in
  unserem Fall die kennzeichnende Zeichenkette. Manche dynamischen Objekte
  können eine beliebige Anzahl Parameter aufnehmen. In diesem Fall können
  neue <key|Tab>-Taste eingefügt werden.

  <\big-figure>
    <with|color|blue|<with|mode|math|\<langle\>>label<with|mode|math|\|>>pythagoras<with|color|blue|<with|mode|math|\<rangle\>>>
  </big-figure|Inaktive Textmarke>

  Wenn alle relevanten Informationen in das inaktive dynamische Objekt
  eingegeben sind, dann können sie es mit Wagenrücklauf-Taste
  <shortcut|(kbd-return)> <em|aktivieren>. Ein aktives dynamisches Objekt kann
  deaktiviert werden, indem man den Cursor unmittelbar dahinter positioniert
  und dann die Rücktaste <key|backspace>betätigt.

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
    <associate|preamble|false>
  </collection>
</initial>