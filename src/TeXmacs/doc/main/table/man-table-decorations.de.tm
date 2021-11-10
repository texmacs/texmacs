<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Sichtbare Zellränder, Padding und Hintergrundfarbe>

  Sie können die Breite der sichtbaren Umrandung von Zellen und die Breite
  des Paddings, der Leerraum um die Zeichen in der Zelle, in alle vier
  Richtungen festlegen: links, rechts oben und unten. Dazu können sie Befehle
  aus dem Menü <menu|Table|Cell border> benutzen oder Kurzbefehle von der
  Form <key|table b><render-key|<with|mode|math|x>> für die Breite und <key|table p
 ><render-key|<with|mode|math|x>> für das Padding. Dabei kann <render-key|<with|mode|math|x>>
  für die folgenden Tasten stehen: <key|l> für \Rlinks'', <key|r> für
  \Rrechts'', <key|b> für \Runten'' und <key|t> für \Roben''.

  Die Vorgabebreite von Umrandungen ist <verbatim|1ln>, mit anderen Worten
  die Standard Linienbreite in der aktuellen Schriftart z.B. bei einem
  Bruchstrich. Die Umrandung erscheint rechts und unten, auÿer in der ersten
  Zeile oder ersten Spalte. Die horizontale Paddingvorgabe ist
  <verbatim|1spc>: die Standardbreite eines Leerzeichens in der aktuellen
  Schriftart. Die vertikale Paddingvorgabe ist <verbatim|1sep>: der
  Standard-Minimal-Abstand zwischen zwei nahen Boxen.

  Zellen kann eine Hintergrundfarbe mit Befehlen aus <menu|Table|Cell
  background color> gegeben werden.

  Der gesamten Tabelle kann auch eine Umrandung und ein Tabellenpadding
  gegeben werden. Dazu dienen die Befehle aus dem Menü <menu|Table|Special
  table properties|Border>. In diesem Fall erfolgt das Padding auÿerhalb der
  Umrandung.

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