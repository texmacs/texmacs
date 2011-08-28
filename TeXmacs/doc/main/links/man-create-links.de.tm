<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Label, Verknüpfungen, Hyperlinks und Referenzen erzeugen>

  Sie können ein neues inaktives Label erzeugen mit dem Kurzbefehl
  <shortcut|(make-label)> oder dem Menübefehl \ <menu|Insert|Link|Label>. Label sind
  Marken, die mit einer Referenz verbunden sind und die später durch eine
  Zahl, die auf die Referenz verweist, ersetzt wird. Man erzeugt die Referenz
  <shortcut|(make 'reference)> oder mit dem Menübefehl <menu|Insert|Link|Reference>. Die
  Plazierung eines Labels sollte mit Bedacht erfolgen, um eine korrekte
  Nummerierung zu erreichen. Wenn z.B. Abschnitte einen Label erhalten
  sollen, dann setzt man am besten das Label direkt hinter den
  Abschnitts-Namen. Wenn man \RGleichungen`` mit einem Label versieht, sollte
  man einen Punkt innerhalb der Gleichung verwenden.

  Man kann Hyperlinks zu anderen Dokumenten mit dem Kurzbefehl
  \ <key|inactive \<gtr\>> oder mit <menu|Insert|Link|Hyperlink> erzeugen. Der
  Hyperlink hat zwei Felder, zwei Variable. Die erste Variable ist der mit
  der Verknüpfung assoziierte Text, der, wenn der Link aktiviert ist, blau im
  Text erscheint. Die zweite Variable ist der Name des Dokuments. Wie bei
  Hyperlinks üblich, kann mit <verbatim|#<with|font-shape|italic|Label>> auf
  ein Label im gleichen Dokument verwiesen werden. Sonst ist die allgemeine
  Form <verbatim|<with|font-shape|italic|url>#<with|font-shape|italic|label>>
  (oder ohne Label: <verbatim|<with|font-shape|italic|url>>) und verweist auf
  Label <verbatim|#<with|font-shape|italic|Label>> in
  <verbatim|<with|font-shape|italic|url>>.
  <verbatim|<with|font-shape|italic|url>> kann dabei ein Dokument in Netz
  sein.

  Ganz ähnlich kann eine Aktion mit Text oder Graphik verknüpft werden. Man
  benutzt dazu den Kurzbefehl I <key|inactive *> oder <menu|Insert|Link|Action>.
  Das zweite Feld enthält dann ein ausführbares Guile/Scheme-Skript, das
  ausgeführt wird, wenn man auf den Text oder die Graphik mit der Maus
  doppelklickt. Aus Sicherheitsgründen werden solche Skripts nicht immer
  akzeptiert. In der Voreinstellung werden Sie gefragt, ob Sie das Skript
  ausführen wollen. Dieses Verhalten kann in <menu|Options|Security> geändert
  werden. Beachten sie, dass Sie mit dem Guile/Scheme-Befehl:\ 

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  einen Befehl Ihrer Systemumgebung (shell) ausführen können.

  Schlieÿlich kann man mit \ <key|inactive i> bzw. <menu|Insert|Link|Include> ganze
  Dokumente in den Text einfügen. Damit können Sie z.B. den Quellcode eines
  Programms in den Text einfügen und zwar so, dass jegliche Änderung am
  Quellcode sich automatisch im Textdokument wiederfindet.

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