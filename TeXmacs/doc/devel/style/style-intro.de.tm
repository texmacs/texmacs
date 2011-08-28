<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs> Stil-Definitionen>

  Eine der grundlegenden Stärken von <TeXmacs> ist, dass Sie Ihre eigenen
  Stile definieren und vorhandene freizügig anpassen können. Dazu können Sie
  Stil-Definitions-Dateien und Stil-Pakete schreiben. Diese erfüllen
  gleichzeitig mehrere Aufgaben:

  <\itemize>
    <item>Sie dienen zur abstrakten Definition von repetitiven Elementen in
    Texten wie z.B. Abschnitte, Nummerierungen usw..

    <item>Sie bieten Mechanismen zur Text-Strukturierung. Man kann z.B. ein
    Textstück als Abkürzung, als Zitat oder als \Rwichtig'' definieren.\ 

    <item>Mit den Standard-Basis-Stilen können Sie professionell gestaltete
    Dokumente schreiben, denn die zugehörigen Stil-Definitionen wurden mit
    groÿer Sorgfalt von Leuten geschrieben, die viel von Typographie und
    Ästhetik verstehen.
  </itemize>

  Jedes Dokument kann mit mehreren Stilen assoziiert werden, die sowohl
  Standard-Stile sein können als auch vom Anwender selbst definiert sein
  können. Der Haupt-Stil, der Basis-Stil, wird im Menü <menu|Document|Style>
  ausgewählt. Er entspricht in der Regel dem Dokument, das Sie schreiben
  wollen: Brief, Buch, Veröffentlichung usw. oder eine bestimmende
  Layout-Politik, wie Sie z.B. viele Verlage vorgeben. Dazu können weitere
  Stil-Pakete hinzugefügt werden, die im Menü <menu|Document|Use package>
  auszuwählen sind. Diese Stil-Pakete modifizieren den Basis-Stil. Z.B. dient
  das Paket <tmpackage|number-europe> Paket dazu, die europäische Art der
  Nummerierung von Abbildungen, Beweisen usw. einzuführen, bei jeder einzelne
  Typ auch einzeln gezählt wird. Das Paket <tmpackage|maxima> enthält Makros,
  um die Ausgabe des Computer Algebra Systems <name|Maxima> ansprechend zu
  formatieren, wenn man <TeXmacs> als Oberfläche dafür benutzt. Mehrere
  Pakete können gleichzeitig verwendet werden.

  Wenn Sie eigenes Layout schreiben wollen oder wenn Sie vorhandenes Layout
  Ihren Bedürfnissen anpassen wollen, dann müssen Sie sich entscheiden, ob
  Sie einen vollständig neuen Basis-Stil schreiben wollen oder ein
  Stil-Paket. In den meisten Fällen werden Sie möglicherweise es vorziehen,
  ein Stil-Paket zu schreiben, denn dann können Sie dieses mit beliebigen
  anderen Paketen kombinieren. In einigen Fällen ist es vorteilhafter einen
  neuen Basis-Stil zu schaffen, meist, indem Sie einen vorhandenen verändern.
  Das ist hauptsächlich dann der Fall, wenn man die Layout-Politik eine
  bestimmten Zeitschrift nachäffen will.

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