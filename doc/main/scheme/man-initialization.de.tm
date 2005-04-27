<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Initialisierungsdateien>

  Wenn <TeXmacs> gestartet wird, führt es die Datei

  <\verbatim>
    \ \ \ \ progs/init-texmacs.scm
  </verbatim>

  und Ihre persönliche Initialisierungsdatei

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm
  </verbatim>

  aus, sofern sie existiert. Per Vorgabe entspricht der
  <verbatim|$TEXMACS_HOME_PATH> dem Pfad <verbatim|.TeXmacs>. Jedes mal, wenn
  Sie einen neuen Puffer erzeugen, indem Sie eine Datei laden oder mit dem
  Menü-befehl <menu|File|New> neu schaffen, wird die Datei

  <\verbatim>
    \ \ \ \ progs/init-buffer.scm
  </verbatim>

  ausgeführt und auch

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-buffer.scm
  </verbatim>

  wenn sie existiert.

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