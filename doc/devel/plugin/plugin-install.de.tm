<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Ein Plugin installieren und benutzen>

  Anwender finden meist Plugins auf irgendwelchen Webseiten. Ein solches
  Plugin, z.B. <verbatim|<em|myplugin>> liegt normalerweise als \Rgetarte''
  Version

  <\verbatim>
    \ \ \ \ <em|myplugin>-<em|version>-<em|architecture>.tar.gz
    <em|myplugin>-<em|version>-<em|architecture>.tgz
  </verbatim>

  vor. Wenn Sie Ihr <TeXmacs> in das Verzeichnis <verbatim|$TEXMACS_PATH>
  installiert haben, dann sollten Sie diese Datei in dem Verzeichnis
  <verbatim|$TEXMACS_PATH/plugins> auspacken mit dem Befehl

  <\verbatim>
    \ \ \ \ tar -zxvf <em|myplugin>-<em|version>-<em|architecture>.tar.gz
  </verbatim>

  Damit wird ein Unterverzeichnis <verbatim|<em|myplugin>> unter
  <verbatim|$TEXMACS_PATH/plugins> angelegt. Wenn Sie nun <TeXmacs> starten,
  sollte Ihr Plugin automatisch erkannt werden. Bitte lesen Sie die
  Dokumentation, die mit dem Plugin kommt, um den Gebrauch des Plugins zu
  erlernen.

  <\remark>
    Wenn Sie <TeXmacs> nicht selbst installiert haben, oder wenn Sie keinen
    Zugriff auf <verbatim|$TEXMACS_PATH> haben, dann können Sie auch in das
    Verzeichnis <verbatim|$TEXMACS_HOME_PATH/plugins> gehen und dort Ihren
    \RTarball'' auspacken. Erinnern Sie sich, dass
    <verbatim|$TEXMACS_HOME_PATH> gemäÿ Vorgabe das Verzeichnis
    \ <verbatim|$HOME/.TeXmacs> ist. Nachdem Sie Ihr Plugin so installiert
    haben, sollte <TeXmacs> nach dem Neustart auch hier Ihr Plugin
    automatisch erkennen.,
  </remark>

  <\remark>
    Wenn Ihr Plugin in Form des Quellcodes geliefert wird, z.B. als mit einem
    Namen der folgenden Art: <verbatim|<em|myplugin>-<em|version>-src.tar.gz>,
    dann müssen Sie den Code noch kompilieren, bevor Sie \ <TeXmacs> starten.
    Sie müssen zuerst in das myplugin-Verzeichnis wechseln (Befehl: cd
    myplugin) und dann je nach Plugin, lesen Sie die Anweisungen, den Befehl

    <\verbatim>
      \ \ \ \ make
    </verbatim>

    oder

    <\verbatim>
      \ \ \ \ zuerst: ./configure anschlieÿend: make
    </verbatim>

    ausführen.
  </remark>

  <\remark>
    Um eine neue Version des Plugins die Dateien im entsprechenden
    Plugin-Verzeichnis <verbatim|$TEXMACS_PATH/plugins> oder
    <verbatim|$TEXMACS_HOME_PATH/plugins> mit

    <\verbatim>
      \ \ \ \ rm -rf <em|myplugin>
    </verbatim>

    und installieren die neue Version, wie oben beschrieben.
  </remark>

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