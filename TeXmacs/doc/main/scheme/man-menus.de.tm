<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Eigene dynamische Menüs schreiben>

  Sie Menüs oder Teile eines Menüs selbst erstellen oder ändern, indem Sie
  für das Menü mit Namen <verbatim|name> \ den Befehl

  <\verbatim>
    \ \ \ \ (menu-bind name . prog)
  </verbatim>

  benutzen. Sie können neue Einträge an ein existierendes Menü mit dem Namen
  <verbatim|name> mit dem Befehl

  <\verbatim>
    \ \ \ \ (menu-extend name . prog)
  </verbatim>

  anhängen. <verbatim|prog> ist hier ein Programm, das von dem Menüeintrag
  repräsentiert wird. Sehen Sie sich die Dateien im Verzeichnis

  <\verbatim>
    \ \ \ \ progs/menu
  </verbatim>

  um zu sehen, wie die Standard <TeXmacs>-Menüs definiert sind.

  Genauer gesagt, ist das Programm <verbatim|prog> in <verbatim|menu-set>
  oder <verbatim|menu-append> eine Liste von Einträgen einer der folgenden
  Formen:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<next-line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<next-line>
    \ \ \ ("entry" action)<next-line> \ \ \ ---<next-line> \ \ \ (if
    condition menu-definition)<next-line> \ \ \ (link variable)
  </verbatim>

  Die Konstruktoren <verbatim|=\<gtr\>> und <verbatim|-\<gtr\>> werden
  benutzt, um Pulldown- bzw. Pullright-Menüs zu erzeugen.
  <verbatim|menu-definition> enthält ein Programm, das ein Unter-Menü
  erstellt. Der Konstruktor <verbatim|("entry" action)> erzeugt einen
  normalen Eintrag, der <verbatim|action> kompiliert und ausführt, wenn man
  auf den Eintrag <verbatim|entry> klickt. Die Einträge können mit
  <verbatim|---> getrennt werden. Der Konstruktor <verbatim|if> wird benutzt,
  wenn ein Eintrag nur erfolgen soll, wenn eine bestimmte Bedingung
  <verbatim|condition> erfüllt ist, z.B. dass man im mathematischen Modus
  ist.

  Wenn sie ein Menü mit Namen <verbatim|name> definiert haben, können Sie
  dieses Menü indirekt mit dem <verbatim|link> Konstruktor verwenden. Das hat
  für Untermenüs zwei Vorteile:

  <\itemize>
    <item>Ein \Rindirektes'' Untermenü kann zu beliebig vielen Menüs gelinkt
    werden

    <item>Neue Einträge können <with|font-shape|italic|a posteriori>
    \Rindirekten`` Menüs zugefügt werden, indem man <verbatim|menu-append>
    benutzt.
  </itemize>

  Die Haupt-<TeXmacs>-Menüs sind: \ <verbatim|texmacs-menu>,
  <verbatim|texmacs-popup-menu>, <verbatim|texmacs-main-icons>,
  <verbatim|texmacs-context-icons> und <verbatim|texmacs-extra-icons>. Andere
  indirekte Standard-Menüs sind: <verbatim|file-menu>, <verbatim|edit-menu>,
  <verbatim|insert-menu>, <verbatim|text-menu>, <verbatim|paragraph-menu>,
  <verbatim|document-menu>, <verbatim|options-menu> und <verbatim|help-menu>.

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