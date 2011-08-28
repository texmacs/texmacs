<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Berechnungen im Hintergrund>

  Bis jetzt haben wir Schnittstellen von <TeXmacs> zu anderen Anwendungen
  betrachtet, die dazu gedacht sind, aus <TeXmacs> heraus interaktiv mit den
  Anwendungen zu kommunizieren. Es gibt aber auch einen <value|scheme>-Befehl

  <\scheme-fragment>
    (plugin-eval <em|plugin> <em|session> <em|expression>)
  </scheme-fragment>

  mit dem man den Ausdruck <verbatim|<em|expression>> durch die Anwendung
  evaluieren lassen kann. Darin ist <verbatim|<em|plugin>> der Name des
  Anwendungs-Plugin, <verbatim|<em|session>> die Bezeichnung der Sitzung und
  <verbatim|<em|expression>> ein <value|scheme>-Ausdruck, der einen
  <TeXmacs>-Baum repräsentiert.

  <paragraph*|Das <verbatim|substitute> plugin>

  Evaluierungen im Hintergrund können z.B. dazu benutzt werden, dass, wenn
  der Anwender einen Ausdruck ausgewählt hat, dieser Ausdruck durch das
  Ergebnis der Evaluierung ersetzt wird. Das vorliegende Beispiel
  <verbatim|substitute> formt mathematische <LaTeX>-Ausdrücke in
  <TeXmacs>-Ausdrücke um. Auÿerdem stellt es dafür den Kurzbefehl <shortcut|(substitute-substitute)>
  bereit. das Plugin besteht aus den folgenden Dateien:

  <\verbatim>
    \ \ \ \ <example-plugin-link|substitute/Makefile>

    \ \ \ \ <example-plugin-link|substitute/progs/init-substitute.scm>

    \ \ \ \ <example-plugin-link|substitute/src/substitute.cpp>
  </verbatim>

  Die Hauptschleife von <verbatim|substitute.cpp> besteht aus den folgenden
  Zeilen

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Die Konfigurationsdatei des Plugins, <verbatim|init-substitute.scm>, hat
  den folgenden Code, der den ausgewählten Bereich durch das Ergebnis der
  Evaluierung ersetzt.

  <\scheme-fragment>
    (define (substitute-substitute)

    \ \ (import-from (texmacs plugin plugin-cmd))

    \ \ (if (selection-active-any?)

    \ \ \ \ \ \ (let* ((t (tree-\<gtr\>object (the-selection)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ (u (plugin-eval "substitute" "default" t)))

    \ \ \ \ \ \ \ \ (clipboard-cut "primary")

    \ \ \ \ \ \ \ \ (insert-tree (object-\<gtr\>tree u)))))
  </scheme-fragment>

  und den Code für den Kurzbefehl <shortcut|(substitute-substitute)>:

  <\scheme-fragment>
    (kbd-map

    \ \ ("C-F12" (substitute-substitute)))
  </scheme-fragment>

  Beachten Sie bitte, dass diese Routinen in gröÿeren, echten Plugins in
  eigenständige Module gehören.

  <paragraph*|Das <verbatim|secure> plugin>

  Ein weiteres Beispiel dafür, wie man eine Schnittstelle im Hintergrund
  benutzen kann, gibt das Plugin <verbatim|secure>, das die Dateien

  <\verbatim>
    \ \ \ \ <example-plugin-link|secure/Makefile>

    \ \ \ \ <example-plugin-link|secure/packages/secure.ts>

    \ \ \ \ <example-plugin-link|secure/progs/init-secure.scm>

    \ \ \ \ <example-plugin-link|secure/progs/secure-secure.scm>

    \ \ \ \ <example-plugin-link|secure/src/secure.cpp>
  </verbatim>

  enthält.

  Wie <verbatim|substitute.cpp> oben formt das Hauptprogramm
  <verbatim|secure.cpp> <LaTeX>-Ausdrücke in <TeXmacs>-Ausdrücke um. Das
  Modul <verbatim|secure-secure.scm> enthält die <em|sichere>
  <value|scheme>-Routine <verbatim|latexer>:

  <\scheme-fragment>
    (tm-define (latexer s)

    \ \ (:type (tree -\<gtr\> object))

    \ \ (:synopsis "convert LaTeX string to TeXmacs tree using plugin")

    \ \ (:secure #t)

    \ \ (plugin-eval "secure" "default" (tree-\<gtr\>string s)))
  </scheme-fragment>

  Man muss <verbatim|latexer> unbedingt als \Rsicher`` einstufen, damit sie
  mit dem <markup|extern> Konstrukt zur Definition von weiteren Konstrukten
  herangezogen werden kann. Dazu wird die Stil-Definition
  \ <verbatim|secure.ts> benutzt mit dem Code:

  <\tm-fragment>
    <\inactive*>
      Sehen Sie einen mathematischen TeX-Befehl als TeXmacs-Ausdruck mittels
      eines Plugin

      <assign|latexer|<macro|x|<extern|latexer|<arg|x>>>>
    </inactive*>
  </tm-fragment>

  Nach Kompilierung, Installation und Neustart von <TeXmacs> können Sie ,
  nachdem Sie den neuen Menübefehl <menu|Document|Use package|Secure>
  durchgeführt haben, <markup|latexer> als neuen Konstrukt benutzen. Dieser
  nimmt einen mathematischen <LaTeX>-Ausdruck als Argument und zeigt ihn nach
  der Konversion auf dem Bildschirm an.

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