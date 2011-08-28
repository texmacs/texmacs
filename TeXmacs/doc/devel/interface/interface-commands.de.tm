<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Befehle an <TeXmacs> senden>

  Die Anwendung kann <verbatim|command> als ein ganz spezielles
  Ausgabe-Format benutzen. Es dient zur Übertragung von
  <value|scheme>-befehlen an <TeXmacs>. Mit anderen Worten, der Block

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN>command:<em|cmd><render-key|DATA_END>>>
  </quotation>

  sendet den Befehl <verbatim|<em|cmd>> an <TeXmacs>, der dort sofort nach
  dem Erhalt von <render-key|DATA_END> ausgeführt wird. Wir erinnern uns, dass
  derartige Blöcke in gröÿere <render-key|DATA_BEGIN>-<render-key|DATA_END> Blöcke
  eingebettet sein können.

  <paragraph*|Das <verbatim|menus> plugin>

  Das Beispiel <verbatim|menus> zeigte, wie eine Anwendung <TeXmacs>-Menüs
  interaktiv \ ändern kann. Es besteht aus den folgenden Dateien

  <\verbatim>
    \ \ \ \ <example-plugin-link|menus/Makefile>

    \ \ \ \ <example-plugin-link|menus/progs/init-menus.scm>

    \ \ \ \ <example-plugin-link|menus/src/menus.cpp>
  </verbatim>

  Der Rumpf der Hauptschleife von <verbatim|menus.cpp> besteht aus

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "command:(menus-add
    \\""

    \ \ \ \ \ \<less\>\<less\> buffer \<less\>\<less\> "\\")"
    \<less\>\<less\> DATA_END;

    cout \<less\>\<less\> "Added " \<less\>\<less\> buffer \<less\>\<less\> "
    to menu";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Das <value|scheme>-Makro <scheme-code|menus-add> wird in
  <verbatim|init-menus.scm> definiert:

  <\scheme-fragment>
    (menu-bind menus-menu

    \ \ ("Hi" (insert-string "Hello world")))

    \;

    (menu-extend texmacs-extra-menu

    \ \ (if (equal? (get-env "prog language") "menus")

    \ \ \ \ \ \ (=\<gtr\> "Menus" (link menus-menu))))

    \;

    (define-macro (menus-add s)

    \ \ `(menu-extend menus-menu

    \ \ \ \ \ (,s (insert-string ,s))))
  </scheme-fragment>

  Die Konfiguration von <verbatim|menus> erfolgt wie gewöhnlich:

  <\scheme-fragment>
    (plugin-configure menus

    \ \ (:require (url-exists-in-path? "menus.bin"))

    \ \ (:launch "menus.bin")

    \ \ (:session "Menus"))
  </scheme-fragment>

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