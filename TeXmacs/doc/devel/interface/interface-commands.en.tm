<TeXmacs|1.0.4.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Sending commands to <TeXmacs>>

  The application may use <verbatim|command> as a very particular output
  format in order to send <value|scheme> commands to <TeXmacs>. In other
  words, the block

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN>command:<em|cmd><render-key|DATA_END>>>
  </quotation>

  will send the command <verbatim|<em|cmd>> to <TeXmacs>. Such commands are
  executed immediately after reception of <render-key|DATA_END>. We also recall that
  such command blocks may be incorporated recursively in larger
  <render-key|DATA_BEGIN>-<render-key|DATA_END> blocks.

  <paragraph*|The <verbatim|menus> plug-in>

  The <verbatim|nested> plug-in shows how an application can modify the
  <TeXmacs> menus in an interactive way. The plug-in consists of the files

  <\verbatim>
    \ \ \ \ <example-plugin-link|menus/Makefile>

    \ \ \ \ <example-plugin-link|menus/progs/init-menus.scm>

    \ \ \ \ <example-plugin-link|menus/src/menus.cpp>
  </verbatim>

  The body of the main loop of <verbatim|menus.cpp> simply contains

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

  The <value|scheme> macro <scheme-code|menus-add> is defined in
  <verbatim|init-menus.scm>:

  <\scheme-fragment>
    (menu-bind menus-menu

    \ \ ("Hi" (insert "Hello world")))

    \;

    (menu-extend texmacs-extra-menu

    \ \ (if (equal? (get-env "prog language") "menus")

    \ \ \ \ \ \ (=\<gtr\> "Menus" (link menus-menu))))

    \;

    (define-macro (menus-add s)

    \ \ `(menu-extend menus-menu

    \ \ \ \ \ (,s (insert ,s))))
  </scheme-fragment>

  The configuration of <verbatim|menus> proceeds as usual:

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
    <associate|language|english>
  </collection>
</initial>