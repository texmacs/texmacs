<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Sending commands to <TeXmacs>>

  The application may use <verbatim|command> as a very particular output
  format in order to send <value|scheme> commands to <TeXmacs>. In other
  words, the block

  <\quotation>
    <expand|framed-fragment|<verbatim|<key|DATA_BEGIN>command:<em|cmd><key|DATA_END>>>
  </quotation>

  will send the command <verbatim|<em|cmd>> to <TeXmacs>. Such commands are
  executed immediately after reception of <key|DATA_END>. We also recall that
  such command blocks may be incorporated recursively in larger
  <key|DATA_BEGIN>-<key|DATA_END> blocks.

  <paragraph|The <verbatim|menus> plugin>

  The <verbatim|nested> plugin shows how an application can modify the
  <TeXmacs> menus in an interactive way. The plugin consists of the files

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|menus/Makefile>

    \ \ \ \ <expand|example-plugin-link|menus/progs/init-menus.scm>

    \ \ \ \ <expand|example-plugin-link|menus/src/menus.cpp>
  </verbatim>

  The body of the main loop of <verbatim|menus.cpp> simply contains

  <\expand|cpp-fragment>
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
  </expand>

  The <value|scheme> macro <expand|scheme-code|menus-add> is defined in
  <verbatim|init-menus.scm>:

  <\expand|scheme-fragment>
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
  </expand>

  The configuration of <verbatim|menus> proceeds as usual:

  <\expand|scheme-fragment>
    (plugin-configure menus

    \ \ (:require (url-exists-in-path? "menus.bin"))

    \ \ (:launch "menus.bin")

    \ \ (:session "Menus"))
  </expand>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|menus>
      plugin<value|toc-dots><pageref|toc-1>>
    </associate>
  </collection>
</auxiliary>
