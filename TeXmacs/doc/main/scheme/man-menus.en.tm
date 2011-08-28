<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating your own dynamic menus>

  You may define (or modify) a (part of a) menu with name <scm-arg|name>
  using

  <\scm-fragment>
    (menu-bind <scm-arg|name> . <scm-arg|prog>)
  </scm-fragment>

  and append new entries to an existing (part of a) menu with name
  <scm-arg|name> using

  <\scm-fragment>
    (menu-extend <scm-arg|name> . <scm-arg|prog>)
  </scm-fragment>

  Here <scm-arg|prog> is a program which represents the entries of the menu.
  In particular, you may take a look at the files in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  in order to see how the standard <TeXmacs> menus are defined.

  More precisely, the program <verbatim|<em|prog>> in <verbatim|menu-set> or
  <verbatim|menu-append> is a list of entries of one of the following forms:

  <\scm-fragment>
    (=\<gtr\> "pulldown menu name" <scm-arg|menu-definition>)

    (-\<gtr\> "pullright menu name" <scm-arg|menu-definition>)

    ("entry" <scm-arg|action>)

    ---

    (if <scm-arg|condition> <scm-arg|menu-definition>)

    (link <scm-arg|variable>)
  </scm-fragment>

  \;

  The constructors <scm|=\<gtr\>> and <scm|-\<gtr\>> are used to create
  pulldown or pullright menus and <scm-arg|menu-definition> should contain a
  program which creates the submenu. The constructor <scm|("entry"
  <scm-arg|action>)> creates an ordinary entry, where <scm-arg|action> will
  be compiled and executed when you click on <scm|entry>. Items of a menu may
  be separated using <verbatim|--->. The constructor <scm|if> is used for
  inserting menu items only if a certain <scm-arg|condition> is satisfied
  (for instance, if we are in math mode).

  Finally, if you declared a menu <scm-arg|name>, then you may use this menu
  indirectly using the <scm|link> constructor. This indirect way of declaring
  submenus has two advantages

  <\itemize>
    <item>An ``indirect'' submenu may be linked to as many menus as we like.

    <item>New items may be added to ``indirect'' submenus
    <with|font-shape|italic|a posteriori> using <scm|menu-append>.
  </itemize>

  The main <TeXmacs> menus are <scm|texmacs-menu>, <scm|texmacs-popup-menu>,
  <scm|texmacs-main-icons>, <scm|texmacs-mode-icons>,
  <scm|texmacs-focus-icons> and <scm|texmacs-extra-icons>. Other standard
  indirect menus are <scm|file-menu>, <scm|edit-menu>, <scm|insert-menu>,
  <scm|text-menu>, <scm|paragraph-menu>, <scm|document-menu>,
  <scm|options-menu> and <scm|help-menu>.

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