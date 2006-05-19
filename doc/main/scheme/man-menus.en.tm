<TeXmacs|1.0.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating your own dynamic menus>

  You may define (or modify) a (part of a) menu with name <verbatim|name>
  using

  <\verbatim>
    \ \ \ \ (menu-bind <em|name> . <em|prog>)
  </verbatim>

  and append new entries to an existing (part of a) menu with name
  <verbatim|name> using

  <\verbatim>
    \ \ \ \ (menu-extend <em|name> . <em|prog>)
  </verbatim>

  Here <verbatim|<em|prog>> is a program which represents the entries of the
  menu. In particular, you may take a look at the files in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  in order to see how the standard <TeXmacs> menus are defined.

  More precisely, the program <verbatim|<em|prog>> in <verbatim|menu-set> or
  <verbatim|menu-append> is a list of entries of one of the following forms:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "<em|pulldown menu name>"
    <em|menu-definition>)<next-line> \ \ \ (-\<gtr\> "<em|pullright menu
    name>" <em|menu-definition>)<next-line> \ \ \ ("<em|entry>"
    <em|action>)<next-line> \ \ \ ---<next-line> \ \ \ (if c<em|ondition>
    <em|menu-definition>)<next-line> \ \ \ (link <em|variable>)
  </verbatim>

  The constructors <verbatim|=\<gtr\>> and <verbatim|-\<gtr\>> are used to
  create pulldown or pullright menus and <verbatim|<em|menu-definition>>
  should contain a program which creates the submenu. The constructor
  <verbatim|("<em|entry>" <em|action>)> creates an ordinary entry, where
  <verbatim|action> will be compiled and executed when you click on
  <verbatim|<em|entry>>. Items of a menu may be separated using
  <verbatim|--->. The constructor <verbatim|if> is used for inserting menu
  items only if a certain <verbatim|<em|condition>> is satisfied (for
  instance, if we are in math mode).

  Finally, if you declared a menu <verbatim|<em|name>>, then you may use this
  menu indirectly using the <verbatim|link> constructor. This indirect way of
  declaring submenus has two advantages

  <\itemize>
    <item>An ``indirect'' submenu may be linked to as many menus as we like.

    <item>New items may be added to ``indirect'' submenus
    <with|font-shape|italic|a posteriori> using <verbatim|menu-append>.
  </itemize>

  The main <TeXmacs> menus are <verbatim|texmacs-menu>,
  <verbatim|texmacs-popup-menu>, <verbatim|texmacs-main-icons>,
  <verbatim|texmacs-context-icons> and <verbatim|texmacs-extra-icons>. Other
  standard indirect menus are <verbatim|file-menu>, <verbatim|edit-menu>,
  <verbatim|insert-menu>, <verbatim|text-menu>, <verbatim|paragraph-menu>,
  <verbatim|document-menu>, <verbatim|options-menu> and <verbatim|help-menu>.

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