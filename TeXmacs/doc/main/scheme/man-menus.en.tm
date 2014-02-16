<TeXmacs|1.99.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Creating your own dynamic menus>

  You may define a menu with name <scm-arg|name> either using

  <\scm-code>
    (menu-bind <scm-arg|name> . <scm-arg|def>)
  </scm-code>

  or

  <\scm-code>
    (tm-menu (<scm-arg|name>) . <scm-arg|def>)
  </scm-code>

  Here <scm-arg|def> is a program which represents the entries of the menu.
  In particular, you may take a look at the files in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  in order to see how the standard <TeXmacs> menus are defined. In the case
  of <scm|tm-menu>, it is possible to specify additional arguments, which
  makes it possible to dynamically construct more complex menus which depend
  on parameters.

  More precisely, the program <verbatim|<em|def>> in <scm|menu-bind> or
  <scm|tm-menu> is a list of entries of one of the following forms:

  <\scm-code>
    (=\<gtr\> "pulldown menu name" <scm-arg|menu-definition>)

    (-\<gtr\> "pullright menu name" <scm-arg|menu-definition>)

    ("entry" <scm-arg|action>)

    ---

    (if <scm-arg|condition> <scm-arg|menu-definition>)

    (when condition menu-definition)

    (link <scm-arg|variable>)

    (former)
  </scm-code>

  The constructors <scm|=\<gtr\>> and <scm|-\<gtr\>> are used to create
  <with|font-shape|italic|pulldown> or <with|font-shape|italic|pullright>
  menus and <scm-arg|menu-definition> should contain a program which creates
  the submenu. In the main (or system) menu bar all root items are pulldown
  menus and all submenus of these are pullright. Both pulldown and pullright
  may be used in toolbars or other widgets.

  The constructor <scm|("entry" <scm-arg|action>)> creates an ordinary entry,
  where <scm-arg|action> will be compiled and executed when you click on
  <scm|entry>. Items of a menu may be separated using <verbatim|--->. The
  constructor <scm|if> is used for inserting menu items only if a certain
  <scm-arg|condition> is satisfied (for instance, if we are in math mode),
  whereas <scm|while> always inserts the item but deactivates (e.g. greying
  it out) it <scm-arg|condition> is not met.

  If you declared a menu <scm-arg|name>, then you may use this menu
  indirectly using the <scm|link> constructor, thus one may link any such
  ``indirect'' submenu to as many menus as desired.

  Finally, new items may be added to any given menu <with|font-shape|italic|a
  posteriori> using <scm|former>, as in the following example:

  <\scm-code>
    (tm-menu (tools-menu)

    \ \ (former)

    \ \ ---

    \ \ ("New item" (noop)))
  </scm-code>

  The main <TeXmacs> menus are:

  <\itemize-dot>
    <item><scm|texmacs-menu>: contains the root entries of the main menu bar
    at the top of the window (or desktop under <name|MacOS>). It uses
    <scm|link> to display <scm|file-menu>, <scm|edit-menu>,
    <scm|insert-menu>, <scm|text-menu>, <scm|paragraph-menu>,
    <scm|document-menu> and <scm|help-menu> among others.

    <item><scm|texmacs-main-icons>: contains the main toolbar, which
    typically features buttons to open and save files, copy and paste text,
    etc.

    <item><scm|texmacs-mode-icons>: contains the icons which depend on the
    current editing mode, that is: mathematics, text, code, etc.

    <item><scm|texmacs-focus-icons>: these icons change with the cursor. One
    should install here any icons that are specific to a particular tag or
    context.

    <item><scm|texmacs-extra-icons>: custom icons for user extensions.

    <item><scm|texmacs-popup-menu>: the menu which pops up when the user
    right-clicks on a <TeXmacs> document. Extending or replacing this menu is
    useful for instance for plugin writers: you may want to display some
    extra actions while removing others when the user in inside a session for
    your plugin.
  </itemize-dot>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>