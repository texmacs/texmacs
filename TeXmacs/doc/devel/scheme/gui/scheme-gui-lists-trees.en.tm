<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Displaying lists and trees>

  <paragraph|Displaying lists with <scm|enum>, <scm|choice> and
  <scm|choices>>

  <\explain>
    <scm|(enum <scm-arg|cmd> <scm-arg|items> <scm-arg|default>
    <scm-arg|width>)><explain-synopsis|a combo box>
  <|explain>
    Builds a combo box which will execute <scm-arg|cmd> whenever the user
    makes a choice. The <scm-arg|width> may be given in any <TeXmacs> length
    unit.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (tm-widget (test-enum)

        \ \ (enum (display* "First " answer "\\n")

        \ \ \ \ \ \ \ \ '("gnu" "gnat" "zebra")

        \ \ \ \ \ \ \ \ "zebra" "10em")))
      <|unfolded-io>
        \;
      </unfolded-io>

      <\input|Scheme] >
        (show test-enum)
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(choice <scm-arg|cmd> <scm-arg|items>
    <scm-arg|default>)><explain-synopsis|a list of items allowing one to be
    chosen>
  <|explain>
    Builds a list of items which will execute <scm-arg|cmd> whenever the user
    makes a choice. <scm-arg|items> is a list, <scm-arg|default> a value.
    Contrary to <scm|enum>, all items are displayed simultaneously. If one
    desires scrollbars, the widget must be enclosed in a <scm|scrollable>
    container. The width of the widget may be set using a <scm|resize>
    widget.

    <\session|scheme|default>
      <\input|Scheme] >
        (tm-widget (test-choice)

        \ \ (resize "200px" "50px"

        \ \ \ \ (scrollable

        \ \ \ \ \ \ (choice (display* answer "\\n")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("First" "Second" "Third" "Fourth"
        "Fifth" "Sixth")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ "Third"))))
      </input>

      <\input|Scheme] >
        (show test-choice)
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(choices <scm-arg|cmd> <scm-arg|items>
    <scm-arg|defaults>)><explain-synopsis|a list of items allowing several to
    be chosen>
  <|explain>
    Builds a list of items which will execute <scm-arg|cmd> whenever the user
    makes a choice. Several items may be selected at the same time. Both
    <scm-arg|items> and <scm-arg|defaults> are hence lists. Contrary to
    <scm|enum>, all items are displayed simultaneously. If one desires
    scrollbars, the widget must be enclosed in a <scm|scrollable> container.
    The width of the widget may be set using a <scm|resize> widget.

    <\session|scheme|default>
      <\input|Scheme] >
        (tm-widget (test-choices)

        \ \ (resize "200px" "100px"

        \ \ \ \ (scrollable

        \ \ \ \ \ \ (choices (display* answer "\\n")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("A" "B" "C" "D" "E" "F" "G" "H" "I"
        "J" "K" "L")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("B" "D" "F" "H" "J" "L")))))
      </input>

      <\input|Scheme] >
        (show test-choices)
      </input>
    </session>
  </explain>

  <paragraph|Displaying trees with <scm|tree-widget>>

  <\explain>
    <scm|(tree-widget cmd <scm-arg|data> <scm-arg|data-roles>)><explain-synopsis|a
    tree view>
  <|explain>
    The <scm|tree-widget> provides a graphical representation of a <TeXmacs>
    tree <scm-arg|data> (not a <scheme> tree!). This may be part of a
    document or any other tree. The first node in <scm-arg|data> won't be
    displayed. All other nodes may have attributes called <em|data roles>
    which will determine the textual representation of the node, whether it
    has some icon next to it and which one, etc. These attributes are simply
    children of the nodes in <scm-arg|data> at predefined positions given by
    the data roles specification in the argument <scm-arg|data-roles>. This
    is a list of identifiers for each tree label present in the data. For
    instance, with the following data roles specification:

    <\scm-code>
      (list

      \ \ (library \ \ \ DisplayRole DecorationRole UserRole:1)

      \ \ (collection DisplayRole UserRole:1))
    </scm-code>

    we use the data:

    <\scm-code>
      (root

      \ \ (library "Library" "icon.png" 12345

      \ \ \ \ (collection "Cool stuff" 001)

      \ \ \ \ (collection "Things to read" 002)

      \ \ \ \ (collection "Current work" 003

      \ \ \ \ \ \ (collection "Forever current" 004)

      \ \ \ \ \ \ (collection "Read me" 005))))
    </scm-code>

    Notice that the node <scm|root> won't be displayed by the
    <scm|tree-widget> and needs no data roles. Here <scm|UserRole:1> is used
    to store database ids but it can be anything else. The supported data
    roles are:

    <\verbatim-code>
      <\code>
        DisplayRole \ \ \ \ \ \ \ ; a string to be displayed

        EditRole \ \ \ \ \ \ \ \ \ \ ; a string valid for an editable
        representation

        ToolTipRole \ \ \ \ \ \ \ ; a small tooltip to display when the mouse
        hovers over

        StatusTipRole \ \ \ \ \ ; for the status bar (if present and
        supported)

        DecorationRole \ \ \ \ ; file name of an icon to use

        CommandRole \ \ \ \ \ \ \ ; sent to the command executed after
        (double?) clicks

        UserRole:\<less\>number\<gtr\> \ ; left to user definition (will be
        returned as strings)
      </code>
    </verbatim-code>

    <paragraph|Default data roles>It is possible to omit some or all of the
    data role specification. By default the widget will use the tree label's
    string representation for <scm|DisplayRole>, <scm|EditRole>,
    <scm|ToolTipRole> and <scm|StatusTipRole>. For the <scm|DecorationRole>
    it will try to load pixmaps named <shell|treelabel-\<less\>label\<gtr\>.xpm>
    in <shell|$TEXMACS_PIXMAP_PATH>. This search
    <with|font-series|bold|won't> happen if the <scm|DecorationRole> is
    specified (i.e. a full path with or without environment variables and
    wildcards must be given). The default <scm|CommandRole> is the subtree
    itself (see below).

    <paragraph|Using commands>The first argument of <scm|tree-widget>,
    <scm-arg|cmd>, \ is a <scheme> lambda that will be called when items are
    clicked. The procedure must have the following signature:

    <\scm-code>
      (lambda (Event CommandRole . UserRoles) (...))
    </scm-code>

    where:

    <\itemize-dot>
      <item><scm|Event> is an integer: either 1, 2 or 4 for a single, right
      or middle click respectively. In the future, other events could be
      supported (like double clicks, drag&drop, unfold, etc.)

      <item><scm|CommandRole> is either the value of that role if given for
      the data item, or the subtree itself otherwise.

      <item><scm|UserRoles> is a (possibly empty) list with the data for
      those roles given in the data role specification.
    </itemize-dot>

    If multiple selections are enabled and one is made, <scm|CommandRole> and
    <scm|UserRole> will both be lists (not implemented yet). Keep in mind
    that the data is a <TeXmacs> tree and thus not a copy but always a
    pointer to the actual data (unless you copy or transform it into another
    format with e.g. <scm|tree-\<gtr\>stree>)

    <paragraph|Examples>See <scm|widget10> in
    <hlink|<verbatim|menu-test.scm>|$TEXMACS_PATH/progs/kernel/gui/menu-test.scm>
    and ``<hlink|Displaying lists and trees|$TEXMACS_PATH/doc/devel/scheme/gui/scheme-gui-lists-trees.scm>''.
  </explain>

  <paragraph|An example using data roles>

  We build on the previous example, but now we add a command. Notice how the
  way one adds commands to <scm|tree-view> departs from that of other
  widgets, where instead of a procedure one must provide a list with code
  expecting one or two arguments with fixed names (usually <scm|answer> and
  <scm|filter>). <with|font-shape|italic|Note to self>: this is easily
  changed in <scm|$tree-view>, but it seems easier to manage empty arguments
  this way.

  <\session|scheme|default>
    <\input|Scheme] >
      (define t

      \ \ (stree-\<gtr\>tree

      \ \ \ '(root

      \ \ \ \ \ (library "Library" "$TEXMACS_PIXMAP_PATH/tm_german.xpm" 01

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ (collection "Cool stuff" 001)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ (collection "Things to read" 002)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ (collection "Current work" 003

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (collection
      "Forever current" 004)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (collection "Read
      me" 005))))))
    </input>

    <\input|Scheme] >
      (define dd

      \ \ (stree-\<gtr\>tree

      \ \ \ '(list (library DisplayRole DecorationRole UserRole:1)

      \ \ \ \ \ \ \ \ \ \ (collection DisplayRole UserRole:1))))
    </input>

    <\input|Scheme] >
      (define (action clicked cmd-role . user-roles)

      \ \ (display* "clicked= " clicked ", cmd-role= " cmd-role

      \ \ \ \ \ \ \ \ \ \ \ \ ", user-roles= " user-roles "\\n")))
    </input>

    <\input|Scheme] >
      (tm-widget (widget-library)

      \ \ (resize ("150px" "400px" "9000px") ("300px" "600px" "9000px")

      \ \ \ \ (vertical

      \ \ \ \ \ \ (bold (text "Testing tree-view"))

      \ \ \ \ \ \ ===

      \ \ \ \ \ \ (tree-view action t dd))))
    </input>

    <\input|Scheme] >
      (top-window widget-library "Tree View")
    </input>
  </session>

  Notice how we must add <shell|$TEXMACS_PIXMAP_PATH> to the name of the
  pixmap because we are not using the default <scm|DecorationRole>.\ 

  <subparagraph|An example using the buffer tree>

  We can even use the <scm|buffer-tree> as argument to <scm|tree-widget>.
  Changes in the buffer will show up immediately in the widget. In this
  example we use the default data role specification.

  <\warning>
    As of this writing (31 Dec. 2013) the <name|Qt> implementation is sloppy
    and forces a full reloading of the data model for each <cpp|modification>
    of the <cpp|tree>. The slowdown is already noticeable with documents of a
    few pages like this one. Additionally, the current selection in the
    widget is lost after each modification to the buffer (fixing this
    requires writing a fully fledged <cpp|observer> and probably an
    intermediate copy of the data).
  </warning>

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (widget-buffer)

      \ \ (resize ("150px" "400px" "9000px") ("300px" "600px" "9000px")

      \ \ \ \ (vertical

      \ \ \ \ \ \ (bold (text "Testing tree-view"))

      \ \ \ \ \ \ ===

      \ \ \ \ \ \ (tree-view noop (buffer-tree) (stree-\<gtr\>tree
      '(dummy))))))
    </input>

    <\input|Scheme] >
      (top-window widget-buffer "Tree View")
    </input>
  </session>

  <subparagraph|An example with the side tools>

  If your <TeXmacs> has the side tools enabled, you can try this:

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (texmacs-side-tools)

      \ \ (vertical

      \ \ \ \ (hlist (glue #t #f 15 0) (text "Document tree:") (glue #t #f 15
      0))

      \ \ \ \ ---

      \ \ \ \ (tree-view noop (buffer-tree) (stree-\<gtr\>tree '(unused)))))
    </input>
  </session>

  <tmdoc-copyright|2013|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>