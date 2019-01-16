<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Forms>

  As explained in \P<hlink|Dialogs and composite
  widgets|scheme-gui-dialogs.en.tm>\Q the available widgets can be used to
  compose dialog windows which perform one simple task. But sometimes one
  needs to read complex input from the user and forms provide one mechanism
  to do this. They allow you to define multiple named fields of several
  types, whose values are stored in a hash table. The contents of this hash
  can be retrieved when the user clicks a button using the functions
  <scm|form-fields> and <scm|form-values>.

  In the following example you can see that the syntax is pretty much the
  same as for regular widgets, but you must prefix the keywords with
  <scm|form-> :

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tm-widget (form3 cmd)

      \ \ (resize "500px" "500px"

      \ \ \ \ (padded

      \ \ \ \ \ \ (form "Test"

      \ \ \ \ \ \ \ \ (aligned

      \ \ \ \ \ \ \ \ \ \ (item (text "Input:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-input "fieldname1" "string" '("one")
      "1w"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Enum:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-enum "fieldname2" '("one" "two" "three")
      "two" "1w"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Choice:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-choice "fieldname3" '("one" "two"
      "three") "one"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Choices:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-choices "fieldname4"\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two"
      "three")\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two"))))

      \ \ \ \ \ \ \ \ (bottom-buttons

      \ \ \ \ \ \ \ \ \ \ ("Cancel" (cmd "cancel")) \<gtr\>\<gtr\>

      \ \ \ \ \ \ \ \ \ \ ("Ok"

      \ \ \ \ \ \ \ \ \ \ \ (display* (form-fields) " -\<gtr\> "
      (form-values) "\\n")

      \ \ \ \ \ \ \ \ \ \ \ (cmd "ok")))))))
    <|folded-io>
      \;
    </folded-io>

    <\input|Scheme] >
      (dialogue-window form3 (lambda (x) (display* x "\\n")) "Test of form3")
    </input>
  </session>

  A complete list of the widgets you can embed in a form is in the table
  <scm|gui-make-table> inside <hlink|<verbatim|menu-define.scm>|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>