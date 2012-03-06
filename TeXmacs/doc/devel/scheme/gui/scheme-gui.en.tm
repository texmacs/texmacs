<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating new interface items>

  Most of the user interface to <TeXmacs> is dynamically created from within
  the interpreted scheme code. Imagine you want to implement some feature
  which requires interaction with the user. One possible approach is to use
  the facility <scm|interactive>, which will either popoup a dialog or ask in
  the footer bar, based in metadata you provide inside your <scm|tm-define>'d
  function. See <with|color|red|here> for more on this topic. However,
  automatically generated stuff is not always the best approach, so you might
  want to explicitly design your interface placing it inside a complicated
  dialog.

  <section|User dialogs><label|interface:dialogs>

  In <TeXmacs> you create visual interfaces using <em|widgets>. This word
  means either the basic building blocks you have at your disposal, like
  buttons, popup lists, etc. or the collections of those into dialogs, menus
  or whatever. The latter can be aggregated to build more complicated ones as
  well.<\footnote>
    If you miss some particular ``building block'' from your OS, you might
    see whether it's feasible as an aggregation of simpler ones or try and
    play with the UI interface code in C++ (but you'll have to add it for
    every supported platform!).
  </footnote>

  Let's see how you create a dialog. First you'll want to use <scm|tm-widget>
  to define a new widget. The call to this function uses its particular
  syntax, with many keywords for the creation of widgets. You can see the
  whole list in the table <scm|gui-make-table> inside
  <hlink|menu-define.scm|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>.
  Here is one little example taken from <hlink|menu-test.scm|$TEXMACS_PATH/progs/kernel/gui/menu-test.scm>:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (widget1)

      \ \ (centered

      \ \ \ \ (aligned

      \ \ \ \ \ \ (item (text "First:")

      \ \ \ \ \ \ \ \ (toggle (display* "First " answer "\\n") #f))

      \ \ \ \ \ \ (item (text "Second:")

      \ \ \ \ \ \ \ \ (toggle (display* "Second " answer "\\n") #f)))))
    <|unfolded-io>
      \;
    </unfolded-io>
  </session>

  In order to display this you create a <scm|top-window> and give it a title.

  <\session|scheme|default>
    <\input|Scheme] >
      (top-window widget1 "Two toggle widgets")
    </input>
  </session>

  You'll notice that the created window is too small and the title is not
  wholly displayed. You can force it to be of a certain size using
  <scm|resize>:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (widget1)

      \ \ (centered

      \ \ \ \ (resize "500px" "200px"

      \ \ \ \ \ \ (aligned

      \ \ \ \ \ \ \ \ (item (text "First:")

      \ \ \ \ \ \ \ \ \ \ (toggle (display* "First " answer "\\n") #f))

      \ \ \ \ \ \ \ \ (item (text "Second:")

      \ \ \ \ \ \ \ \ \ \ (toggle (display* "Second " answer "\\n") #f))))))
    <|unfolded-io>
      ((guile-user) (guile-user))
    </unfolded-io>

    <\input|Scheme] >
      (top-window widget1 "A bigger window")
    </input>
  </session>

  <with|color|red|This should of course work!>

  If you want to add the usual buttons you use <scm|bottom-buttons> like in
  the following example. Notice the widget now accepts one parameter
  <scm|cmd> which will be called when the user clicks the ``Ok'' button. This
  will also automatically close the window.\ 

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (form2 cmd)

      \ \ (centered

      \ \ \ \ (aligned

      \ \ \ \ \ \ (item (text "First:")

      \ \ \ \ \ \ \ \ (toggle (display* "First " answer "\\n") #f))

      \ \ \ \ \ \ (item (text "Second:")

      \ \ \ \ \ \ \ \ (toggle (display* "Second " answer "\\n") #f))))

      \ \ (bottom-buttons \<gtr\>\<gtr\> ("Ok" (cmd "Ok"))))
    <|unfolded-io>
      \;
    </unfolded-io>
  </session>

  Since the widget now needs an argument, we must use another function to
  display it, namely <scm|dialogue-window>, like this:

  <\session|scheme|default>
    <\input|Scheme] >
      (dialogue-window form2 (lambda (arg) (display* arg "\\n")) "Two
      toggles")
    </input>
  </session>

  As you can see, this approach has a shortcoming: there's no way to access
  all the values of the different widgets in your dialog at the same time. Of
  course you can use the function <scm|cmd> passed to your widget to perform
  some computations, but in case you need to retrieve or store complicated
  data, what you need is a form (See <reference|interface:forms>).

  <section|Forms><label|interface:forms>

  As explained in <reference|interface:dialogs> the available widgets can be
  used to create menu items and dialog windows, but you may want to create
  more complex dialogs to perform more complicated tasks. Forms provide one
  mechanism to do this. They allow you to define multiple named fields of
  several types, whose values are stored in a <with|color|red|hash table>.
  You can retrieve and return the values of this hash when the user clicks a
  button using the functions <scm|form-fields> and <scm|form-values>.

  In the following example you can see that the syntax is pretty much the
  same as for regular widgets, but you must prefix your widgets with
  <scm|form-> :

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (form3 cmd)

      \ \ (form "Test"

      \ \ \ \ (centered

      \ \ \ \ \ \ (aligned

      \ \ \ \ \ \ \ \ (item (text "Input:")

      \ \ \ \ \ \ \ \ \ \ (form-input "fieldname1" "string" '("one" "two"
      "three") "1w"))

      \ \ \ \ \ \ \ \ (item (text "Enum:")

      \ \ \ \ \ \ \ \ \ \ (form-enum "fieldname2" '("one" "two" "three")
      "two" "1w"))

      \ \ \ \ \ \ \ \ (item (text "Choice")

      \ \ \ \ \ \ \ \ \ \ (form-choice "fieldname3" '("one" "two" "three")
      "one"))

      \ \ \ \ \ \ \ \ (item (text "Choices")

      \ \ \ \ \ \ \ \ \ \ (form-choices "fieldname4"\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two" "three")\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two"))))

      \ \ \ \ \ \ (bottom-buttons

      \ \ \ \ \ \ \ \ ("Cancel" (cmd "cancel")) \<gtr\>\<gtr\>

      \ \ \ \ \ \ \ \ ("Ok"

      \ \ \ \ \ \ \ \ \ (display* (form-fields) " -\<gtr\> " (form-values)
      "\\n")

      \ \ \ \ \ \ \ \ \ (cmd "ok"))))))
    <|unfolded-io>
      ((guile-user) (guile-user))
    </unfolded-io>

    <\input|Scheme] >
      (dialogue-window form3 (lambda (x) (display* x "\\n")) "Test of form3")
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  A complete list of the widgets you can embed in a form is in the table
  <scm|gui-make-table> inside <hlink|menu-define.scm|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>.

  \;
</body>