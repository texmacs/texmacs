<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|An introduction to widgets, menus, dialogs and forms>

  <section|Widgets><label|sec:widgets>

  In <TeXmacs> you create and extend the visual interface using <em|widgets>.
  This word means either the basic building blocks you have at your disposal,
  like buttons, popup lists, etc. or the collections of those into dialogs,
  menu bars or whatever. This rather loose concept might be confusing,
  especially when we refer to what usually are know as dialogs as widgets,
  but it makes sense because all sorts of widgets can be aggregated to build
  more complicated ones as well.<\footnote>
    If you miss some particular ``building block'' from your OS, you might
    see whether it's feasible as an aggregation of simpler ones or try and
    play with the UI interface code in C++ (but you'll have to add it for
    every supported platform!).
  </footnote>

  However, it must be kept in mind that items intended to be inserted in a
  menu bar won't necessarily display as they do in a separate window:
  complicated aggregations of widgets might be better placed in a separate
  window or dialogue.

  A complete reference with all the available widgets is
  <hlink|here|scheme-gui-reference.en.tm>, some more examples are here and
  here.

  To create a widget, you'll want to use <scm|tm-widget> to define a new one.
  The call to this function uses its particular syntax, with many keywords
  for the creation of widgets. You can see the whole list of keywords in the
  table <scm|gui-make-table> inside <hlink|menu-define.scm|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>,
  but we'll start with some buttons.\ 

  <subsection|Buttons and labels>

  Execute the following two lines to get the unavoidable example and leave
  your mouse over the ``Hello'' button.

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (example1) ("Hello" "world!"))
    </input>

    <\input|Scheme] >
      (top-window example1 "A first try")
    </input>
  </session>

  As you can see, buttons are implicitly created by simply writing a list
  with the button's title and a tooltip to be displayed when the user hovers
  over the button. A bit confusing, and also ugly, because this is intended
  for <with|font-shape|italic|toolbar> buttons. What you probably want is
  this:

  <\session|scheme|default>
    <\input|Scheme] >
      (tm-widget (example2) (explicit-buttons ("Hello" (noop))))
    </input>

    <\input|Scheme] >
      (top-window example2 "A nicer button")
    </input>
  </session>

  The second argument is now a <scheme> command to be executed when the user
  clicks the button, in this case a no-operation, or <scm|(noop)>. Try
  changing it for <scm|(display "World")> or anything that suits you.

  The next step is to add some text next to the button, i.e. a label. This is
  done with the <scm|text> keyword, as in <scm|(text "Hello")>, but in order
  to have both widgets sit side by side, you'll need a <hlink|container
  widget|scheme-gui-container.en.tm>, such as <scm|hlist>:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (example3)

      \ \ (hlist\ 

      \ \ \ \ (text "Hello")\ 

      \ \ \ \ (explicit-buttons ("world" (display "!\\n")))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window example3 "Some text")
    </input>
  </session>

  That was nice, but as you see, the two widgets are packed together until
  you resize the window. We need to explicitly tell <TeXmacs> to insert some
  space between them:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (example3)

      \ \ (hlist\ 

      \ \ \ \ (text "Hello")

      \ \ \ \ \<gtr\>\<gtr\>\<gtr\>

      \ \ \ \ (explicit-buttons ("world" (display "!\\n")))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window example3 "Some text")
    </input>
  </session>

  The special symbol <scm|\<gtr\>\<gtr\>\<gtr\>> is just one of the
  predefined <hlink|glue widgets|scheme-gui-advanced.en.tm>.

  <section|Menus>

  As we said before, menus are special collections of widgets:

  <with|color|red|Problems with toolbars, system menus, context menus... Menu
  containers: horizontal menu, vertical menu. Separators.>

  <section|More complex widgets>

  In order to create more complex layouts you'll need a few containers. Among
  these are <scm|aligned> and <scm|tabs>. A very useful macro is
  <scm|dynamic>: it allows you to embed one widget into another.

  <subsection|User dialogs><label|sec:dialogs>

  Let's see how you create a dialog. To get started here is one little
  example taken from <hlink|menu-test.scm|$TEXMACS_PATH/progs/kernel/gui/menu-test.scm>:

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

  The keyword <scm|centered> is clear, but <scm|aligned> not so much: it
  builds two column tables, with each row of type <scm|item>. As you can see,
  each <scm|item> takes two arguments, which can be of
  <with|font-shape|italic|any> type.

  The <scm|toggle> is another example of a widget which triggers a <scheme>
  command whenever it's clicked, or toggled in this case. The second argument
  stands for the default state of the <scm|toggle>.

  Again, in order to display this you create a <scm|top-window> and give it a
  title.

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
      \;
    </unfolded-io>

    <\input|Scheme] >
      (top-window widget1 "A bigger window")
    </input>
  </session>

  <scm|resize> is one of the several available container or <hlink|content
  management widgets|scheme-gui-container.en.tm>. It accepts two sorts of
  arguments. Either one sets a fixed size for the widget with two strings, as
  in the example above, or one passes two lists, the first for widths, the
  second for heights, with the minimum, default and maximum values in that
  order, like this:\ 

  <scm|(resize ("100px" "200px" "400px") ("100px" "200px" "400px")
  (some-widget-here))>

  This sets <scm|some-widget-here> to have a default square size of 200x200
  pixels.

  If you want to add the usual buttons you use <scm|bottom-buttons> like in
  the following example. Notice that the widget now accepts one parameter
  <scm|cmd> which will be called when the user clicks the ``Ok'' button.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (widget1-buttons cmd)

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
  display it, namely <scm|dialogue-window>, which will also close the window
  after the button has been clicked.

  <\session|scheme|default>
    <\input|Scheme] >
      (dialogue-window widget1-buttons (lambda (arg) (display* arg "\\n"))
      "Two toggles")
    </input>
  </session>

  That special <scm|\<gtr\>\<gtr\>> at the end of the widget inserts as
  before whitespace, but it stretches and aligns the <scm|bottom-buttons> to
  the right. This is just another example of a <hlink|glue
  widget|scheme-gui-advanced.en.tm>.

  <subsection|Composite widgets>

  Note that our second dialog, <scm|widget1-buttons> is just a copy of
  <scm|widget1> with an extra line at the end. We could have spared us the
  keytrokes in this way:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-widget (widget1-buttons-smarter cmd)

      \ \ (dynamic (widget1))

      \ \ (bottom-buttons \<gtr\>\<gtr\> ("Ok" (cmd "Ok"))))
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (dialogue-window widget1-buttons-smarter (lambda (arg) (display* arg
      "\\n")) "Two toggles")
    </input>

    \;
  </session>

  As you can see, the approach we've shown has a shortcoming: there's no way
  to access all the values of the different widgets in your dialog at the
  same time. Of course you can use the function <scm|cmd> passed to your
  widget to perform some computations, but in case you need to retrieve or
  store complicated data, what you need is a form (See
  <reference|sec:forms>).

  <section|Forms><label|sec:forms>

  As explained in <reference|interface:dialogs> the available widgets can be
  used to create menu items and dialog windows, but you may want to create
  more complex dialogs to perform more complicated tasks. Forms provide one
  mechanism to do this. They allow you to define multiple named fields of
  several types, whose values are stored in a hash table You can retrieve and
  return the values of this hash when the user clicks a button using the
  functions <scm|form-fields> and <scm|form-values>.

  In the following example you can see that the syntax is pretty much the
  same as for regular widgets, but you must prefix the keywords with
  <scm|form-> :

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
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
    <|unfolded-io>
      \;
    </unfolded-io>

    <\input|Scheme] >
      (dialogue-window form3 (lambda (x) (display* x "\\n")) "Test of form3")
    </input>
  </session>

  A complete list of the widgets you can embed in a form is in the table
  <scm|gui-make-table> inside <hlink|menu-define.scm|$TEXMACS_PATH/progs/kernel/gui/menu-define.scm>.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>