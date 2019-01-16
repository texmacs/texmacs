<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Dialogs and composite widgets>

  <with|font-shape|italic|Dialogs> are collections of widgets arranged in a
  window in order to perform a common task. You might want to create one of
  this in order to configure or interact with a plugin: add some
  configuration options as well as some common actions and have the window
  always open besides your document. A good example whose code might help is
  the preferences dialog <scm|(open-preferences)>.

  In order to create more complex layouts than those we did before you'll
  need a few containers. Among these are <scm|aligned> and <scm|tabs>, which
  we explain below. A very useful macro which you'll be using often is
  <scm|dynamic>: it allows you to embed one widget into another.

  Let's see how you create a dialog. To get started here is one little
  example taken from <hlink|<verbatim|menu-test.scm>|$TEXMACS_PATH/progs/kernel/gui/menu-test.scm>:

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

  The keyword <scm|centered> is clear, just center whatever it contains, but
  <scm|aligned> not so much: it builds two column tables, with each row of
  type <scm|item>. As you can see, each <scm|item> takes two arguments, which
  can be of <with|font-shape|italic|any> type.

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

  <scm|resize> is another of the several available container or
  <hlink|content management widgets|scheme-gui-container.en.tm>. It accepts
  two sorts of arguments. Either one sets a fixed size for the widget with
  two strings, as in the example above, or one passes two lists, the first
  for widths, the second for heights, with the minimum, default and maximum
  values in that order, like this:\ 

  <scm|(resize ("100px" "200px" "400px") ("100px" "200px" "400px")
  (some-widget-here))>

  This sets <scm|some-widget-here> to have a default square size of 200x200
  pixels.

  If you want to add the usual buttons you use <scm|bottom-buttons> like in
  the following example. Notice that the widget now accepts one parameter
  <scm|cmd> which will be called when the user clicks the \POk\Q button.

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
  keystrokes in this way:

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
  store complicated data, what you need is a form.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>