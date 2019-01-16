<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Containers, glue, refresh and co.>

  <subsection|Attribute widgets>

  In what follows <scm-arg|widget> can be anything defined using
  <scm|tm-widget>.

  <\explain>
    <scm|(centered <scm-arg|widget>)><explain-synopsis|centers
    <scm-arg|widget>>
  <|explain>
    This does just that: it centers <scm-arg|widget> with respect to the
    enclosing widget. Although we are calling this an attribute, the effect
    is achieved by using a vertical list and a horizontal one together with
    four <scm|glue> widgets. This means that in the following example, the
    first widget is actually expanded to something like the second one.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (tm-widget (wid1)

        \ \ (centered (text "I'm centered.")))
      <|unfolded-io>
        ((guile-user) (guile-user))
      </unfolded-io>

      <\folded-io|Scheme] >
        (tm-widget (wid2)

        \ \ (vlist

        \ \ \ \ (glue #f #f 0 10)

        \ \ \ \ (hlist

        \ \ \ \ \ \ (glue #t #f 25 0)

        \ \ \ \ \ \ (text "I'm centered.")

        \ \ \ \ \ \ (glue #t #f 25 0))

        \ \ \ \ (glue #f #f 0 10)))
      <|folded-io>
        ((guile-user) (guile-user))
      </folded-io>

      <\input|Scheme] >
        (show wid1)
      </input>

      <\input|Scheme] >
        (show wid2)
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(resize (<scm-arg|w1> <scm-arg|w2> <scm-arg|w3>) (<scm-arg|h1>
    <scm-arg|h2> <scm-arg|h3>) <scm-arg|wid>)>

    <scm|(resize <scm-arg|w> <scm-arg|h> <scm-arg|widget>)><explain-synopsis|resizes
    <scm-arg|widget>>
  <|explain>
    These two variants resize the argument. The first one specifies a minimum
    size of <scm-arg|w1><math|\<times\>><scm-arg|h1>, a default size of
    <scm-arg|w2><math|\<times\>><scm-arg|h2> and a maximum size of
    <scm-arg|w3><math|\<times\>><scm-arg|h3>. <scm-arg|widget> will be set to
    the default size and will be allowed to resize but not beyond the bounds
    specified. The second alternative sets a fixed width and height.

    Sizes are specified as strings with a unit suffix, like in <scm|"150px">.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (tm-widget (wid)

        \ \ (resize "200px" "70px" (text "I'm stuck!")))
      <|unfolded-io>
        \;
      </unfolded-io>

      <\input|Scheme] >
        (show wid)
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(padded <scm-arg|widget>)><explain-synopsis|surrounds
    <scm-arg|widget> by padding>
  <|explain>
    This sets some fixed padding around <scm-arg|widget>. As in the case of
    <scm|centered>, the effect is achieved by means of several widgets into
    which this macro expands. These are actually the same as in the example
    there, but the <scm|glue> widgets are all fixed (i.e. have all their
    expansion parameters set to <scm|#f>).
  </explain>

  <subsection|Container or layout widgets>

  You can arrange widgets horizontally or vertically, or in two column mode
  as in forms. When running the QT version the latter will default to the OS
  standard for arranging labels and their associated input widgets in
  dialogs. Other possibilities are splitters and tabbed widgets. A very
  useful macro is <scm|dynamic>, which allows you to embed one widget into
  another.

  <\explain>
    <scm|(aligned <scm-arg|items-list>)><explain-synopsis|arranges items in a
    two column table>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(hlist <scm-arg|widgets>)> <explain-synopsis|arranges items
    horizontally>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(vlist <scm-arg|widgets>)><explain-synopsis|arranges items
    vertically>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(hsplit (item (<scm-arg|widget>)) (item (<scm-arg|widget>))
    ...)><explain-synopsis|arranges two items in a split panel>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(tabs (tab (<scm-arg|widget>)) (tab (<scm-arg|widget>))
    ...)><explain-synopsis|a tabbed widget>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(dynamic (<scm-arg|widget>))><explain-synopsis|embeds a tm-widget
    into another one>
  <|explain>
    \;
  </explain>

  <subsection|Glue widgets>

  Besides laying out widgets in containers, you will often want to specify
  how they eat up space around them when the user resizes the window. By
  default (most?) widgets take up as much space as they can (i.e. they always
  expand) unless you used <scm|resize> with them or they can have their size
  set with a parameter. If you don't want this to happen you can place
  invisible spacers around them which will (if you tell them to) gobble up as
  much as they can, either vertically or horizontally or in both directions.

  <TeXmacs> provides one such basic building block:

  <\explain>
    <scm|(glue <scm-arg|horiz> <scm-arg|vert> <scm-arg|width>
    <scm-arg|height>)><explain-synopsis|possibly expanding whitespace>
  <|explain>
    The first two parameters, <scm-arg|horiz> and <scm-arg|vert>, are of
    boolean type and specify whether the <scm|glue> widget will try to expand
    horizontally or vertically when its surroundings do. The last two
    parameters, <scm-arg|width> <scm-arg|height>, either fix the size for
    non-expanding <scm|glue> or set a minimum one.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (tm-widget (wid1)

        \ \ (centered (text "I'm centered.")))
      <|unfolded-io>
        \;
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (tm-widget (wid2)

        \ \ (vlist

        \ \ \ \ ===

        \ \ \ \ (hlist

        \ \ \ \ \ \ (glue #t #f 25 0)

        \ \ \ \ \ \ (text "I'm centered.")

        \ \ \ \ \ \ (glue #t #f 25 0))

        \ \ \ \ (glue #f #f 0 10)))
      <|unfolded-io>
        \;
      </unfolded-io>

      <\input|Scheme] >
        (show wid1)
      </input>

      <\input|Scheme] >
        (show wid2)
      </input>
    </session>
  </explain>

  In addition to the basic <scm|glue> widget, there are several convenience
  macros.

  <\explain>
    <scm|===><explain-synopsis|vertical separator>
  <|explain>
    Expands to <scm|(glue #f #f 0 5)>.
  </explain>

  <\explain>
    <scm|======><explain-synopsis|big vertical separator>
  <|explain>
    Expands to <scm|(glue #f #f 0 15)>.
  </explain>

  <\explain>
    <scm|//><explain-synopsis|horizontal separator>
  <|explain>
    Expands to <scm|(glue #f #f 5 0)>.
  </explain>

  <\explain>
    <scm|///><explain-synopsis|big horizontal separator>
  <|explain>
    Expands to <scm|(glue #f #f 15 0)>.
  </explain>

  <\explain>
    <scm|\<gtr\>\<gtr\>><explain-synopsis|expanding horizontal separator>
  <|explain>
    Expands to <scm|(glue #t #f 5 0)>.
  </explain>

  <\explain>
    <scm|\<gtr\>\<gtr\>\<gtr\>><explain-synopsis|big expanding horizontal
    separator>
  <|explain>
    Expands to <scm|(glue #t #f 15 0)>.
  </explain>

  For the specific use in menus the following two macros are defined:

  <\explain>
    <scm|\|><explain-synopsis|horizontal separator>
  <|explain>
    (A vertical bar)
  </explain>

  <\explain>
    <scm|---><explain-synopsis|vertical separator>
  <|explain>
    (Three dashes)
  </explain>

  <subsection|Refresh widgets>

  Refresh widgets redraw their contents every time a command is executed.
  They achieve this re-evaluating the code for the whole widget, so you can
  have new values in your variables...

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