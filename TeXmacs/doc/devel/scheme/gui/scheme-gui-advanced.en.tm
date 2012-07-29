<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|Containers, glue, refresh and cia.>

  <section|Attribute widgets>

  Setting attributes of widgets is achieved by enclosing them in the
  following special widgets.

  <\explain>
    <scm|(centered wid)><explain-synopsis|centers the widget <scm|wid>>
  <|explain>
    This does just that: it centers the widget <scm|wid> with respect to the
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
    <scm|(resize (w1 w2 w3) (h1 h2 h3) wid)>

    <scm|(resize w h wid)><explain-synopsis|resizes the widget <scm|wid>>
  <|explain>
    These two variants resize the argument. The first one specifies a minimum
    size of <scm|w1 x h1>, a default size of <scm|w2 x h2> and a maximum size
    of <scm|w3 x h3>. The widget <scm|wid> will be set to the default size
    and will be allowed to resize but not beyond the bounds specified. The
    second alternative sets a fixed width and height.

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
    <scm|(padded wid)><explain-synopsis|surrounds the widget <scm|wid> by
    padding>
  <|explain>
    This sets some fixed padding around <scm|wid>. As in the case of
    <scm|centered>, the effect is achieved by means of several widgets into
    which this macro expands. These are actually the same as in the example
    there, but the <scm|glue> widgets are all fixed (i.e. have all their
    expansion paramenters set to <scm|#f>).
  </explain>

  <section|Container or layout widgets>

  You can arrange widgets horizontally or vertically, or in two column mode
  as in forms. When running the QT version the latter will default to the OS
  standard for arranging labels and their associated input widgets in
  dialogs. Other possibilites are splitters and tabbed widgets. A very useful
  macro is <scm|dynamic>, which allows you to embed one widget into another.

  <\explain>
    <scm|(aligned items-list)><explain-synopsis|arranges items in a two
    column table>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(hlist widgets)><explain-synopsis|arranges items horizontally>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(vlist widgets)><explain-synopsis|arranges items vertically>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(hsplit (item ) (item ) ...)><explain-synopsis|arranges two items in
    a variable size split panel>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(tabs (tab ) (tab ) ...)><explain-synopsis|a tabbed widget>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(dynamic (wid))><explain-synopsis|embeds a tm-widget into another
    one>
  <|explain>
    <scm|wid> can be any widget defined using <scm|tm-define>.
  </explain>

  <section|Glue widgets>

  Besides laying out widgets in containers, you will often want to specifiy
  how they eat up space around them when the user resizes the window. By
  default (most?) widgets take up as much space as they can (i.e. they always
  expand) unless you used <scm|resize> with them. If you don't want this to
  happen you can place invisible spacers around them which will (if you tell
  them to) gobble up as much as they can, either vertically or horizontally
  or in both directions.

  <TeXmacs> provides one such basic building block:

  <\explain>
    <scm|(glue horiz vert width height)><explain-synopsis|possibly expanding
    whitespace>
  <|explain>
    The first two parameters are of type <scm|bool> and specify whether the
    <scm|glue> widget will try to expand horizontally or vertically when its
    surroundings do. The last two parameters either fix the size for
    non-expanding <scm|glue> or set a minimum one.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (tm-widget (wid1)

        \ \ (centered (text "I'm centered.")))
      <|unfolded-io>
        ((guile-user) (guile-user))
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
        ((guile-user) (guile-user) (guile-user) (guile-user))
      </unfolded-io>

      <\input|Scheme] >
        (show wid1)
      </input>

      <\input|Scheme] >
        (show wid2)
      </input>

      <\input|Scheme] >
        \;
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
    (That's a vertical bar: <scm|\|>)
  </explain>

  <\explain>
    <scm|---><explain-synopsis|vertical separator>
  <|explain>
    (That's <with|font-series|bold|three> dashes. If you see only two, that's
    a bug which hasn't yet been fixed)
  </explain>

  <section|Refresh widgets>

  Refresh widgets reevaluate their contents every time a command is
  executed...

  <section|Composite widgets>

  \;

  \;
</body>