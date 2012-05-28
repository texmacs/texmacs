<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|Containers, glue, refresh and cia.>

  <section|Attribute widgets>

  Setting attributes of widgets is achieved by enclosing them in the
  following special widgets:

  <scm|centered>, <scm|resize>, <scm|padded>, ...\ 

  <section|Container widgets>

  You can arrange widgets horizontally or vertically, or in two column mode
  as in forms. When running the QT version the latter will default to the OS
  standard for arranging labels and their associated input widgets in
  dialogs.

  <scm|aligned>, <scm|hlist>, <scm|vlist>, <scm|hsplit>, ...

  <section|Glue widgets>

  Shifting of widgets... From the definitions:

  <\verbatim>
    ((== x '---) '$---)

    ((== x '===) (gui-make '(glue #f #f 0 5)))

    ((== x '======) (gui-make '(glue #f #f 0 15)))

    ((== x '/) '$/)

    ((== x '//) (gui-make '(glue #f #f 5 0)))

    ((== x '///) (gui-make '(glue #f #f 15 0)))

    ((== x '\<gtr\>\<gtr\>) (gui-make '(glue #t #f 5 0)))

    ((== x '\<gtr\>\<gtr\>\<gtr\>) (gui-make '(glue #t #f 15 0)))

    ((== x (string-\<gtr\>symbol "\|")) '$/)
  </verbatim>

  <section|Refresh widgets>

  Refresh widgets reevaluate their contents every time a command is
  executed...

  <section|Other topics>

  \;

  \;
</body>