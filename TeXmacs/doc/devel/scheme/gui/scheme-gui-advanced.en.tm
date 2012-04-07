<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|Containers, glue, refresh and cia.>

  <section|Container widgets>

  <scm|centered>

  <\scm>
    aligned
  </scm>

  <scm|hlist>

  <scm|vlist>

  <scm|padded>

  <section|Glue widgets>

  From the definitions...

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
</body>