<TeXmacs|2.1.4>

<style|<tuple|tmdoc|no-page-numbers|british|cite-sort>>

<\body>
  <tmdoc-title|The cite-sort package>

  <paragraph|How to use the cite-sort package?>

  Click <menu|Document|Style|Add package|Cite|cite-sort> or click
  <menu|<icon|tm_add.svg>|Cite|cite-sort> on the focus toolbar.

  <paragraph|How to cite?>

  Cite two entries: <cite|kleene2002mathematical|10.1145/362686.362692>

  Cite three entries: <cite|kleene2002mathematical|10.1145/362686.362692|\<#897F\>\<#74DC\>\<#4E66\>>

  <paragraph|How to test?>

  <\session|scheme|default>
    <\input|Scheme] >
      (use-modules (utils cite cite-sort-test))
    </input>

    <\input|Scheme] >
      (regtest-cite-sort)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <\bibliography|bib|tm-plain|global.bib>
    <\bib-list|3>
      <bibitem*|1><label|bib-\<#897F\>\<#74DC\>\<#4E66\>>\<#5468\>\<#5FD7\>\<#534E\>.
      <newblock><with|font-shape|italic|\<#673A\>\<#5668\>\<#5B66\>\<#4E60\>>.
      <newblock>\<#6E05\>\<#534E\>\<#5927\>\<#5B66\>\<#51FA\>\<#7248\>\<#793E\>,
      \<#5317\>\<#4EAC\>, 1<localize| edition>, 1 2016.<newblock>

      <bibitem*|2><label|bib-10.1145/362686.362692>Burton<nbsp>H.<nbsp>Bloom.
      <newblock>Space/time trade-offs in hash coding with allowable errors.
      <newblock><with|font-shape|italic|Commun. ACM>, 13(7):422\U426, jul
      1970.<newblock>

      <bibitem*|3><label|bib-kleene2002mathematical>Stephen<nbsp>Cole Kleene.
      <newblock><with|font-shape|italic|Mathematical logic>.
      <newblock>Courier Corporation, 2002.<newblock>
    </bib-list>
  </bibliography>
</body>

<\initial>
  <\collection>
    <associate|page-screen-margin|false>
  </collection>
</initial>

<\attachments>
  <\collection>
    <\associate|bib-bibliography>
      <\db-entry|+M0Oye9e1owShdem|article|10.1145/362686.362692>
        <db-field|contributor|da>

        <db-field|modus|imported>

        <db-field|date|1713253286>
      <|db-entry>
        <db-field|author|Burton H. <name|Bloom>>

        <db-field|title|Space/time trade-offs in hash coding with allowable
        errors>

        <db-field|journal|Commun. ACM>

        <db-field|year|1970>

        <db-field|volume|13>

        <db-field|number|7>

        <db-field|pages|422\U426>

        <db-field|month|jul>

        <db-field|issue_date|July 1970>

        <db-field|publisher|Association for Computing Machinery>

        <db-field|address|New York, NY, USA>

        <db-field|issn|0001-0782>

        <db-field|url|<slink|https://doi.org/10.1145/362686.362692>>

        <db-field|doi|10.1145/362686.362692>

        <db-field|abstract|In this paper trade-offs among certain
        computational factors in hash coding are analyzed. The paradigm
        problem considered is that of testing a series of messages one-by-one
        for membership in a given set of messages. Two new hash-coding
        methods are examined and compared with a particular conventional
        hash-coding method. The computational factors considered are the size
        of the hash area (space), the time required to identify a message as
        a nonmember of the given set (reject time), and an allowable error
        frequency.The new methods are intended to reduce the amount of space
        required to contain the hash-coded information from that associated
        with conventional methods. The reduction in space is accomplished by
        exploiting the possibility that a small fraction of errors of
        commission may be tolerable in some applications, in particular,
        applications in which a large amount of data is involved and a core
        resident hash area is consequently not feasible using conventional
        methods.In such applications, it is envisaged that overall
        performance could be improved by using a smaller core resident hash
        area in conjunction with the new methods and, when necessary, by
        using some secondary and perhaps time-consuming test to \Pcatch\Q the
        small fraction of errors associated with the new methods. An example
        is discussed which illustrates possible areas of application for the
        new methods.Analysis of the paradigm problem demonstrates that
        allowing a small number of test messages to be falsely identified as
        members of the given set will permit a much smaller hash area to be
        used without increasing reject time.>

        <db-field|numpages|5>

        <db-field|keywords|storage layout, scatter storage, retrieval
        efficiency, retrieval trade-offs, searching, storage efficiency, hash
        coding, hash addressing>
      </db-entry>

      <\db-entry|+M0Oye9e1owShdel|book|kleene2002mathematical>
        <db-field|contributor|da>

        <db-field|modus|imported>

        <db-field|date|1713253286>
      <|db-entry>
        <db-field|author|Stephen Cole <name|Kleene>>

        <db-field|title|Mathematical logic>

        <db-field|publisher|Courier Corporation>

        <db-field|year|2002>
      </db-entry>

      <\db-entry|+M0Oye9e1owShdep|book|\<#897F\>\<#74DC\>\<#4E66\>>
        <db-field|contributor|da>

        <db-field|modus|imported>

        <db-field|date|1713253286>
      <|db-entry>
        <db-field|author|<name|\<#5468\>\<#5FD7\>\<#534E\>>>

        <db-field|title|\<#673A\>\<#5668\>\<#5B66\>\<#4E60\>>

        <db-field|publisher|\<#6E05\>\<#534E\>\<#5927\>\<#5B66\>\<#51FA\>\<#7248\>\<#793E\>>

        <db-field|year|2016>

        <db-field|address|\<#5317\>\<#4EAC\>>

        <db-field|edition|1>

        <db-field|month|1>
      </db-entry>
    </associate>
  </collection>
</attachments>