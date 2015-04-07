<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Indexation>

  The purpose of the first extension of the basic database API in
  <scm|index-base.scm> is to permit searching for certain keywords in the
  database entries. The current implementation achieves this by maintaining a
  few additional tables with the list of entries in which given keywords
  occur and counters of how many times a given keyword appears. We also
  maintain a few additional prefix tables which allow us to search for
  uncompleted keywords. The indexation mechanism only indexates
  alphanumerical keywords and normalizes all keywords to lowercase; see
  <verbatim|indexation.cpp> for more details.

  <paragraph|Macros for context specification>

  <\explain>
    <scm|(with-indexing method . body)><explain-synopsis|enable indexation>
  <|explain>
    Specify the indexing method used within the <scm|body>. Currently, the
    <scm|method> can be either <scm|#f> (no indexation), <scm|:basic> (basic
    indexation of the most important fields only) and <scm|#t> (systematic
    indexation of all fields).
  </explain>

  <paragraph|Affected routines of the database API>

  <\explain>
    <scm|(db-set-field id attr vals)><explain-synopsis|set values for a given
    field>

    <scm|(db-set-entry id l)><explain-synopsis|fill out a complete entry>

    <scm|(db-remove-entry id)><explain-synopsis|remove a complete entry>
  <|explain>
    The default behaviour is extended so as to maintain the indexation
    tables.
  </explain>

  <\explain>
    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    Two types of supplementary constraints are introduced: <scm|(:match
    keyword)> and <scm|(:prefix keyword)>. The first constraint only returns
    entries for which <scm|keyword> occurs in one of the indexed fields. In
    the second case, the <scm|keyword> may also occur as a prefix of a
    keyword in one of the indexed fields.
  </explain>

  <paragraph|Other useful routines>

  <\explain>
    <scm|(index-do-indexate? attr)><explain-synopsis|test whether to indexate
    attribute>
  <|explain>
    Tests whether to indexate fields for the specified attribute <scm|attr>
    when using the <scm|:basic> indexing method. The test is done by looking
    up values in the smart table <scm|index-attribute-table>.
  </explain>

  <\explain>
    <scm|(index-get-completions prefix)><explain-synopsis|get possible
    completions of a prefix>
  <|explain>
    Get the list of all possible completions of a <scm|prefix> into a keyword
    which has been indexated.
  </explain>

  <\explain>
    <scm|(index-get-name-completions prefix)><explain-synopsis|name field
    completions of a prefix>
  <|explain>
    Get the list of all names of fields which admit <scm|prefix> as a prefix.
    Contrary to the other indexation routines, the completions are not
    necessarily alphanumerical keywords, and case matters. This routine is
    for instance useful for the tab-completion of names of bibliographic
    entries.
  </explain>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>