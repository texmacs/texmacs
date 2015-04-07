<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Formats of database entries>

  From the high-level point of view, it is often useful to manipulate
  database values as other objects than raw strings. In <scm|db-format.scm>,
  an additional layer is added to the basic database API which allows you to
  specify encoding and decoding schemes between raw strings and user defined
  data formats for specific types of entries and attributes. More precisely,
  the extension introduces the following tables:

  <\description>
    <item*|<scm|db-kind-table>>For each ``kind'' of database (<abbr|e.g.>
    <scm|"bib"> for bibliographies), specify a finite list of supported entry
    types (<abbr|e.g.> <scm|"article">, <scm|"book">, <abbr|etc.> for
    bibliographies).

    <item*|<scm|db-format-table>>For each entry type, specify the format of
    the fields: mandatory fields, alternative fields and/or optional fields.
    See <scm|bib-db.scm> for examples.

    <item*|<scm|db-encoding-table>>For each triple <scm|(attr type enc)> of
    an attribute, an entry type and an encoding scheme, specify a keywords
    which specifies how to encode/decode field values. The two possible
    values implemented in <scm|db-format.scm> are <scm|:identity> and
    <scm|:texmacs>.

    <item*|<scm|db-encoder-table>, <scm|db-decoder-table>> For the keyword
    values in <scm|db-encoding-table>, specify the actual routines to be used
    for encoding and decoding.
  </description>

  <paragraph|Macros for context specification>

  <\explain>
    <scm|(with-encoding enc . body)><explain-synopsis|specify
    encoding/decoding scheme>
  <|explain>
    Specify the encoding/decoding scheme <scm|enc> to be used inside
    <scm|body>. This value is used as the third component of triples in
    <scm|db-encoding-table>. Implemented encoding/decoding schemes are
    <scm|#f> (no encoding/decoding) and <scm|:default>.
  </explain>

  <paragraph|Affected routines of the database API>

  <\explain>
    <scm|(db-set-field id attr vals)><explain-synopsis|set values for a given
    field>

    <scm|(db-get-field id attr)><explain-synopsis|get all values for a given
    field>

    <scm|(db-set-entry id l)><explain-synopsis|fill out a complete entry>

    <scm|(db-get-entry id)><explain-synopsis|retrieve a complete entry>

    <scm|(db-remove-entry id)><explain-synopsis|remove a complete entry>

    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    The default behaviour is customized so as to encode/decode field values
    according to the current encoding/decoding scheme.
  </explain>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>