<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard titles>

  The <tmdtd|header-title> <abbr|d.t.d.> provides tags for title information.
  The following high-level tags can only be used when encapsulated inside a
  <markup|make-title> tag:

  <\description>
    <expand|item*|<markup|title>>Specify a title for the document.

    <expand|item*|<markup|author>>Specify one or several authors for the
    document.

    <expand|item*|<markup|address>>Specify the address of the author.

    <expand|item*|<markup|address-block>>Specify an address of an author (in
    case of multiple addresses).

    <expand|item*|<markup|title-email>>Specify the email address of the
    author.

    <expand|item*|<markup|title-date>>Specify the creation date of the
    article.
  </description>

  The <markup|title> and <markup|author> use the <markup|header-title> and
  <markup|header-author> tags for specifying the running title and header.
  You may override these by reusing <markup|header-title> <abbr|resp.>
  <markup|header-author>. The above tags also depend on the following
  low-level tags for their physical layout:

  <\description>
    <expand|item*|<markup|title*>>Macro with one argument which specifies the
    physical layout of titles.

    <expand|item*|<markup|author*>>Macro with one argument which specifies
    the physical layout of authors.

    <expand|item*|<markup|address*>>Macro with one argument which specifies
    the physical layout of addresses.

    <expand|item*|<markup|title-email*>>Macro with one argument which
    specifies the physical layout of email addresses.

    <expand|item*|<markup|title-date*>>Macro with one argument which
    specifies the physical layout of creation dates.
  </description>

  The <tmdtd|header-title> <abbr|d.t.d.> also defines the <markup|abstract>
  tag for abstracts of documents.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
