<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Automatic content generation>

  The <tmdtd|std-automatic> <abbr|d.t.d.> specifies for the automatic
  generation of auxiliary content like tables of contents and bibliographies,
  as well as for the presentation of such auxiliary content. The following
  tags are used for bibliographies:

  <\description>
    <expand|item*|<markup|cite>>A function with an arbitrary number of
    arguments. Each argument is a citation corresponding to an item in a
    BiB-<TeX> file. The citations are displayed in the same way as they are
    referenced in the bibliography and they also provide hyperlinks to the
    correspoding references. The citations are displayed as question marks if
    you did not generate the bibliography.

    <expand|item*|<markup|nocite*>>Similar as <markup|cite>, but the
    citations are not displayed in the main text.

    <expand|item*|<markup|bibitem*>>A function which specifies how to display
    an item in the bibliography.
  </description>

  The following tags are used for compiling tables of contents:

  <\description>
    <expand|item*|<markup|toc-main-1>>A function with one argument for
    creating primordial entry in the table of contents. This function can for
    instance be used when a book consists of several parts.

    <expand|item*|<markup|toc-main-2>>A function with one argument for
    creating a main entry in the table of contents. This function is usually
    used for chapters.

    <expand|item*|<markup|toc-normal-1>>A function with one argument for
    creating a normal entry in the table of contents. This function is often
    used for sections.

    <expand|item*|<markup|toc-normal-2>>Similar as <markup|toc-normal-2> for
    less important entries like subsections.

    <expand|item*|<markup|toc-normal-3>>Similar as <markup|toc-normal-3> for
    even less important entries like subsubsections.

    <expand|item*|<markup|toc-small-1>>Used for not very important entries
    such as paragraphs (may be ignored).

    <expand|item*|<markup|toc-small-2>>Used for even less important entries
    such as subparagraphs.

    <expand|item*|<markup|toc-dots>>The separation between an entry in the
    table of contents and the corresponding page number. By default, we use
    horizontal dots.
  </description>

  The following tags are used for indices:

  <\description>
    <expand|item*|<markup|index>>A function with one argument <var|x>, which
    inserts <var|x> in the index as a principal entry.

    <expand|item*|<markup|subindex>>A function with two arguments <var|x> and
    <var|y>, which inserts <var|y> in the index as a subentry of <var|x>.

    <expand|item*|<markup|subsubindex>>A function with three arguments
    <var|x>, <var|y> and <var|z>, which inserts <var|z> in the index as a
    subentry of <var|y>, which is itself a subentry of <var|x>.

    <expand|item*|<markup|index-complex>>A function with four arguments
    <var|key>, <var|how>, <var|range>, <var|entry>, which is documented in
    the section about <apply|hyper-link|index
    generation|../../links/man-index.en.tm>.

    <expand|item*|<markup|index-line>>This function takes a <var|key>
    argument, which tells how to sort the entry, and the actual <var|entry>.
    No page number is generated.

    <expand|item*|<markup|index-1>>Macro with an index entry and a page
    number, which is used for rendering a principal index entry in the index.

    <expand|item*|<markup|index-1*>>Similar to <markup|index-1>, but without
    the page number.

    <expand|item*|<markup|index-<with|mode|math|n>>>(with <with|mode|math|n>
    between <with|mode|math|1> and <with|mode|math|5>): macro with an index
    entry and a page number, which is used for rendering an index entry of
    level <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Similar to
    <markup|index-<with|mode|math|n>>, but without the page number.

    <expand|item*|<markup|index-dots>>The macro which produces the dots
    between an index entry and the corresponding page number(s).
  </description>

  The following tags are used for glossaries:

  <\description>
    <expand|item*|<markup|glossary>>A function which inserts its only
    argument into the glossary.

    <expand|item*|<markup|glossary-dup>>For creating an additional page
    number for an entry which was already inserted before.

    <expand|item*|<markup|glossary-explain>>A function for inserting a
    glossary entry with its explanation.

    <expand|item*|<markup|glossary-line>>Insert a glossary entry without a
    page number.

    <expand|item*|<markup|glossary-1>>Macro for rendering a glossary entry
    and its corresponding page number.

    <expand|item*|<markup|glossary-2>>Macro for rendering a glossary entry,
    its explanation, and its page number.

    <expand|item*|<markup|glossary-dots>>The macro which produces the dots
    between a glossary entry and the corresponding page number(s).
  </description>

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
