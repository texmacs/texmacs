<TeXmacs|1.0.2.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Automatic content generation>

  The <tmdtd|std-automatic> <abbr|d.t.d.> specifies for the automatic
  generation of auxiliary content like tables of contents and bibliographies,
  as well as for the presentation of such auxiliary content. The following
  tags are used for bibliographies:

  <\description>
    <item*|<markup|cite>>A function with an arbitrary number of arguments.
    Each argument is a citation corresponding to an item in a BiB-<TeX> file.
    The citations are displayed in the same way as they are referenced in the
    bibliography and they also provide hyperlinks to the correspoding
    references. The citations are displayed as question marks if you did not
    generate the bibliography.

    <item*|<markup|nocite*>>Similar as <markup|cite>, but the citations are
    not displayed in the main text.

    <item*|<markup|cite-detail>>A function with two arguments. The first one
    corresponds to a BiB-<TeX> item and the second one to some additional
    information like a chapter or a page number.

    <item*|<markup|bibitem*>>A function which specifies how to display an
    item in the bibliography.
  </description>

  The following tags are used for compiling tables of contents:

  <\description>
    <item*|<markup|toc-main-1>>A function with one argument for creating
    primordial entry in the table of contents. This function can for instance
    be used when a book consists of several parts.

    <item*|<markup|toc-main-2>>A function with one argument for creating a
    main entry in the table of contents. This function is usually used for
    chapters.

    <item*|<markup|toc-normal-1>>A function with one argument for creating a
    normal entry in the table of contents. This function is often used for
    sections.

    <item*|<markup|toc-normal-2>>Similar as <markup|toc-normal-2> for less
    important entries like subsections.

    <item*|<markup|toc-normal-3>>Similar as <markup|toc-normal-3> for even
    less important entries like subsubsections.

    <item*|<markup|toc-small-1>>Used for not very important entries such as
    paragraphs (may be ignored).

    <item*|<markup|toc-small-2>>Used for even less important entries such as
    subparagraphs.

    <item*|<markup|toc-dots>>The separation between an entry in the table of
    contents and the corresponding page number. By default, we use horizontal
    dots.
  </description>

  The following tags are used for indices:

  <\description>
    <item*|<markup|index>>A function with one argument <var|x>, which inserts
    <var|x> in the index as a principal entry.

    <item*|<markup|subindex>>A function with two arguments <var|x> and
    <var|y>, which inserts <var|y> in the index as a subentry of <var|x>.

    <item*|<markup|subsubindex>>A function with three arguments <var|x>,
    <var|y> and <var|z>, which inserts <var|z> in the index as a subentry of
    <var|y>, which is itself a subentry of <var|x>.

    <item*|<markup|index-complex>>A function with four arguments <var|key>,
    <var|how>, <var|range>, <var|entry>, which is documented in the section
    about <hyper-link|index generation|../../links/man-index.en.tm>.

    <item*|<markup|index-line>>This function takes a <var|key> argument,
    which tells how to sort the entry, and the actual <var|entry>. No page
    number is generated.

    <item*|<markup|index-1>>Macro with an index entry and a page number,
    which is used for rendering a principal index entry in the index.

    <item*|<markup|index-1*>>Similar to <markup|index-1>, but without the
    page number.

    <item*|<markup|index-<with|mode|math|n>>>(with <with|mode|math|n> between
    <with|mode|math|1> and <with|mode|math|5>): macro with an index entry and
    a page number, which is used for rendering an index entry of level
    <with|mode|math|n>.

    <item*|<markup|index-<with|mode|math|n>*>>Similar to
    <markup|index-<with|mode|math|n>>, but without the page number.

    <item*|<markup|index-dots>>The macro which produces the dots between an
    index entry and the corresponding page number(s).
  </description>

  The following tags are used for glossaries:

  <\description>
    <item*|<markup|glossary>>A function which inserts its only argument into
    the glossary.

    <item*|<markup|glossary-dup>>For creating an additional page number for
    an entry which was already inserted before.

    <item*|<markup|glossary-explain>>A function for inserting a glossary
    entry with its explanation.

    <item*|<markup|glossary-line>>Insert a glossary entry without a page
    number.

    <item*|<markup|glossary-1>>Macro for rendering a glossary entry and its
    corresponding page number.

    <item*|<markup|glossary-2>>Macro for rendering a glossary entry, its
    explanation, and its page number.

    <item*|<markup|glossary-dots>>The macro which produces the dots between a
    glossary entry and the corresponding page number(s).
  </description>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-34>>
    </associate>
  </collection>
</auxiliary>