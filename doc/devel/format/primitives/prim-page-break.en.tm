<TeXmacs|1.0.3.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Page breaking primitives>

  <big-table|<assign|tag-info-table|<macro|x|<descriptive-table|<tformat|<cwith|1|2|1|-1|cell-halign|c>|<cwith|1|1|1|-1|cell-row-span|2>|<cwith|1|1|1|-1|cell-valign|c>|<cwith|1|1|3|3|cell-bborder|0ln>|<cwith|1|2|1|-1|cell-lborder|1ln>|<cwith|2|2|1|-1|cell-background|pastel
  blue>|<cwith|1|1|3|3|cell-col-span|2>|<cwith|1|1|3|3|cell-row-span|1>|<twith|table-min-rows|3>|<twith|table-min-cols|6>|<twith|table-max-cols|6>|<arg|x>>>>><tag-info-table|<tformat|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-halign|c>|<cwith|6|7|4|4|cell-halign|c>|<cwith|6|7|2|2|cell-halign|c>|<cwith|8|9|4|4|cell-halign|c>|<cwith|8|9|2|2|cell-halign|c>|<cwith|9|9|4|4|cell-halign|c>|<cwith|9|9|2|2|cell-halign|c>|<cwith|9|9|4|4|cell-halign|c>|<cwith|9|9|2|2|cell-halign|c>|<cwith|3|3|4|4|cell-halign|c>|<cwith|3|3|2|2|cell-halign|c>|<cwith|3|3|4|4|cell-halign|c>|<cwith|3|3|2|2|cell-halign|c>|<cwith|3|3|4|4|cell-halign|c>|<cwith|3|3|2|2|cell-halign|c>|<cwith|4|4|4|4|cell-halign|c>|<cwith|4|4|2|2|cell-halign|c>|<cwith|4|4|4|4|cell-halign|c>|<cwith|4|4|2|2|cell-halign|c>|<cwith|5|8|4|4|cell-halign|c>|<cwith|5|8|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|6|8|4|4|cell-halign|c>|<cwith|6|8|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|5|5|4|4|cell-halign|c>|<cwith|5|5|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|6|6|4|4|cell-halign|c>|<cwith|6|6|2|2|cell-halign|c>|<cwith|7|8|4|4|cell-halign|c>|<cwith|7|8|2|2|cell-halign|c>|<cwith|8|8|4|4|cell-halign|c>|<cwith|8|8|2|2|cell-halign|c>|<cwith|8|8|4|4|cell-halign|c>|<cwith|8|8|2|2|cell-halign|c>|<cwith|7|7|4|4|cell-halign|c>|<cwith|7|7|2|2|cell-halign|c>|<cwith|7|7|4|4|cell-halign|c>|<cwith|7|7|2|2|cell-halign|c>|<cwith|7|7|4|4|cell-halign|c>|<cwith|7|7|2|2|cell-halign|c>|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|no-page-break>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|no-page-break*>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|new-page>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|new-page*>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|page-break>>|<cell|<with|mode|math|0>>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>|<row|<cell|<markup|page-break*>>|<cell|0>|<cell|Yes>|<cell|->|<cell|Typesetting>|<cell|Physical>>>>>|Page
  breaking primitives>

  The paragraphs in a document are broken in pages in a way similar to how
  paragraphs are hyphenated in soft lines. The page breaker performs
  <def-index|page filling>, it tries to distribute page items evenly so text
  runs to the bottom of every page. It also tries to avoid <def-index|orphans
  and widows>, which are single or pairs of soft lines separated from the
  rest of their paragraph by a page break, but these can be produced when
  there is no better solution.

  <\description-dash>
    <item*|<markup|no-page-break>>Prevent automatic page breaking after this
    paragraph.

    Prevent the occurrence of an automatic page break after the current
    paragraph. Set an infinite page breaking penalty for the current
    paragraph, similarly to <markup|no-break>.

    Forbidden page breaking points are overridden by ``new page'' and ``page
    break'' primitives.

    <item*|<markup|no-page-break*>>Prevent automatic page breaking before
    this paragraph.

    Similar to <markup|no-page-break>, but set the page breaking penalty of
    the previous paragraph.

    <item*|<markup|new-page>>Start a new page after this paragraph.

    Cause the next paragraph to appear on a new page, without filling the
    current page. The page breaker will not try to position the current
    paragraph at the bottom of the page.

    <item*|<markup|new-page*>>Start a new page before this paragraph.

    Similar to <markup|new-page>, but start the new page before the current
    paragraph. This directive is appropriate to use in chapter headings.

    <item*|<markup|page-break>>Force a page break after this paragraph.

    Force a page break after the current paragraph. A forced page break is
    different from a new page, the page breaker will try to position the
    current paragraph at the bottom of the page.

    Use only to fine-tune the automatic page breaking. Ideally, this should
    be a hint similar to <markup|line-break>, but this is implemented as a
    directive, use only with extreme caution.

    <item*|<markup|page-break*>>Force a page break before this paragraph.

    Similar to <markup|page-break>, but force a page break before the current
    paragraph.
  </description-dash>

  When several ``new page'' and ``page break'' directives apply to the same
  point in the document, only the first one is effective. Any
  <markup|new-page> or <markup|page-break> after the first one in a paragraph
  is ignored. Any <markup|new-page> or <markup|page-break> in a paragraph
  overrides any <markup|new-page*> or <markup|page-break*> in the following
  paragraph. Any <markup|new-page*> or <markup|page-break*> after the first
  one in a paragraph is ignored.

  <tmdoc-copyright|2004|David Allouche>

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
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-20|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-21|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-22|<tuple|1|?>>
    <associate|idx-12|<tuple|1|?>>
    <associate|idx-13|<tuple|1|?>>
    <associate|idx-23|<tuple|1|?>>
    <associate|idx-24|<tuple|1|?>>
    <associate|idx-14|<tuple|1|?>>
    <associate|idx-25|<tuple|1|?>>
    <associate|idx-15|<tuple|1|?>>
    <associate|idx-26|<tuple|1|?>>
    <associate|idx-16|<tuple|1|?>>
    <associate|idx-27|<tuple|1|?>>
    <associate|idx-17|<tuple|1|?>>
    <associate|idx-18|<tuple|1|?>>
    <associate|idx-19|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Page breaking primitives|<pageref|gly-1>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-page-break>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-page-break*>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page*>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-page-break>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-page-break*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|no-page-break>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break*>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page*>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break*>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|new-page*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|dark
      green>|page-break*>>|<pageref|idx-23>>
    </associate>
  </collection>
</auxiliary>