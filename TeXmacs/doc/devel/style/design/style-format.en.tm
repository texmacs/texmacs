<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Formatting primitives>

  This section contains some important notes on formatting primitives which
  are not really part of the style-sheet language, but nevertheless very
  related.

  First of all, most <TeXmacs> presentation tags can be divided in two main
  categories: inline tags and block tags. For instance, <markup|frac> is a
  typical inline tag, whereas <markup|theorem> is a typical block tag. Some
  tags, like <markup|strong> are inline if their argument is and block in the
  contrary case. When writing macros, it is important to be aware of the
  inline or block nature of tags, because block tags inside a horizontal
  concatenation are not rendered in an adequate way. If you need to surround
  a block tag with some inline text, then you need the surround primitive:

  <\tm-fragment>
    <inactive*|<assign|my-theorem|<macro|body|<surround|<no-indent><with|font-series|bold|Theorem.
    >|<right-flush>|<arg|body>>>>>
  </tm-fragment>

  In this example, we surrounded the body of the theorem with the bold text
  ``Theorem.'' at the left hand side and a ``right-flush'' at the right-hand
  side. Flushing to the right is important in order to make the blue visual
  border hints look nice when you are inside the environment.

  In most cases, <TeXmacs> does a good job in determining which tags are
  inline and which ones are not. However, you sometimes may wish to force a
  tag to be a block environment. For instance, the tag
  <markup|very-important> defined by

  <\tm-fragment>
    <inactive*|<assign|very-important|<macro|body|<with|font-series|bold|color|red|<arg|body>>>>>
  </tm-fragment>

  may both be used as an inline tag and a block environment. When placing
  your cursor just before the <markup|with>-tag and hitting
  <shortcut|(kbd-return)> followed by <key|backspace>, you obtain

  <\tm-fragment>
    <inactive*|<assign|very-important|<\macro|body>
      <with|font-series|bold|color|red|<arg|body>>
    </macro>>>
  </tm-fragment>

  Since the body of the macro is now a block, your tag
  <markup|very-important> will automatically become a block environment too.
  In the future, the <markup|drd-props> primitive will give you even more
  control over which tags and arguments are inline and which ones are block.

  Another important property of tags is whether they contain normal textual
  content or tabular content. For instance, consider the definition of the
  standard <markup|eqnarray*> tag (with a bit of presentation markup
  suppressed):

  <\tm-fragment>
    <inactive*|<assign|eqnarray*|<macro|body|<with|par-mode|center|mode|math|math-display|true|par-sep|0.45fn|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<arg|body>>>>>>>
  </tm-fragment>

  The use of <markup|surround> indicates that <markup|eqnarray*> is a block
  environment and the use of <markup|tformat> specifies that it is also a
  tabular environment. Moreover, the <markup|twith> and <markup|cwith> are
  used to specify further formatting information: since we are a block
  environment, we enable hyphenation and let the table span over the whole
  paragraph (unused space being equally distributed over the first and last
  columns). Furthermore, we have specified that the table contains exactly
  three columns.

  Finally, it is important to bear in mind that style-sheets do not merely
  specify the final presentation of a document, but that they may also
  contain information for the authoring phase. Above, we have already
  mentioned the use of the <markup|right-flush> tag in order to improve the
  rendering of \ visual border hints. Similarly, visual hints on invisible
  arguments may be given in the form of flags:

  <\tm-fragment>
    <inactive*|<assign|labeled-theorem|<macro|id|body|<style-with|src-compact|none|<surround|<style-with|src-compact|none|<no-indent><flag|Id:
    <arg|id>|blue|id><with|font-series|bold|Theorem.
    >>|<right-flush>|<arg|body>>>>>>
  </tm-fragment>

  More generally, the <markup|specific> tag with first argument ``screen''
  may be used to display visual hints, which are removed when printing the
  document.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>