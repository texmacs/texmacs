<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|White space primitives>

  <\explain>
    <explain-macro|vspace|len>

    <explain-macro|vspace|len|min|max><explain-synopsis|vertical space after>
  <|explain>
    This primitive inserts an elastic vertical space after the current
    paragraph. All operands must be <hyper-link|length
    values|../language/lengths.en.tm>. The <src-arg|len> argument specifies
    the default length and the <src-arg|min> and <src-arg|max> arguments the
    bounds to vertical stretching for page breaking and filling. If
    <src-arg|min> and <src-arg|max> are not specified, then they are
    determined implicitly from the length unit of <src-arg|len>.

    Notice that operands are not evaluated, so they must be literal strings.
  </explain>

  <\explain>
    <explain-macro|vspace*|len>

    <explain-macro|vspace*|len|min|max><explain-synopsis|vertical space
    before>
  <|explain>
    This primitive is similar to <markup|vspace>, except that the vertical
    space is inserted <em|before> the current paragraph. The actual vertical
    space between two consecutive paragraphs is the <em|maximum>, not the
    sum, of the vertical spaces specified by the the <markup|vspace> and
    <markup|vspace*> tags in the surrounding paragraphs.
  </explain>

  <\explain>
    <explain-macro|space|len>

    <explain-macro|space|len|bot|top><explain-synopsis|rigid horizontal
    space>
  <|explain>
    This primitive inserts an empty box whose width is <src-arg|len>, and
    whose bottom and top sides are at distances <src-arg|bot> and
    <src-arg|top> from the baseline.

    If <src-arg|bot> and <src-arg|top> are not specified, then an empty box
    is inserted whose bottom is on the baseline and whose height is the same
    as the lowercase letter <samp|x> in the current font.

    Notice that operands are not evaluated, so they must be literal strings.
  </explain>

  <\explain>
    <explain-macro|hspace|len>

    <explain-macro|hspace|len|min|max><explain-synopsis|stretchable
    horizontal space>
  <|explain>
    This primitive inserts inserts a stretchable horizontal space of nominal
    width <src-arg|len>, which must be a <hyper-link|length
    value|../language/lengths.en.tm>. The <src-arg|min> and <src-arg|max>
    arguments specify bounds to horizontal stretching for line breaking and
    filling. If <src-arg|min> and <src-arg|max> are not specified, then they
    are determined implicitly from the length unit of <src-arg|len>.

    Notice that operands are not evaluated, so they must be literal strings.
  </explain>

  <\explain>
    <explain-macro|htab|min>

    <explain-macro|htab|min|weight><explain-synopsis|horizontal spring>
  <|explain>
    Springs are horizontal spaces which extend so the containing paragraph
    takes all the available horizontal space. When a paragraph is line
    wrapped, split in several visual lines, only springs in the last line are
    extended.

    A spring has a <em|minimal width> and a <em|weight>. If the weight is 0,
    the spring is <em|weak>, otherwise it is <em|strong>. If a line contains
    mixed weak and strong springs, only the strong springs extend.\ 

    The fraction of the available horizontal space taken up by each strong
    spring is proportional to its weight. If there are only weak springs,
    they share the available space evenly.

    <\indent>
      <explain-macro|htab|min> inserts a strong spring of minimal width
      <src-arg|min> and of weight unity. The <src-arg|min> operand must be a
      <hyper-link|length value|../language/lengths.en.tm>.

      <explain-macro|htab|min|weight> specifies the weight, which can be a
      positive decimal number or one of the two special values documented
      below.

      <explain-macro|htab|min|<src-value|first>> inserts a <em|tail weak>
      spring, only the first one in a paragraph is significant.

      <explain-macro|htab|min|<src-value|last>> inserts a <em|head weak>
      spring, only the last one in a paragraph is significant.
    </indent>

    Operands are not evaluated and must be literal strings.

    Weak springs are useful in style-sheets. For example, tail weak springs
    are used to make the list environment extend to across the full
    paragraph, so vertical motion commands in nested lists behave as
    expected. In regular documents, springs are often used to place some text
    on the right side of the page and some other text on the left side.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>