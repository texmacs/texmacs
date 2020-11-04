<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Paragraph layout>

  <\explain>
    <var-val|par-mode|justify><explain-synopsis|paragraph alignment>
  <|explain>
    This environment variable specifies the alignment of the different lines
    in a paragraph. Possible values are <verbatim|left>, <verbatim|center>,
    <verbatim|right> and <verbatim|justify>:

    <\big-table>
      <\quote-env>
        <\quote-env>
          <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
            <\with|par-mode|left>
              This paragraph is aligned to the left. This paragraph is
              aligned to the left. This paragraph is aligned to the left.
            </with>
          </cell>|<\cell>
            <\with|par-mode|center>
              This paragraph is has been centered. This paragraph is has been
              centered. This paragraph is has been centered.
            </with>
          </cell>>|<row|<\cell>
            <\with|par-mode|right>
              This paragraph is aligned to the right. This paragraph is
              aligned to the right. This paragraph is aligned to the right.
            </with>
          </cell>|<\cell>
            This paragraph has been justified. Justification is the default
            alignment mode for paragraphs. So be it.
          </cell>>>>>
        </quote-env>
      </quote-env>
    <|big-table>
      The supported modes for alignment.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-flexibility|1000.0><explain-synopsis|paragraph flexibility>
  <|explain>
    When using the justified alignment mode, it sometimes occurs that certain
    lines need to be stretched a lot, thereby leaving abnormally large spaces
    in the middle of those lines. This is typically the case inside
    bibliographies with unbreakable hyperlinks. The <src-var|par-flexibility>
    variable specifies a threshold above which justification of a line is
    abandoned. More precisely, we switch to left alignment whenever the
    remaining space on a line exceeds <src-var|par-flexibility> times the
    maximal amount of stretching which still ``looks nice'' for the given
    font.

    <\big-table>
      <\quote-env>
        <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
          For certain paragraphs with wide unbreakable pieces of text, such
          as the hyperlink <rigid|<href|https://www.texmacs.org>> it is
          sometimes preferrable to switch from justified to left aligned text
          when the spacing between words becomes to wide.
        </cell>|<\cell>
          <\with|par-flexibility|2.0>
            For certain paragraphs with wide unbreakable pieces of text, such
            as the hyperlink <rigid|<href|https://www.texmacs.org>> it is
            sometimes preferrable to switch from justified to left aligned
            text when the spacing between words becomes to wide.
          </with>
        </cell>>>>>
      </quote-env>
    <|big-table>
      Difference between a large and small flexibility (on the left and right
      hand sides respectively).
    </big-table>
  </explain>

  <\explain>
    <var-val|par-hyphen|normal><explain-synopsis|quality of hyphenation>
  <|explain>
    This parameter controls the quality of the hyphenation algorithm.
    Possible values are <verbatim|normal> and <verbatim|professional>. The
    professional hyphenation algorithm uses a global algorithm on the entire
    paragraph, whereas the normal one uses a faster first-fit algorithm.

    <\big-table>
      <\with|font-base-size|10>
        <\quote-env>
          <\quote-env>
            <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
              <with|par-hyphen|normal|The difference between the different
              hyphenation algorithms provided by <TeXmacs> is seen best for
              long paragraphs which are typeset into a narrow column. The
              professional hyphenation usually succeeds to minimize the
              number of ugly gaps between words.>
            </cell>|<\cell>
              <with|par-hyphen|professional|The difference between the
              different hyphenation algorithms provided by <TeXmacs> is seen
              best for long paragraphs which are typeset into a narrow
              column. The professional hyphenation usually succeeds to
              minimize the number of ugly gaps between words.>
            </cell>>>>>
          </quote-env>
        </quote-env>
      </with>
    <|big-table>
      Comparison different hyphenation algorithms. At the left hand side, we
      have used the normal algorithm and on the right hand side the
      professional one. Even though there are some ugly gaps at the right
      hand side around ``hyphenation'', the really bad gap around ``The'' on
      the left hand side has been avoided.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-width|auto><explain-synopsis|paragraph width>
  <|explain>
    This environment variable controls the width of paragraphs. By default,
    it is automatically determined as a function of the page (or screen) size
    and margins.
  </explain>

  <\explain>
    <var-val|par-left|0cm>

    <var-val|par-right|0cm><explain-synopsis|left and right margins>
  <|explain>
    These environment variables specify absolute left and right margins for
    the paragraph, with respect to the default left and right margins (which
    are determined as a function of the page layout). For instance:

    <\tm-fragment>
      This text uses the default margins.

      <\with|par-left|1cm>
        This text uses a left margin of <verbatim|1cm>
      </with>

      <\with|par-left|2cm>
        This text uses a left margin of <verbatim|2cm>
      </with>

      <\with|par-left|3cm>
        This text uses a left margin of <verbatim|3cm>
      </with>

      <\with|par-left|3cm|par-right|3cm>
        The left and right margins of this text have both been set to
        <verbatim|3cm>.
      </with>
    </tm-fragment>

    Environments like <markup|itemize> and <markup|quote-env> which maybe
    nested usually compute new margins as a function of the old values by
    adding or subtracting some space:

    <\tm-fragment>
      <inactive*|<assign|quote-env|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.5fn>|<right-flush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|par-first|0fn|par-par-sep|0.25fn|<arg|body>>>>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-first|1.5fn><explain-synopsis|first indentation>
  <|explain>
    The <src-var|par-first> parameter specifies the additional indentation
    which is used for the first line of the paragraph. The aim of first
    indentations is to indicate the starts of new paragraphs. An alternative
    technique is the use of vertical whitespace.

    <\big-table>
      <\quote-env>
        <\quote-env>
          <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
            <\with|par-par-sep|0fn>
              <\with|par-first|1.5fn>
                The <tmstyle|article> and <tmstyle|book> styles in <TeXmacs>
                indictate the starts of new paragraphs through the use of a
                first indentation.

                The <tmstyle|generic> and <tmstyle|letter> styles rather use
                vertical whitespace.
              </with>
            </with>
          </cell>|<\cell>
            <\with|par-par-sep|0.5fn>
              The <tmstyle|generic> and <tmstyle|letter> styles in <TeXmacs>
              indictate the starts of new paragraphs through the use of
              vertical whitespace.

              The <tmstyle|article> and <tmstyle|book> styles rather use a
              first indentation.
            </with>
          </cell>>>>>
        </quote-env>
      </quote-env>
    <|big-table>
      <label|par-first-tab>Two classical ways to indicate the starts of new
      paragraphs.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-sep|0.2fn><explain-synopsis|extra separation between
    successive lines>
  <|explain>
    The sum of the font size and <inactive|<value|par-sep>> determines the
    ideal distance between two successive base lines in a paragraph (also
    called the ``base line skip''). Of course, when the lines contain large
    boxes, then this distance may need to be increased. When <verbatim|1fn>
    for <inactive|<value|par-sep>>, one may for instance produce documents
    with a double interline space:

    <\tm-fragment>
      <\with|par-sep|1fn>
        A double interline space corresponds to <var-val|par-sep|1fn>. Double
        interline spaces are often used by lazy people who want to pretend
        that they have written many pages. They generally do not care about
        tropical rain forests.
      </with>
    </tm-fragment>

    In the case when two successive lines use different base line skips, then
    the maximal value is used in order to compute the ideal distance between
    their baselines. This allows for a reasonable spacing when the font size
    is changed from one paragraph to another:

    <\tm-fragment>
      <\with|par-par-sep|0fn>
        Normal text.

        <very-large|Some very large text.>

        And back to normal.
      </with>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-line-sep|0.025fn*><explain-synopsis|extra space between
    lines>
  <|explain>
    This parameter corresponds an additional stretchable amount of whitespace
    between successive lines in a paragraph. Setting
    <inactive|<value|par-line-sep>> to a small stretchable value which
    defaults to <math|0> allows the page breaker to correctly stretch pages
    which contain a very long textual paragraph. Indeed,
    <inactive|<value|par-line-sep>> vanishes, then the height of a textual
    paragraph is of the form <math|a+b*n>, where <math|a> and <math|b> are
    constants and <math|n> is the number of lines. There is no reason why the
    usable height of a page should be of this form.
  </explain>

  <\explain>
    <var-val|par-par-sep|0.5fn*><explain-synopsis|extra space between
    paragraphs>
  <|explain>
    The <src-var|par-par-sep> parameter specifies the amount of vertical
    whitespace which separates two successive paragraphs. This space is
    determined in <hlink|stretchable length units|../basics/lengths.en.tm>.
    By default, <TeXmacs> does not use any whitespace between successive
    paragraphs, except when no nice page breaks could be found (this explains
    the use of the <verbatim|fn*> length unit). Starts of new paragraphs are
    rather indicated through the use of first indentations (see table
    <reference|par-first-tab>).

    In the case when two successive paragraph use different paragraph
    separations, then the maximum of the two is taken. In fact, the
    <src-var|par-par-sep> length is added to both the vertical spacing before
    and the vertical spacing after the paragraph.
  </explain>

  <\explain>
    <var-val|par-hor-sep|0.5fn>

    <var-val|par-ver-sep|0.2fn><explain-synopsis|minimal space between ink>
  <|explain>
    When a paragraph contains several exceptionally large boxes, then
    <TeXmacs> attempts to ``shove successive lines into another'' as long as
    none of the boxes collide:

    <\with|font-base-size|10>
      <\tm-fragment>
        Consider a fraction which decends more than usual like
        <math|<frac|1|x+1>> at the end of a line and an expression like
        <math|\<mathe\><rsup|\<mathe\><rsup|x>>> which is higher than usual.

        When these expressions occur at different places, then <TeXmacs>
        tries to render the successive lines in a compact manner.

        In the case of a fraction <math|<frac|1|x+1>> and an exceptionally
        high expression at the wrong place, like the expression
        <math|\<mathe\><rsup|\<mathe\><rsup|x>>> here, the boxes are
        separated by <src-var|env-ver-sep>.
      </tm-fragment>
    </with>

    As soon as the horizontal distance between two large boxes is less than
    <src-var|par-hor-sep>, then they are considered to be in collision. In
    that case, the vertical distance between them must be at least
    <src-var|par-ver-sep>. Also, the amount of showing never exceeds
    <verbatim|1ex>.

    When using an interline space of <verbatim|1.5> or <verbatim|2>, the
    default value of <src-var|par-ver-sep> allows the user to type larger
    formulas in the text while preserving a uniform layout. When using a
    small <src-var|par-sep> and a large <inactive|<value|par-ver-sep>>, the
    distance between two successive lines remains small, except when their
    contents are horizontally close. This may for instance be used to reduce
    the space between a short like followed by a centered equation.
  </explain>

  <\explain>
    <var-val|par-fnote-sep|0.2fn><explain-synopsis|minimal space between
    different footnotes>
  <|explain>
    This parameter controls the amount of vertical space between successive
    footnotes.
  </explain>

  <\explain>
    <var-val|par-columns|1><explain-synopsis|number of columns>
  <|explain>
    This environment variable specifies the number of columns into which the
    text is being typeset. Different numbers of columns may be used
    successively in the same document.
  </explain>

  <\explain>
    <var-val|par-columns-sep|2fn><explain-synopsis|distance between columns>
  <|explain>
    This environment variable specifies the amount of horizontal whitespace
    which separates different columns in multi-column mode.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>