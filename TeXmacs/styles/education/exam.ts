<TeXmacs|1.99.9>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|exam|1.0>

      <\src-purpose>
        The exam style.
      </src-purpose>

      <\src-copyright|2002--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|title-generic|header-generic|section-generic|std-edu|padded-paragraphs>

  <use-module|(education edu-markup)>

  <\active*>
    <\src-comment>
      Specific title information
    </src-comment>
  </active*>

  <drd-props|doc-exam-class|border|no>

  <drd-props|doc-exam-date|border|no>

  <assign|doc-exam-block-sep|0.5fn>

  <assign|doc-exam-title-sep|2fn>

  <assign|doc-exam-class|<\macro|body>
    <\surround||<vspace|<value|doc-exam-block-sep>>>
      <doc-title-block|<right-flush><arg|body>>
    </surround>
  </macro>>

  <assign|doc-exam-date|<\macro|body>
    <\surround||<vspace|<value|doc-exam-block-sep>>>
      <doc-title-block|<right-flush><arg|body>>
    </surround>
  </macro>>

  <assign|doc-exam-class-date|<\macro|cl|da>
    <\surround||<vspace|<value|doc-exam-block-sep>>>
      <doc-title-block|<arg|cl><right-flush><arg|da>>
    </surround>
  </macro>>

  <assign|doc-title|<macro|x|<\surround||<vspace|<value|doc-exam-block-sep>>>
    <doc-title-block|<with|math-font-series|bold|font-series|bold|font-size|1.189|font-shape|small-caps|<arg|x>>>
  </surround>>>

  <assign|doc-misc|<macro|body|<style-with|src-compact|none|<vspace*|0.25fn><doc-title-block|<arg|body>><vspace|<value|doc-exam-block-sep>>>>>

  <assign|doc-make-title|<macro|body|<surround||<vspace|<value|doc-exam-title-sep>>|<doc-title-block|<arg|body>>>>>

  <assign|doc-data|<xmacro|args|<with|par-columns|1|<extern|doc-data-exam|<quote-arg|args>|>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>