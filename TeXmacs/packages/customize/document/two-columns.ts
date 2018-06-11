<TeXmacs|1.99.6>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|two-columns|1.0|two-columns|1.0>

    <\src-purpose>
      Standard customization for two column styles
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|par-columns|2>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-make-title|<macro|body|<with|par-columns|1|<surround||<vspace|2fn>|<doc-title-block|<arg|body>>>>>>

  <assign|custom-footnote-text|<macro|sym|id|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<\with|par-mode|justify|par-left|0cm|par-right|0cm|par-columns|1>
      <\custom-note-text|<arg|sym>|<arg|id>>
        <arg|body>
      </custom-note-text>
    </with>>
  </float>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>