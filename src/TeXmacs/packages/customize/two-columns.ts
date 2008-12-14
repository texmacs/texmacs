<TeXmacs|1.0.4.7>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|two-columns|1.0|two-columns|1.0>

    <\src-purpose>
      Standard customization for two column styles
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|par-columns|2>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-make-title-2col|<value|doc-make-title>>

  <assign|doc-make-title|<\macro|body>
    <\with|par-columns|1>
      <doc-make-title-2col|<arg|body>>

      \;
    </with>
  </macro>>

  <assign|doc-footnote|<macro|body|<style-with|src-compact|none|<if|<unequal|<get-arity|<quote-arg|body>>|0>|<quasi|<with|par-columns|1|<style-with|src-compact|none|<render-footnote|<unquote|<doc-author-note-next>>|<arg|body|0><map-args|doc-footnote-sub|concat|body|1>>>>>>>>>

  <\active*>
    <\src-comment>
      Big figures.
    </src-comment>
  </active*>

  <assign|render-big-figure-2col|<value|render-big-figure>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <\with|par-columns|1>
      <render-big-figure-2col|<arg|type>|<arg|name>|<arg|fig>|<arg|cap>>
    </with>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>