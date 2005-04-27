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
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
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