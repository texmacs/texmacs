<TeXmacs|1.99.1>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|coq|1.0>

      <\src-purpose>
        The article style.
      </src-purpose>

      <\src-copyright|1998--2004>
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

  <assign|par-first|0em>

  <assign|theorem-sep|<macro|: >>

  \;

  <assign|coq-prompt-color|red>

  <assign|coq-input-color|dark brown>

  <assign|coq-prompt|<macro|nr|<with|prog-language|verbatim|Coq]<specific|html|&nbsp;>
  >>>

  <assign|coq-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|coq-prompt-color>|generic-input-color|<value|coq-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  <assign|coq-output|<macro|body|<generic-output|<with|prog-language|verbatim|<arg|body>>>>>

  <assign|coq-section|<\macro|name|body>
    <section*|<arg|name>>

    <arg|body>
  </macro>>

  <assign|coq-command|<macro|id|status|body|<with|color|<arg|status>|<arg|body>>>>

  <assign|render-coq-comment|<macro|body|<with|color|#c08040|font-size|0.841|font-family|tt|<arg|body>>>>

  <assign|hrule|<macro|<no-indent><render-coq-comment|<tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|-1|cell-height|1ln>|<cwith|1|-1|1|-1|cell-lsep|0fn>|<cwith|1|-1|1|-1|cell-rsep|0fn>|<cwith|1|-1|1|-1|cell-bsep|0fn>|<cwith|1|-1|1|-1|cell-tsep|0fn>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>>>

  <assign|coq-enunciation|<\macro|id|status|type|name|body>
    <\render-theorem|<arg|type>>
      <surround| <with|font-shape|italic|<arg|name>>
      ||<with|color|<arg|status>|<arg|body>>>
    </render-theorem>
  </macro>>

  <assign|coq-proof|<\macro|id|status|header|body>
    <\with|color|<arg|status>|remark-sep|>
      <\render-proof|<arg|header>>
        \;

        <arg|body>
      </render-proof>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Coq Doc
    </src-comment>
  </active*>

  <assign|coq-comment|<\macro|body>
    <\render-coq-comment>
      <\indent>
        <surround|(*|*)|<arg|body>>
      </indent>
    </render-coq-comment>
  </macro>>

  <assign|coq-coqdoc|<\macro|body>
    <\render-coq-comment>
      <\indent>
        <surround|<with|par-first|<minus|2em>|<yes-indent>><resize|(**
        |1l||<plus|1l|2em>|>| *)|<arg|body>>
      </indent>
    </render-coq-comment>
  </macro>>

  <assign|coqdoc-coq|<macro|body|<with|color|<value|coq-input-color>|font-size|0.841|font-family|tt|<arg|body>>>>

  <assign|coqdoc-html|<value|identity>>

  <assign|coqdoc-latex|<value|identity>>

  <assign|coqdoc-verbatim|<macro|body|<with|color|black|font-size|0.841|font-family|tt|<arg|body>>>>

  <assign|coqdoc-vernac|<macro|body|<with|color|<value|coq-input-color>|font-size|0.841|font-family|tt|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>