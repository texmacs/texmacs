<TeXmacs|1.0.7.13>

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

  <assign|coq-prompt|<macro|nr|<with|prog-language|verbatim|Coq]<specific|html|&nbsp;>
  >>>

  <assign|coq-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<with|color|red|<arg|prompt>>|<with|color|dark
  brown|<arg|body>>>>>>

  <assign|coq-output|<macro|body|<generic-output|<with|prog-language|verbatim|<arg|body>>>>>

  \;

  <assign|coq-command|<macro|id|status|body|<with|color|<arg|status>|<arg|body>
  >>>

  <assign|coq-comment|<macro|body|<with|color|#c08040|font-size|0.841|<arg|body>>>>

  <assign|coq-enunciation|<\macro|id|status|type|name|thm-body|proof-body>
    <\render-theorem|<arg|type> <with|font-shape|italic|<arg|name>>>
      \;

      <\indent>
        <with|color|<arg|status>|<arg|thm-body>>
      </indent>
    </render-theorem>

    <\proof>
      \;

      <\indent>
        <arg|proof-body>
      </indent>
    </proof>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>