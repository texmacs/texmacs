<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|program|1.0>

    <\src-purpose>
      Environments for displaying computer algebra, shell, or other programs.
      Unlike sessions which are evaluated chronologically, programs are
      evaluated in their context.
    </src-purpose>

    <src-copyright|1998--2014|Joris van der Hoeven, François Poulain>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Style parameters
    </src-comment>
  </active*>

  <assign|generic-prompt-color|dark brown>

  <assign|generic-input-color|dark blue>

  <assign|generic-error-color|#c00000>

  <assign|scheme-prompt-color|dark green>

  <assign|scheme-input-color|black>

  <\active*>
    <\src-comment>
      Englobing programs.
    </src-comment>
  </active*>

  <assign|program|<\macro|language|program|body>
    <\with|prog-language|<arg|language>|prog-program|<arg|program>>
      <render-program|<arg|body>>
    </with>
  </macro>>

  <assign|render-program|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-program>>|<merge|<value|prog-language>|-program>|generic-program>>|<value|prog-language>|<arg|body>>>>>

  <assign|generic-program|<\macro|name|body>
    <\padded>
      <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
    </padded>
  </macro>>

  <\active*>
    <\src-comment>
      Input fields.
    </src-comment>
  </active*>

  <assign|input|<macro|prompt|body|<with|mode|prog|font-family|rm|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-input>>|<merge|<value|prog-language>|-input>|generic-input>>|<arg|prompt>|<arg|body>>>>>>

  <assign|input-math|<\macro|prompt|in>
    <input|<arg|prompt>|<math|<arg|in>>>
  </macro>>

  <assign|id-function|<macro|body|<arg|body>>>

  <assign|generic-input|<\macro|prompt|body>
    <tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|1|1|cell-lsep|0fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|2|2|cell-lsep|0fn>|<cwith|1|1|2|2|cell-rsep|0fn>|<cwith|1|1|2|2|cell-hyphen|t>|<twith|table-hyphen|y>|<table|<row|<cell|<id-function|<with|color|<value|generic-prompt-color>|<arg|prompt>>>>|<\cell>
      <with|color|<value|generic-input-color>|math-display|true|<arg|body>>
    </cell>>>>>
  </macro>>

  <assign|scheme-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|scheme-prompt-color>|generic-input-color|<value|scheme-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Output fields and error reporting in output.
    </src-comment>
  </active*>

  <assign|output|<macro|body|<with|mode|prog|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-output>>|<merge|<value|prog-language>|-output>|generic-output>>|<arg|body>>>>>>

  <assign|generic-output*|<macro|body|<with|par-mode|justify|par-flexibility|2.0|par-hyphen|normal|math-display|true|par-swell|<value|math-swell>|math-frac-limit|<value|session-frac-limit>|math-table-limit|<value|session-table-limit>|<arg|body>>>>

  <assign|generic-output|<\macro|body>
    <\padded>
      <\indent>
        <generic-output*|<arg|body>>
      </indent>
    </padded>
  </macro>>

  <assign|errput|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-errput>>|<merge|<value|prog-language>|-textput>|generic-errput>>|<arg|body>>>>>

  <assign|generic-errput|<\macro|body>
    <\wide-normal>
      <with|color|<value|generic-error-color>|<arg|body>>
    </wide-normal>
  </macro>>

  <assign|timing|<macro|time|<compound|small|<htab|5mm><with|color|dark
  grey|<arg|time>>>>>

  <\active*>
    <\src-comment>
      Textual fields.
    </src-comment>
  </active*>

  <assign|textput|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-textput>>|<merge|<value|prog-language>|-textput>|generic-textput>>|<arg|body>>>>>

  <assign|generic-textput|<\macro|body>
    <\wide-normal>
      <arg|body>
    </wide-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Input-output fields
    </src-comment>
  </active*>

  <assign|unfolded-prog-io|<\macro|prompt|in|out|id>
    <\surround||<right-flush>>
      <\input|<with|locus-color|preserve|<action|<arg|prompt>|mouse-fold|<arg|in>>>>
        <arg|in>
      </input>

      <\output>
        <arg|out>
      </output>
    </surround>
  </macro>>

  <assign|folded-prog-io|<\macro|prompt|in|out|id>
    <\input|<with|locus-color|preserve|<action|<arg|prompt>|mouse-unfold|<arg|in>>>>
      <arg|in>
    </input>
  </macro>>

  <assign|unfolded-prog-io-math|<\macro|prompt|in|out|id>
    <unfolded-prog-io|<arg|prompt>|<math|<arg|in>>|<arg|out>>
  </macro>>

  <assign|folded-prog-io-math|<\macro|prompt|in|out|id>
    <folded-prog-io|<arg|prompt>|<math|<arg|in>>|<arg|out>>
  </macro>>

  <\active*>
    <\src-comment>
      Importing various formats
    </src-comment>
  </active*>

  <assign|html-text|<macro|body|<with|mode|text|par-mode|justify|font-family|ss|par-indent|0em|par-par-sep|0.33em|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>