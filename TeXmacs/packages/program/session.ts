<TeXmacs|1.0.7.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|session|1.0>

    <\src-purpose>
      Environments for displaying computer algebra, shell, or other sessions.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Englobing sessions.
    </src-comment>
  </active*>

  <assign|session|<\macro|lan|ses|body>
    <\with|prog-language|<arg|lan>|prog-session|<arg|ses>>
      <render-session|<arg|body>>
    </with>
  </macro>>

  <assign|render-session|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-session>>|<merge|<value|prog-language>|-session>|generic-session>>|<value|prog-language>|<arg|body>>>>>

  <assign|generic-session|<\macro|name|body>
    <\padded-normal|1fn|1fn>
      <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
    </padded-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Input fields.
    </src-comment>
  </active*>

  <assign|input|<macro|prompt|body|<with|mode|prog|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-input>>|<merge|<value|prog-language>|-input>|generic-input>>|<arg|prompt>|<arg|body>>>>>>

  <assign|input-math|<\macro|prompt|in>
    <input|<arg|prompt>|<math|<arg|in>>>
  </macro>>

  <assign|id-function|<macro|x|<arg|x>>>

  <assign|generic-input|<\macro|prompt|body>
    <tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|1|1|cell-lsep|0fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|2|2|cell-lsep|0fn>|<cwith|1|1|2|2|cell-rsep|0fn>|<cwith|1|1|2|2|cell-hyphen|t>|<twith|table-hyphen|y>|<table|<row|<cell|<id-function|<arg|prompt>>>|<\cell>
      <with|color|blue|math-display|true|<arg|body>>
    </cell>>>>>
  </macro>>

  <assign|scheme-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<with|color|dark
  green|<arg|prompt>>|<with|color|black|<arg|body>>>>>>

  <\active*>
    <\src-comment>
      Output fields and error reporting in output.
    </src-comment>
  </active*>

  <assign|output|<macro|body|<with|mode|prog|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-output>>|<merge|<value|prog-language>|-output>|generic-output>>|<arg|body>>>>>>

  <assign|generic-output*|<macro|body|<with|par-mode|left|math-display|true|<arg|body>>>>

  <assign|generic-output|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-left|1.5fn>
        <generic-output*|<arg|body>>
      </indent-left>
    </padded-normal>
  </macro>>

  <assign|errput|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-errput>>|<merge|<value|prog-language>|-textput>|generic-errput>>|<arg|body>>>>>

  <assign|generic-errput|<\macro|body>
    <\wide-normal>
      <with|color|red|<arg|body>>
    </wide-normal>
  </macro>>

  <assign|timing|<macro|s|<compound|small|<htab|5mm><with|color|dark
  grey|<arg|s>>>>>

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

  <assign|unfolded-io|<\macro|prompt|in|out>
    <\surround||<right-flush>>
      <\input|<with|locus-color|preserve|<action|<arg|prompt>|(mouse-fold)|<arg|in>>>>
        <arg|in>
      </input>

      <\output>
        <arg|out>
      </output>
    </surround>
  </macro>>

  <assign|folded-io|<\macro|prompt|in|out>
    <\input|<with|locus-color|preserve|<action|<arg|prompt>|(mouse-unfold)|<arg|in>>>>
      <arg|in>
    </input>
  </macro>>

  <assign|unfolded-io-math|<\macro|prompt|in|out>
    <unfolded-io|<arg|prompt>|<math|<arg|in>>|<arg|out>>
  </macro>>

  <assign|folded-io-math|<\macro|prompt|in|out>
    <folded-io|<arg|prompt>|<math|<arg|in>>|<arg|out>>
  </macro>>

  <\active*>
    <\src-comment>
      Importing various formats
    </src-comment>
  </active*>

  <assign|html-text|<macro|x|<with|mode|text|par-mode|justify|font-family|ss|par-indent|0em|par-par-sep|0.33em|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>