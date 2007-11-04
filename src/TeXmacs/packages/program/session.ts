<TeXmacs|1.0.6.10>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|session|1.0>

    <\src-purpose>
      Environments for displaying computer algebra, shell, or other sessions.
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

  <\active*>
    <\src-comment>
      Englobing sessions.
    </src-comment>
  </active*>

  <assign|session|<macro|body|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-language>|-session>>|<merge|<value|prog-language>|-session>|generic-session>>|<value|prog-language>|<arg|body>>>>>

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

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>