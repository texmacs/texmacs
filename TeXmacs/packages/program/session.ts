<TeXmacs|1.0.5.4>

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

  <\active*>
    <\src-comment>
      Script rendering.
    </src-comment>
  </active*>

  <assign|script-aux-1|<macro|lan|body|<arg|body>>>

  <assign|script-aux-2|<macro|lan|body|<with|mode|prog|prog-language|<arg|lan>|<arg|body>>>>

  <assign|script-aux-3|<macro|lan|body|<style-with|src-compact|none|<compound|<if|<equal|<value|mode>|math>|script-aux-1|script-aux-2>|<arg|lan>|<arg|body>>>>>

  <assign|render-big-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|2|2|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-bsep|0.15fn>|<cwith|1|1|1|1|cell-tsep|0.15fn>|<cwith|2|2|1|1|cell-width|1par>|<cwith|2|2|1|1|cell-hmode|min>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-tsep|0.3fn>|<cwith|2|2|1|1|cell-bsep|0.3fn>|<cwith|1|1|1|1|cell-halign|c>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>>|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<script-aux-3|<arg|lan>|<arg|body>>>
  </cell>>>>>>>>

  <assign|render-small-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-lsep|0.15fn>|<cwith|1|1|1|1|cell-rsep|0.15fn>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>|<cell|<with|color|black|<script-aux-3|<arg|lan>|<arg|body>>>>>>>>>>>

  <assign|render-eval-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<script-aux-3|<arg|lan>|<arg|body>>>
  </cell>>>>>>>>

  <\active*>
    <\src-comment>
      Scripts for different types of languages.
    </src-comment>
  </active*>

  <assign|script-input|<macro|in|out|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<value|prog-scripts>|-script-input>>|<merge|<value|prog-scripts>|-script-input>|generic-script-input>>|<value|prog-scripts>|<arg|in>|<arg|out>>>>>

  <assign|generic-script-input|<macro|lan|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|lan>|<arg|in>>>>>

  <assign|script-output|<macro|in|out|<arg|out>>>

  <assign|script-eval|<macro|in|<render-eval-script|<value|prog-scripts>|<arg|in>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>