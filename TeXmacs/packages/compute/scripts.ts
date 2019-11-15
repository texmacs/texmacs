<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|scripts|1.0>

    <\src-purpose>
      Environments for on the fly evaluation of scripts.
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
      Style parameters
    </src-comment>
  </active*>

  <assign|script-border-color|grey>

  <assign|script-background-color|pastel yellow>

  <assign|script-language-color|white>

  <assign|script-language-background-color|#8080A0>

  <assign|script-status-color|red>

  <assign|script-status-background-color|pastel yellow>

  <assign|script-input-field-background-color|white>

  <\active*>
    <\src-comment>
      Macros for the option "keep evaluated expressions"
    </src-comment>
  </active*>

  <assign|script-assign|<macro|<with|color|dark blue|\<assign\>>>>

  <assign|script-result|<macro|in|out|<arg|in><with|color|dark
  blue|=><arg|out>>>

  <assign|script-approx|<macro|in|out|<arg|in><with|color|dark
  blue|\<approx\>><arg|out>>>

  <drd-props|script-result|arity|2|border|no>

  <drd-props|script-approx|arity|2|border|no>

  <\active*>
    <\src-comment>
      Macros for status information
    </src-comment>
  </active*>

  <assign|script-status|<macro|body|<with|color|<value|script-status-color>|<block|<tformat|<cwith|1|1|1|1|cell-background|<value|script-status-background-color>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|script-busy|<macro|msg|<script-status|<if|<equal|<arg|msg>|<uninit>>|<localize|Busy>...|<arg|msg>>>>>

  <assign|script-interrupted|<macro|<script-status|<math|\<lightning\>>
  <localize|Interrupted>>>>

  <assign|script-dead|<macro|<script-status|<math|\<lightning\>>
  <localize|Dead>>>>

  <\active*>
    <\src-comment>
      Script rendering.
    </src-comment>
  </active*>

  <assign|script-aux-1|<macro|language|body|<arg|body>>>

  <assign|script-aux-2|<macro|language|body|<with|mode|prog|prog-language|<arg|language>|<arg|body>>>>

  <assign|script-aux-3|<macro|language|body|<style-with|src-compact|none|<compound|<if|<equal|<value|mode>|math>|script-aux-1|script-aux-2>|<arg|language>|<arg|body>>>>>

  <assign|render-big-script|<macro|language|body|<with|old-color|<value|color>|color|<value|script-border-color>|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|<value|script-language-background-color>>|<cwith|2|2|1|1|cell-background|<value|script-background-color>>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-bsep|0.15fn>|<cwith|1|1|1|1|cell-tsep|0.15fn>|<cwith|2|2|1|1|cell-width|1par>|<cwith|2|2|1|1|cell-hmode|min>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-tsep|0.3fn>|<cwith|2|2|1|1|cell-bsep|0.3fn>|<cwith|1|1|1|1|cell-halign|c>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|<value|script-language-color>|<change-case|<arg|language>|Upcase>>>>>>|<row|<\cell>
    <with|color|<value|old-color>|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <assign|render-small-script|<macro|language|body|<with|old-color|<value|color>|color|<value|script-border-color>|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|<value|script-language-background-color>>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-background|<value|script-background-color>>|<cwith|1|1|1|1|cell-lsep|0.15fn>|<cwith|1|1|1|1|cell-rsep|0.15fn>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|<value|script-language-color>|<change-case|<arg|language>|Upcase>>>>>|<cell|<with|color|<value|old-color>|<arg|body>>>>>>>>>>

  <assign|render-eval-script|<macro|language|body|<with|old-color|<value|color>|color|<value|script-border-color>|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|<value|script-background-color>>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|<value|old-color>|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <\active*>
    <\src-comment>
      Scripts for different types of languages.
    </src-comment>
  </active*>

  <assign|script-input|<macro|language|session|in|out|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<arg|language>|-script-input>>|<merge|<arg|language>|-script-input>|generic-script-input>>|<arg|language>|<arg|session>|<arg|in>|<arg|out>>>>>

  <assign|generic-script-input|<macro|language|session|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|language>|<script-aux-3|<arg|language>|<arg|in>>>>>>

  <assign|script-output|<macro|language|session|in|out|<arg|out>>>

  <assign|script-eval|<macro|in|<render-eval-script|<value|prog-scripts>|<script-aux-3|<value|prog-scripts>|<arg|in>>>>>

  <drd-props|script-input|arity|3|accessible|2|unaccessible|1|unaccessible|3>

  <\active*>
    <\src-comment>
      Plots.
    </src-comment>
  </active*>

  <assign|plot|<macro|name|body|<render-big-script|<arg|name>|<arg|body>>>>

  <assign|plot-group|<macro|text|<style-with|src-compact|none|<resize|<small|<with|font-shape|italic|font-series|bold|<arg|text>>>||<minus|1b|0.3fn>||<plus|1t|0.3fn>>>>>

  <assign|plot-input-field|<macro|size|body|<with|old-color|<value|color>|color|<value|script-border-color>|<tabular|<tformat|<cwith|1|1|1|1|cell-background|<value|script-input-field-background-color>>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-width|<arg|size>>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<cell|<with|color|<value|old-color>|<arg|body>>>>>>>>>>

  <assign|plot-function|<macro|x|body|<style-with|src-compact|none|<resize|<math|<arg|x>>:
  |||1.5fn|><plot-input-field|0.666par|<math|<arg|body>>>>>>

  <assign|plot-range|<macro|x|start|end|<style-with|src-compact|none|<resize|<math|<arg|x>>:
  |||1.5fn|><plot-input-field|0.25par|<math|<arg|start>>> \U
  <plot-input-field|0.25par|<math|<arg|end>>>>>>

  <assign|plot-curve|<macro|fun-f|start-x|end-x|<\plot|<localize|Plot curve>>
    <plot-group|<localize|Function>>

    <plot-function|f|<arg|fun-f>>

    <plot-group|<localize|Range>>

    <plot-range|x|<arg|start-x>|<arg|end-x>>
  </plot>>>

  <assign|plot-curve*|<macro|fun-x|fun-y|start-t|end-t|<\plot|<localize|Plot
  parametric curve>>
    <plot-group|<localize|Function>>

    <plot-function|x|<arg|fun-x>>

    <plot-function|y|<arg|fun-y>>

    <plot-group|<localize|Range>>

    <plot-range|t|<arg|start-t>|<arg|end-t>>
  </plot>>>

  <assign|plot-surface|<macro|fun-f|start-x|end-x|start-y|end-y|<\plot|<localize|Plot
  surface>>
    <plot-group|<localize|Function>>

    <plot-function|f|<arg|fun-f>>

    <plot-group|<localize|Range>>

    <plot-range|x|<arg|start-x>|<arg|end-x>>

    <plot-range|y|<arg|start-y>|<arg|end-y>>
  </plot>>>

  <assign|plot-surface*|<macro|fun-x|fun-y|fun-z|start-u|end-u|start-v|end-v|<\plot|<localize|Plot
  parametric surface>>
    <plot-group|<localize|Function>>

    <plot-function|x|<arg|fun-x>>

    <plot-function|y|<arg|fun-y>>

    <plot-function|z|<arg|fun-z>>

    <plot-group|<localize|Range>>

    <plot-range|u|<arg|start-u>|<arg|end-u>>

    <plot-range|v|<arg|start-v>|<arg|end-v>>
  </plot>>>

  <assign|plot-output|<macro|in|out|<arg|out>>>

  <\active*>
    <\src-comment>
      New plots.
    </src-comment>
  </active*>

  <assign|plot-script-output|<macro|language|session|name|in|out|<arg|out>>>

  <assign|plot-script-input|<\macro|language|session|name|in|out>
    <\with|par-par-sep|0fn>
      <\script-aux-2|<arg|language>>
        <arg|in>
      </script-aux-2>
    </with>
  </macro>>

  <drd-props|plot-script-input|arity|5|unaccessible|0|unaccessible|1|unaccessible|2|accessible|3|unaccessible|4|border|no>

  <\active*>
    <\src-comment>
      Converters.
    </src-comment>
  </active*>

  <assign|converter-input|<macro|format|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|format>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  <assign|converter-output|<macro|format|in|out|<arg|out>>>

  <assign|converter-eval|<macro|format|in|<style-with|src-compact|none|<render-small-script|<arg|format>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  <drd-props|converter-input|arity|3|accessible|1|unaccessible|2>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>