<TeXmacs|1.0.5.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|scripts|1.0>

    <\src-purpose>
      Environments for on the fly evaluation of scripts.
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
      Script rendering.
    </src-comment>
  </active*>

  <assign|script-aux-1|<macro|lan|body|<arg|body>>>

  <assign|script-aux-2|<macro|lan|body|<with|mode|prog|prog-language|<arg|lan>|<arg|body>>>>

  <assign|script-aux-3|<macro|lan|body|<style-with|src-compact|none|<compound|<if|<equal|<value|mode>|math>|script-aux-1|script-aux-2>|<arg|lan>|<arg|body>>>>>

  <assign|render-big-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|2|2|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-bsep|0.15fn>|<cwith|1|1|1|1|cell-tsep|0.15fn>|<cwith|2|2|1|1|cell-width|1par>|<cwith|2|2|1|1|cell-hmode|min>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-tsep|0.3fn>|<cwith|2|2|1|1|cell-bsep|0.3fn>|<cwith|1|1|1|1|cell-halign|c>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>>|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <assign|render-small-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-lsep|0.15fn>|<cwith|1|1|1|1|cell-rsep|0.15fn>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|render-eval-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <\active*>
    <\src-comment>
      Scripts for different types of languages.
    </src-comment>
  </active*>

  <assign|script-input|<macro|lan|session|in|out|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<arg|lan>|-script-input>>|<merge|<arg|lan>|-script-input>|generic-script-input>>|<arg|lan>|<arg|session>|<arg|in>|<arg|out>>>>>

  <assign|generic-script-input|<macro|lan|session|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|lan>|<script-aux-3|<arg|lan>|<arg|in>>>>>>

  <assign|script-output|<macro|lan|session|in|out|<arg|out>>>

  <assign|script-eval|<macro|in|<render-eval-script|<value|prog-scripts>|<script-aux-3|<value|prog-scripts>|<arg|in>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>