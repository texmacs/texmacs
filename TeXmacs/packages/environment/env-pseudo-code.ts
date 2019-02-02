<TeXmacs|1.99.8>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|env-pseudo-code|1.0>

    <\src-purpose>
      Some environments for typesetting pseudo-code.
    </src-purpose>

    <src-copyright|2012--2014|François Poulain, Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Rendering of keywords
    </src-comment>
  </active*>

  <assign|render-keyword|<macro|name|<with|font-series|bold|<localize|<arg|name>>>>>

  <assign|render-keyword*|<macro|name|<with|font-series|bold|<arg|name>>>>

  \;

  <assign|render-begin|<macro|<render-keyword|begin>>>

  <assign|render-call|<macro|<render-keyword|call>>>

  <assign|render-do|<macro|<render-keyword|do>>>

  <assign|render-else|<macro|<render-keyword|else>>>

  <assign|render-end|<macro|<render-keyword|end>>>

  <assign|render-false|<macro|<render-keyword|false>>>

  <assign|render-for-all|<macro|<render-keyword|for all>>>

  <assign|render-for-each|<macro|<render-keyword|for each>>>

  <assign|render-for|<macro|<render-keyword|for>>>

  <assign|render-function|<macro|<render-keyword|function>>>

  <assign|render-globals|<macro|<render-keyword|globals>>>

  <assign|render-if|<macro|<render-keyword|if>>>

  <assign|render-inputs|<macro|<render-keyword|inputs>>>

  <assign|render-loop|<macro|<render-keyword|loop>>>

  <assign|render-outputs|<macro|<render-keyword|outputs>>>

  <assign|render-print|<macro|<render-keyword|print>>>

  <assign|render-procedure|<macro|<render-keyword|procedure>>>

  <assign|render-repeat|<macro|<render-keyword|repeat>>>

  <assign|render-return|<macro|<render-keyword|return>>>

  <assign|render-then|<macro|<render-keyword|then>>>

  <assign|render-true|<macro|<render-keyword|true>>>

  <assign|render-until|<macro|<render-keyword|until>>>

  <assign|render-while|<macro|<render-keyword|while>>>

  <assign|render-and|<macro|<render-keyword|and>>>

  <assign|render-xor|<macro|<render-keyword|xor>>>

  <assign|render-or|<macro|<render-keyword|or>>>

  <assign|render-not|<macro|<render-keyword|not>>>

  <assign|render-to|<macro|<render-keyword|to>>>

  \;

  <assign|render-data|<macro|<render-keyword|Data:>>>

  <assign|render-ensure|<macro|<render-keyword|Ensure:>>>

  <assign|render-require|<macro|<render-keyword|Require:>>>

  <assign|render-result|<macro|<render-keyword|Result:>>>

  \;

  <assign|render-else-if|<macro|<render-else> <render-if>>>

  <assign|render-end-for|<macro|<render-end> <render-for>>>

  <assign|render-end-function|<macro|<render-end> <render-function>>>

  <assign|render-end-if|<macro|<render-end> <render-if>>>

  <assign|render-end-loop|<macro|<render-end> <render-loop>>>

  <assign|render-end-procedure|<macro|<render-end> <render-procedure>>>

  <assign|render-end-while|<macro|<render-end> <render-while>>>

  <\active*>
    <\src-comment>
      Constructs with block arguments
    </src-comment>
  </active*>

  <assign|algo-procedure|<\macro|name|args|body>
    <render-procedure> <with|font-shape|small-caps|<arg|name>>(<arg|args>)<no-page-break>

    <\indent>
      <arg|body>
    </indent>

    <render-end-procedure><right-flush>
  </macro>>

  <assign|algo-function|<\macro|name|args|body>
    <render-function> <with|font-shape|small-caps|<arg|name>>(<arg|args>)<no-page-break>

    <\indent>
      <arg|body>
    </indent>

    <render-end-function><right-flush>
  </macro>>

  <assign|algo-for|<\macro|cond|block>
    <render-for> <arg|cond> <render-do><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for><right-flush>
  </macro>>

  <assign|algo-for-all|<\macro|cond|block>
    <render-for-all> <arg|cond> <render-do><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for><right-flush>
  </macro>>

  <assign|algo-for-each|<\macro|cond|block>
    <render-for-each> <arg|cond> <render-do><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for><right-flush>
  </macro>>

  <assign|algo-while|<\macro|cond|block>
    <render-while> <arg|cond> <render-do><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <render-end-while><right-flush>
  </macro>>

  <assign|algo-repeat|<\macro|cond|block>
    <render-repeat><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <surround|<render-until> |<right-flush>|<arg|cond>>
  </macro>>

  <assign|algo-loop|<\macro|block>
    <render-loop><no-page-break>

    <\indent>
      <arg|block>
    </indent>

    <render-end-loop><right-flush>
  </macro>>

  \;

  <assign|algo-body|<\macro|block>
    <render-do><no-page-break>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-begin|<\macro|block>
    <render-begin><no-page-break>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-inputs|<\macro|block>
    <render-inputs><no-page-break>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-outputs|<\macro|block>
    <render-outputs><no-page-break>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <\active*>
    <\src-comment>
      If-then-else statements
    </src-comment>
  </active*>

  <assign|algo-if|<\macro|cond|body>
    <render-if> <arg|cond> <render-then><no-page-break>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-else-if|<\macro|cond|body>
    <render-else> <render-if> <arg|cond> <render-then><no-page-break>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-else|<\macro|body>
    <render-else><no-page-break>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-make-if-else-if|<\macro|args>
    <compound|<if|<equal|<length|<arg|args>>|0>|algo-null|<compound|<if|<equal|<length|<arg|args>>|1>|algo-else|<compound|<if|<greatereq|<length|<arg|args>>|2>|algo-else-if|algo-null>|<look-up|<arg|args>|0>|<look-up|<arg|args>|1>>>|<look-up|<arg|args>|0>>>>

    <compound|<if|<greater|<length|<arg|args>>|2>|algo-make-if-else-if|<render-end-if><right-flush>>|<range|<arg|args>|2|<length|<arg|args>>>>
  </macro>>

  <assign|algo-if-else-if|<\xmacro|test-seq>
    <algo-if|<arg|test-seq|0>|<arg|test-seq|1>>

    <\with|args|<map-args|identity|tuple|test-seq>|arity|<length|<map-args|identity|tuple|test-seq>>>
      <compound|<if|<greater|<arity>|2>|algo-make-if-else-if|render-end-if>|<range|<value|args>|2|<arity>>>
    </with>
  </xmacro>>

  <\active*>
    <\src-comment>
      Constructs with inline arguments
    </src-comment>
  </active*>

  <assign|algo-require|<macro|body|<surround|<render-require> ||<arg|body>>>>

  <assign|algo-data|<macro|body|<surround|<render-data> ||<arg|body>>>>

  <assign|algo-result|<macro|body|<surround|<render-result> ||<arg|body>>>>

  <assign|algo-ensure|<macro|body|<surround|<render-ensure> ||<arg|body>>>>

  <assign|algo-return|<macro|body|<surround|<render-return> ||<arg|body>>>>

  <assign|algo-print|<macro|body|<surround|<render-print> ||<arg|body>>>>

  <\active*>
    <\src-comment>
      Simple keywords
    </src-comment>
  </active*>

  <assign|algo-and|<macro|<render-and>>>

  <assign|algo-or|<macro|<render-or>>>

  <assign|algo-xor|<macro|<render-xor>>>

  <assign|algo-not|<macro|<render-not>>>

  <assign|algo-true|<macro|<render-true>>>

  <assign|algo-false|<macro|<render-false>>>

  <assign|algo-to|<macro|<render-to>>>

  <assign|algo-globals|<macro|<render-globals>>>

  <\active*>
    <\src-comment>
      Miscellaneous constructs for pseudo-code
    </src-comment>
  </active*>

  <assign|render-new-line|<macro|>>

  <assign|render-inc-indent|<macro|<assign|par-left|<plus|<value|par-left>|1.5fn>>>>

  <assign|render-dec-indent|<macro|<assign|par-left|<plus|<value|par-left>|1.5fn>>>>

  <assign|algo-number-label|<macro|lab|<label|<arg|lab>>>>

  <assign|algo-label|<macro|lab|<label|<arg|lab>>>>

  <assign|algo-comment|<macro|body|<with|font-shape|italic|color|dark
  grey|{<arg|body>}>>>

  <assign|algo-comment|<macro|body|<with|color|dark
  grey|{<with|font-shape|italic|<arg|body>>}>>>

  <assign|algo-state|<macro|body|<arg|body>>>

  <assign|algo-call|<\macro|name|args>
    <with|font-shape|small-caps|<arg|name>>(<arg|args>)
  </macro>>

  <\active*>
    <\src-comment>
      Defining new keywords, functions, etc. for algorithms
    </src-comment>
  </active*>

  <assign|algo-new-keyword|<macro|name|body|<assign|<arg|name>|<render-keyword|<arg|body>>>>>

  <assign|algo-new-function|<macro|name|body|<quasiquote|<style-with|src-compact|none|<assign|<arg|name>|<macro|<render-keyword*|<unquote|<arg|body>>:>
  >><assign|<merge|<arg|name>|*>|<macro|x|<surround|<render-keyword*|<unquote|<arg|body>>:>
  ||<arg|x>>>>>>>>

  <assign|algo-new-data|<macro|name|body|<quasiquote|<style-with|src-compact|none|<assign|<arg|name>|<macro|<render-keyword*|<unquote|<arg|body>>:>
  >><assign|<merge|<arg|name>|*>|<macro|x|<surround|<render-keyword*|<unquote|<arg|body>>:>
  ||<arg|x>>>>>>>>

  <assign|algo-new-input|<macro|name|body|<quasiquote|<assign|<arg|name>|<macro|x|<surround|<render-keyword*|<unquote|<arg|body>>:>
  ||<arg|x>>>>>>>

  <assign|algo-new-in-out|<macro|name|body|<quasiquote|<assign|<arg|name>|<macro|x|<surround|<render-keyword*|<unquote|<arg|body>>:>
  ||<arg|x>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>