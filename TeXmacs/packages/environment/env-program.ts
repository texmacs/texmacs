<TeXmacs|1.0.7.15>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|env-program|1.0>

    <\src-purpose>
      Some environments for typesetting algorithms.
    </src-purpose>

    <src-copyright|1998--2012|Joris van der Hoeven, François Poulain>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Algorithmic environments
    </src-comment>
  </active*>

  <assign|algorithm-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|algorithm-sep|<macro|<enunciation-sep>>>

  <assign|render-code|<\macro|body>
    <\padded-normal|1fn|1fn>
      <\indent>
        <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
          <arg|body>
        </with>
      </indent>
    </padded-normal>
  </macro>>

  <assign|render-algorithm|<\macro|name|body>
    <\padded-normal|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
        <surround||<vspace|0.5fn>|<algorithm-name|<arg|name>>>

        <\surround||<yes-indent*>>
          <\indent>
            <arg|body>
          </indent>
        </surround>
      </with>
    </padded-normal>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\padded-normal|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
        <\surround||<vspace|0.5fn>>
          <algorithm-name|<arg|name>>

          <arg|intro>
        </surround>

        <\surround||<yes-indent*>>
          <\indent>
            <arg|body>
          </indent>
        </surround>
      </with>
    </padded-normal>
  </macro>>

  <new-algorithm|algorithm|Algorithm>

  <\active*>
    <\src-comment>
      Floating algorithmic environments
    </src-comment>
  </active*>

  <new-figure|algorithm|Algorithm>

  <assign|render-small-algorithm|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<table|<row|<cell|<resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>>>|<row|<cell|>>|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <arg|cap>
    </surround>>
  </cell>>>>>>>

  <assign|render-big-algorithm|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<cwith|3|3|1|1|cell-lsep|1.5fn>|<cwith|3|3|1|1|cell-rsep|1.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>>>>>
  </macro>>

  \;

  <assign|algorithm-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <\active*>
    <\src-comment>
      Special layout markup for algorithms
    </src-comment>
  </active*>

  <assign|indent*|<\macro|body>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent|<\macro|body>
    <\surround||<right-flush>>
      <\indent*>
        <arg|body>
      </indent*>
    </surround>
  </macro>>

  <assign|tabbed|<macro|body|<tformat|<twith|table-valign|T>|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-2|1|-1|cell-bsep|<plus|<value|par-sep>|<value|par-line-sep>>>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Standard languages
    </src-comment>
  </active*>

  <assign|scheme|<macro|<name|Scheme>>>

  <assign|c++|<macro|<name|C++>>>

  <assign|mathemagix|<macro|<name|Mathemagix>>>

  <assign|shell|<macro|body|<with|mode|prog|prog-language|shell|<arg|body>>>>

  <assign|scm|<macro|body|<with|mode|prog|prog-language|scheme|<arg|body>>>>

  <assign|cpp|<macro|body|<with|mode|prog|prog-language|cpp|<arg|body>>>>

  <assign|mmx|<macro|body|<with|mode|prog|prog-language|mathemagix|<arg|body>>>>

  <\active*>
    <\src-comment>
      Blocks of code for standard languages
    </src-comment>
  </active*>

  <assign|pseudo-code|<\macro|body>
    <\render-code>
      <arg|body>
    </render-code>
  </macro>>

  <assign|verbatim-code|<\macro|body>
    <\pseudo-code>
      <verbatim|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|scm-code|<\macro|body>
    <\pseudo-code>
      <scm|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|shell-code|<\macro|body>
    <\pseudo-code>
      <shell|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|cpp-code|<\macro|body>
    <\pseudo-code>
      <cpp|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|mmx-code|<\macro|body>
    <\pseudo-code>
      <mmx|<arg|body>>
    </pseudo-code>
  </macro>>

  <\active*>
    <\src-comment>
      Deprecated markup for algorithms
    </src-comment>
  </active*>

  <assign|named-algorithm-old|<macro|name|body|<surround|<vspace*|1fn>|<vspace|1fn><right-flush><yes-indent*>|<with|par-first|0cm|<surround|<with|font-series|bold|<translate|Algorithm|english|<language>>>
  <tt|<arg|name>>||<with|item*|<macro|what|<with|font-series|bold|math-font-series|bold|<arg|what>
  >>|<arg|body>>>>>>>

  <assign|algorithm-body|<macro|body|<surround|<vspace*|0.5fn>|<yes-indent*>|<with|item*|<macro|name|<vspace*|0.5fn><with|font-series|bold|math-font-series|bold|<arg|name>
  >>|<arg|body>>>>>

  <assign|minimal|<macro|body|<with|mode|prog|prog-language|minimal|<arg|body>>>>

  <\active*>
    <\src-comment>
      Algorithm pseudocode keywords
    </src-comment>
  </active*>

  <assign|algo-if|<\macro|cond|body>
    <render-if> <arg|cond> <render-then>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-else-if|<\macro|cond|body>
    <render-else> <render-if> <arg|cond> <render-then>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-else|<\macro|body>
    <render-else>

    <indent|<arg|body>>
  </macro>>

  <assign|algo-make-if-else-if|<\macro|args>
    <compound|<if|<equal|<length|<arg|args>>|0>|algo-null|<compound|<if|<equal|<length|<arg|args>>|1>|algo-else|<compound|<if|<greatereq|<length|<arg|args>>|2>|algo-else-if|algo-null>|<look-up|<arg|args>|0>|<look-up|<arg|args>|1>>>|<look-up|<arg|args>|0>>>>

    <compound|<if|<greater|<length|<arg|args>>|2>|algo-make-if-else-if|<render-end-if>>|<range|<arg|args>|2|<length|<arg|args>>>>
  </macro>>

  <assign|algo-if-else-if|<\xmacro|test-seq>
    <algo-if|<arg|test-seq|0>|<arg|test-seq|1>>

    <\with|args|<map-args|identity|tuple|test-seq>|arity|<length|<map-args|identity|tuple|test-seq>>>
      <compound|<if|<greater|<arity>|2>|algo-make-if-else-if|render-end-if>|<range|<value|args>|2|<arity>>>
    </with>
  </xmacro>>

  <assign|algo-for|<\macro|cond|block>
    <render-for> <arg|cond> <render-do>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for>
  </macro>>

  <assign|algo-for-all|<\macro|cond|block>
    <render-for-all> <arg|cond> <render-do>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for>
  </macro>>

  <assign|algo-for-each|<\macro|cond|block>
    <render-for-each> <arg|cond> <render-do>

    <\indent>
      <arg|block>
    </indent>

    <render-end-for>
  </macro>>

  <assign|algo-to|<macro|<render-to>>>

  <assign|algo-globals|<macro|<render-globals>>>

  <assign|algo-while|<\macro|cond|block>
    <render-while> <arg|cond> <render-do>

    <\indent>
      <arg|block>
    </indent>

    <render-end-while>
  </macro>>

  <assign|algo-repeat|<\macro|cond|block>
    <render-repeat>

    <\indent>
      <arg|block>
    </indent>

    <render-until> \ <arg|cond>
  </macro>>

  <assign|algo-body|<\macro|block>
    <render-do>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-begin|<\macro|block>
    <render-begin>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-inputs|<\macro|block>
    <render-inputs>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-outputs|<\macro|block>
    <render-outputs>

    <\indent>
      <arg|block>
    </indent>
  </macro>>

  <assign|algo-loop|<\macro|block>
    <render-loop>

    <\indent>
      <arg|block>
    </indent>

    <render-end-loop>
  </macro>>

  <assign|algo-procedure|<\macro|name|args|body>
    <render-procedure> <with|font-shape|small-caps|<arg|name>>(<arg|args>)

    <\indent>
      <arg|body>
    </indent>

    <render-end-procedure>
  </macro>>

  <assign|algo-function|<\macro|name|args|body>
    <render-function> <with|font-shape|small-caps|<arg|name>>(<arg|args>)

    <\indent>
      <arg|body>
    </indent>

    <render-end-function>
  </macro>>

  <assign|algo-call|<\macro|name|args>
    <with|font-shape|small-caps|<arg|name>>(<arg|args>)
  </macro>>

  <assign|algo-print|<macro|body|<render-print> <arg|body>>>

  <assign|algo-comment|<macro|body|<with|font-shape|italic|color|dark
  grey|{<arg|body>}>>>

  <assign|algo-require|<macro|body|<render-require> <arg|body>>>

  <assign|algo-data|<macro|body|<render-data> <arg|body>>>

  <assign|algo-result|<macro|body|<render-result> <arg|body>>>

  <assign|algo-ensure|<macro|body|<render-ensure> <arg|body>>>

  <assign|algo-return|<macro|body|<render-return> <arg|body>>>

  <assign|algo-state|<macro|body|<arg|body>>>

  <assign|algo-and|<macro|<render-and>>>

  <assign|algo-or|<macro|<render-or>>>

  <assign|algo-xor|<macro|<render-xor>>>

  <assign|algo-not|<macro|<render-not>>>

  <assign|algo-true|<macro|<render-true>>>

  <assign|algo-false|<macro|<render-false>>>

  <\active*>
    <\src-comment>
      Algorithm constructors
    </src-comment>
  </active*>

  <assign|algo-new-keyword|<macro|name|body|<assign|<arg|name>|<with|font-family|ss|<arg|body>>>>>

  <assign|algo-new-function|<macro|name|body|<quasiquote|<assign|<merge|<arg|name>|*>|<\macro|x>
    <with|font-series|bold|<unquote|<arg|body>>:> <arg|x>
  </macro>><assign|<arg|name>|<macro|<with|font-series|bold|<unquote|<arg|body>>:>
  >>>>>

  <assign|algo-new-data|<macro|name|body|<quasiquote|<assign|<merge|<arg|name>|*>|<\macro|x>
    <with|font-series|bold|<unquote|<arg|body>>:> <arg|x>
  </macro>><assign|<arg|name>|<macro|<with|font-series|bold|<unquote|<arg|body>>:>
  >>>>>

  <assign|algo-new-input|<macro|name|body|<quasiquote|<assign|<arg|name>|<\macro|x>
    <with|font-series|bold|<unquote|<arg|body>>:> <arg|x>
  </macro>>>>>

  <assign|algo-new-in-out|<macro|name|body|<quasiquote|<assign|<arg|name>|<\macro|x>
    <with|font-series|bold|<unquote|<arg|body>>:> <arg|x>
  </macro>>>>>

  <assign|algo-number-label|<macro|lab|<label|<arg|lab>>>>

  <assign|algo-label|<macro|lab|<label|<arg|lab>>>>

  <\active*>
    <\src-comment>
      Rendering style for pseudo-code
    </src-comment>
  </active*>

  <assign|render-new-line|<macro|>>

  <assign|render-inc-indent|<macro|<assign|par-left|<plus|<value|par-left>|1.5fn>>>>

  <assign|render-dec-indent|<macro|<assign|par-left|<plus|<value|par-left>|1.5fn>>>>

  <assign|render-begin|<macro|<with|font-series|bold|begin>>>

  <assign|render-call|<macro|<with|font-series|bold|call>>>

  <assign|render-data|<macro|<with|font-series|bold|Data:>>>

  <assign|render-do|<macro|<with|font-series|bold|do>>>

  <assign|render-else-if|<macro|<render-else> <render-if>>>

  <assign|render-else|<macro|<with|font-series|bold|else>>>

  <assign|render-end-for|<macro|<render-end> <render-for>>>

  <assign|render-end-function|<macro|<render-end> <render-function>>>

  <assign|render-end-if|<macro|<render-end> <render-if>>>

  <assign|render-end-loop|<macro|<render-end> <render-loop>>>

  <assign|render-end-procedure|<macro|<render-end> <render-procedure>>>

  <assign|render-end-while|<macro|<render-end> <render-while>>>

  <assign|render-end|<macro|<with|font-series|bold|end>>>

  <assign|render-ensure|<macro|<with|font-series|bold|Ensure:>>>

  <assign|render-false|<macro|<with|font-series|bold|false>>>

  <assign|render-for-all|<macro|<with|font-series|bold|for all>>>

  <assign|render-for-each|<macro|<with|font-series|bold|for each>>>

  <assign|render-for|<macro|<with|font-series|bold|for>>>

  <assign|render-function|<macro|<with|font-series|bold|function>>>

  <assign|render-globals|<macro|<with|font-series|bold|globals>>>

  <assign|render-if|<macro|<with|font-series|bold|if>>>

  <assign|render-inputs|<macro|<with|font-series|bold|inputs>>>

  <assign|render-loop|<macro|<with|font-series|bold|loop>>>

  <assign|render-outputs|<macro|<with|font-series|bold|outputs>>>

  <assign|render-print|<macro|<with|font-series|bold|print>>>

  <assign|render-procedure|<macro|<with|font-series|bold|procedure>>>

  <assign|render-repeat|<macro|<with|font-series|bold|repeat>>>

  <assign|render-require|<macro|<with|font-series|bold|Require:>>>

  <assign|render-result|<macro|<with|font-series|bold|Result:>>>

  <assign|render-return|<macro|<with|font-series|bold|return>>>

  <assign|render-then|<macro|<with|font-series|bold|then>>>

  <assign|render-true|<macro|<with|font-series|bold|true>>>

  <assign|render-until|<macro|<with|font-series|bold|until>>>

  <assign|render-while|<macro|<with|font-series|bold|while>>>

  <assign|render-and|<macro|<with|font-series|bold|and>>>

  <assign|render-xor|<macro|<with|font-series|bold|xor>>>

  <assign|render-or|<macro|<with|font-series|bold|or>>>

  <assign|render-not|<macro|<with|font-series|bold|not>>>

  <assign|render-to|<macro|<with|font-series|bold|to>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>