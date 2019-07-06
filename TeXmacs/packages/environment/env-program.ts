<TeXmacs|1.99.8>

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

  <use-package|env-pseudo-code>

  <\active*>
    <\src-comment>
      Code environments
    </src-comment>
  </active*>

  <assign|code-numbered-offset|0.5tab>

  <assign|render-code|<\macro|body>
    <\surround||<no-indent*>>
      <\padded*>
        <\indent>
          <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1tab>|numbered-offset|<value|code-numbered-offset>>
            <arg|body>
          </with>
        </indent>
      </padded*>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Algorithmic environments
    </src-comment>
  </active*>

  <assign|algorithm-name|<macro|name|<enunciation-name|<arg|name>>>>

  <assign|algorithm-sep|<macro|<enunciation-sep>>>

  <assign|algorithm-numbered-offset|0.5tab>

  <assign|render-algorithm|<\macro|name|body>
    <\padded*>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1tab>|numbered-offset|<value|algorithm-numbered-offset>>
        <surround||<vspace|0.5fn>|<algorithm-name|<arg|name>>>

        <\surround||<yes-indent*>>
          <\algorithm-indent>
            <arg|body>
          </algorithm-indent>
        </surround>
      </with>
    </padded*>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\padded*>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1tab>|numbered-offset|<value|algorithm-numbered-offset>>
        <\surround||<vspace|0.5fn>>
          <algorithm-name|<arg|name>>

          <arg|intro>
        </surround>

        <\surround||<yes-indent*>>
          <\algorithm-indent>
            <arg|body>
          </algorithm-indent>
        </surround>
      </with>
    </padded*>
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
    <padded-normal|<enunciation-padding>|<enunciation-padding>|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<cwith|3|3|1|1|cell-lsep|1.5fn>|<cwith|3|3|1|1|cell-rsep|1.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
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

  <assign|algorithm-indentation|1tab>

  <assign|indent-indentation|1tab>

  <assign|algorithm-indent|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-left|<plus|<value|par-left>|<value|algorithm-indentation>>>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|indent*|<\macro|body>
    <\with|par-left|<plus|<value|par-left>|<value|indent-indentation>>>
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

  <assign|tabbed*|<macro|body|<tformat|<twith|table-valign|T>|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|-1|-1|cell-vcorrect|n>|<arg|body>>>>

  <assign|wide-tabbed|<macro|body|<tformat|<twith|table-valign|T>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-2|1|-1|cell-bsep|<plus|<value|par-sep>|<value|par-line-sep>>>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Standard languages
    </src-comment>
  </active*>

  <assign|scheme|<macro|<name|Scheme>>>

  <assign|c++|<macro|<name|C++>>>

  <assign|mathemagix|<macro|<name|Mathemagix>>>

  <assign|scilab|<macro|<name|Scilab>>>

  <assign|fortran|<macro|<name|Fortran>>>

  <assign|python|<macro|<name|Python>>>

  <assign|scala|<macro|<name|Scala>>>

  <assign|r|<macro|<name|R>>>

  <assign|shell|<macro|body|<with|mode|prog|prog-language|shell|font-family|rm|<arg|body>>>>

  <assign|scm|<macro|body|<with|mode|prog|prog-language|scheme|font-family|rm|<arg|body>>>>

  <assign|cpp|<macro|body|<with|mode|prog|prog-language|cpp|font-family|rm|<arg|body>>>>

  <assign|python|<macro|body|<with|mode|prog|prog-language|python|font-family|rm|<arg|body>>>>

  <assign|scala|<macro|body|<with|mode|prog|prog-language|scala|font-family|rm|<arg|body>>>>

  <assign|r|<macro|body|<with|mode|prog|prog-language|r|font-family|rm|<arg|body>>>>

  <assign|scilab|<macro|body|<with|mode|prog|prog-language|scilab|font-family|rm|<arg|body>>>>

  <assign|fortran|<macro|body|<with|mode|prog|prog-language|fortran|font-family|rm|<arg|body>>>>

  <assign|mmx|<macro|body|<with|mode|prog|prog-language|mathemagix|font-family|rm|<arg|body>>>>

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

  <assign|scilab-code|<\macro|body>
    <\pseudo-code>
      <scilab|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|fortran-code|<\macro|body>
    <\pseudo-code>
      <fortran|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|python-code|<\macro|body>
    <\pseudo-code>
      <python|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|scala-code|<\macro|body>
    <\pseudo-code>
      <scala|<arg|body>>
    </pseudo-code>
  </macro>>

  <assign|r-code|<\macro|body>
    <\pseudo-code>
      <r|<arg|body>>
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
      Line numbered listings
    </src-comment>
  </active*>

  <assign|numbered-offset|-1spc>

  <assign|numbered-color|dark grey>

  <assign|numbered-font-size|0.841>

  <assign|numbered-block|<\macro|body>
    <\surround||<right-flush>>
      <\with|line-nr|0>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|render-line-number|<macro|nr|offset|<set-binding|<arg|nr>><line-note|<with|color|<value|numbered-color>|font-size|<value|numbered-font-size>|font-shape|upright|font-series|medium|<move|<arg|nr>|-1r|>>|<arg|offset>|0cm>>>

  <assign|numbered-line|<macro|body|<assign|line-nr|<plus|<value|line-nr>|1>><render-line-number|<value|line-nr>|<value|numbered-offset>><arg|body>>>

  <assign|numbered|<\macro|body>
    <\with|dummy1|<value|numbered-offset>|dummy2|<value|numbered-color>|dummy3|<value|numbered-font-size>>
      <extern|ext-numbered|<quote-arg|body>>
    </with>
  </macro>>

  <drd-props|numbered|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Fancy listings using stripes
    </src-comment>
  </active*>

  <assign|striped-table|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-background|<if|<equal|<mod|<value|cell-row-nr>|2>|0>|#f4f4ff|>>|<twith|table-width|1par>|<twith|table-hmode|exact>|<arg|body>>>>

  <assign|listing|<\macro|body>
    <extern|ext-listing|<quote-arg|body>>
  </macro>>

  <drd-props|listing|arity|1|accessible|all>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>
