<TeXmacs|1.0.7.9>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|env-program|1.0>

    <\src-purpose>
      Some environments for typesetting algorithms.
    </src-purpose>

    <src-copyright|1998--2011|Joris van der Hoeven>

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

  <assign|pseudo-code|<\macro|body>
    <\padded-normal|1fn|1fn>
      <\indent>
        <\with|par-first|0fn|par-par-sep|0fn>
          <arg|body>
        </with>
      </indent>
    </padded-normal>
  </macro>>

  <assign|render-algorithm|<\macro|name|body>
    <\padded-normal|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn>
        <surround||<vspace|0.5fn>|<algorithm-name|<arg|name>>>

        <\indent>
          <arg|body>
        </indent>
      </with>
    </padded-normal>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\padded-normal|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn>
        <\surround||<vspace|0.5fn>>
          <algorithm-name|<arg|name>>

          <arg|intro>
        </surround>

        <\indent>
          <arg|body>
        </indent>
      </with>
    </padded-normal>
  </macro>>

  <new-algorithm|algorithm|Algorithm>

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

  <assign|named-algorithm-old|<macro|name|body|<surround|<vspace*|1fn>|<vspace|1fn><right-flush>|<with|par-first|0cm|<surround|<with|font-series|bold|<translate|Algorithm|english|<language>>>
  <tt|<arg|name>>||<with|item*|<macro|what|<with|font-series|bold|math-font-series|bold|<arg|what>
  >>|<arg|body>>>>>>>

  <assign|algorithm-body|<macro|body|<surround|<vspace*|0.5fn>||<with|item*|<macro|name|<vspace*|0.5fn><with|font-series|bold|math-font-series|bold|<arg|name>
  >>|<arg|body>>>>>

  <assign|minimal|<macro|body|<with|mode|prog|prog-language|minimal|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>