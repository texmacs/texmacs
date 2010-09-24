<TeXmacs|1.0.7.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|program|1.0>

    <\src-purpose>
      Some environments for typesetting algorithms. This package is very
      simplistic and should be replaced by something more elaborate in the
      future.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|simple-prog|<macro|x|<with|mode|prog|prog-language|simple|<highlight|<arg|x>>>>>

  <assign|algo|<macro|name|<with|font-family|tt|math-font-family|ttt|<arg|name>>>>

  <assign|algorithm|<macro|name|body|<surround|<vspace*|1fn>|<vspace|1fn><right-flush>|<with|par-first|0cm|<surround|<with|font-series|bold|<translate|Algorithm|english|<language>>>
  <algo|<arg|name>>||<with|item*|<macro|what|<with|font-series|bold|math-font-series|bold|<arg|what>
  >>|<arg|body>>>>>>>

  <assign|body|<macro|body|<surround|<vspace*|0.5fn>||<with|item*|<macro|name|<vspace*|0.5fn><with|font-series|bold|math-font-series|bold|<arg|name>
  >>|<arg|body>>>>>

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

  <assign|pile|<macro|x|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0.5sep>|<cwith|1|-1|1|-1|cell-tsep|0.5sep>|<cwith|1|1|1|-1|cell-tsep|0sep>|<cwith|-1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>