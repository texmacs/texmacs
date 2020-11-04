<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|header-article|1.0|header|1.0>

    <\src-purpose>
      Headers for articles.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|header-title|<macro|name|<simple-page>>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|<style-with|src-compact|none|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><with|font-shape|small-caps|<arg|name>><htab|5mm><page-number>>>><assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><page-number><htab|5mm><with|font-shape|small-caps|<arg|what>
  <arg|nr>>>>>>>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;

  <assign|cite-website|<macro|<localize|This article has been written using>
  GNU <TeXmacs><if|<equal|<value|language>|french>| ; |; ><localize|see>
  <hlink|<with|font-family|tt|www.texmacs.org>|https://www.texmacs.org>.>>

  <assign|cite-TeXmacs|<xmacro|keys|<localize|This article has been written
  using> GNU <TeXmacs><nbsp><map-args|identity|cite|keys>.>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>