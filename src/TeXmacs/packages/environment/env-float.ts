<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-float|1.0>

    <\src-purpose>
      Environments for floating content.
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

  <assign|figure-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|figure-sep|<macro|. >>

  <assign|footnote-sep|<macro|. >>

  \;

  <assign|list-caption|<macro|type|cap|<style-with|src-compact|none|<assign|gly-nr|<plus|<value|gly-nr>|1>><label|<the-gly>><write|<arg|type>|<tuple|normal|<arg|cap>|<pageref|<the-gly>>>>>>>

  <assign|small-figure*|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<table|<row|<cell|<resize|<arg|fig>|l-2fn||r+2fn||>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>||<arg|cap>>>
  </cell>>>>>>>

  <assign|big-figure*|<macro|type|name|fig|cap|<surround|<vspace*|1fn><no-indent>|<vspace|1fn>|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<figure-name|<arg|name><figure-sep><list-caption|<arg|type>|<arg|cap>>>||<arg|cap>>>
  </cell>>>>>>>>

  \;

  <assign|footnote|<macro|x|<style-with|src-compact|none|<next-footnote><style-with|src-compact|none|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<the-footnote><footnote-sep><label|<merge|footnote-|<the-footnote>>>|<right-flush>|<style-with|src-compact|none|<arg|x>>>>>>><space|0spc><rsup|<reference|<merge|footnote-|<the-footnote>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>