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
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|figurename|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|figuresep|<macro|. >>

  <assign|footnotesep|<macro|. >>

  \;

  <assign|list-caption|<macro|type|cap|<style-with|src-compact|none|<assign|glynr|<plus|<value|glynr>|1>><label|<thegly>><write|<arg|type>|<tuple|normal|<arg|cap>|<pageref|<thegly>>>>>>>

  <assign|small-figure*|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<table|<row|<cell|<resize|<arg|fig>|l-2fn||r+2fn||>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<figurename|<arg|name><figuresep>><list-caption|<arg|type>|<arg|cap>>||<arg|cap>>>
  </cell>>>>>>>

  <assign|big-figure*|<macro|type|name|fig|cap|<surround|<vspace*|1fn><no-indent>|<vspace|1fn>|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
    <with|font-size|0.84|<surround|<figurename|<arg|name><figuresep><list-caption|<arg|type>|<arg|cap>>>||<arg|cap>>>
  </cell>>>>>>>>

  \;

  <assign|footnote|<macro|x|<style-with|src-compact|none|<assign|footnotenr|<plus|<footnotenr>|1>><assign|thelabel|<thefootnote>><style-with|src-compact|none|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<thefootnote><footnotesep><label|<merge|footnote-|<thefootnote>>>|<rightflush>|<style-with|src-compact|none|<arg|x>>>>>>><space|0spc><rsup|<reference|<merge|footnote-|<thefootnote>>>>>>>

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