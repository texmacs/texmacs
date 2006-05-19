<TeXmacs|1.0.5.6>

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

  <\active*>
    <\src-comment>
      Figure-like environments.
    </src-comment>
  </active*>

  <assign|list-caption|<macro|type|cap|<style-with|src-compact|none|<auto-label><write|<arg|type>|<tuple|normal|<arg|cap>|<pageref|<the-auto>>>>>>>

  <assign|render-small-figure|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<table|<row|<cell|<resize|<arg|fig>|l-2fn||r+2fn||>>>|<row|<cell|>>|<row|<\cell>
    <small|<surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>||<arg|cap>>>
  </cell>>>>>>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<cwith|3|3|1|1|cell-lsep|1.5fn>|<cwith|3|3|1|1|cell-rsep|1.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>>>>>
  </macro>>

  <new-figure|figure|Figure>

  <new-figure|table|Table>

  <\active*>
    <\src-comment>
      Footnotes.
    </src-comment>
  </active*>

  <assign|render-footnote|<macro|nr|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<arg|nr><footnote-sep><label|<merge|footnote-|<arg|nr>>>|<right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>

  <assign|footnote|<macro|body|<style-with|src-compact|none|<next-footnote><render-footnote|<the-footnote>|<arg|body>><space|0spc><rsup|<reference|<merge|footnote-|<the-footnote>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>