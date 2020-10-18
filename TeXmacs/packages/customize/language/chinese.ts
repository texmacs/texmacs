<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|chinese|1.0>

    <\src-purpose>
      A style package for chinese language support
    </src-purpose>

    <\src-copyright|2013>
      Joris van der Hoeven
    </src-copyright>

    <\src-copyright|2020>
      Darcy Shen
    </src-copyright>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|language|chinese>

  <assign|font|sys-chinese>

  <if|<greater|<value|par-first>|0fn>|<assign|par-first|2fn>>

  <assign|par-spacing|kaiming>

  <assign|render-small-figure|<macro|type|name|fig|cap|<if|<equal|<arg|type>|table>|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|3|3|1|1|cell-lsep|0spc>|<cwith|3|3|1|1|cell-rsep|0spc>|<table|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <arg|cap>
    </surround>>
  </cell>>|<row|<cell|>>|<\row>
    <resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>
  </row>>>>|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<table|<row|<cell|<resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>>>|<row|<cell|>>|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <\html-div-class|float>
        <arg|cap>
      </html-div-class>
    </surround>>
  </cell>>>>>>>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<if|<equal|<arg|type>|table>|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|<value|figure-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|figure-right-padding>>|<cwith|1|1|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|1|1|1|1|cell-rsep|<value|caption-right-padding>>|<table|<\row>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </row>|<row|<cell|>>|<row|<cell|<arg|fig>>>>>>|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|caption-right-padding>>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <\html-div-class|float>
          <arg|cap>
        </html-div-class>
      </surround>>
    </cell>>>>>>>
  </macro>>

  \;
</body>

<initial|<\collection>
</collection>>