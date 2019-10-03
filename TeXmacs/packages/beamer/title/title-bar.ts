<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|title-bar|1.0>

    <\src-purpose>
      A style package for title bars
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Customized title
    </src-comment>
  </active*>

  <assign|title-theme|title-bar>

  <assign|title-sub-bar-frame|<macro|body|<art-box|<surround||<right-flush>|<arg|body>>|<tuple|text|normal|lpadding|1spc|rpadding|1spc|tpadding|1sep|bpadding|1sep>>>>

  <assign|title-sub-bar-contents|<macro|body|<with|color|#888|locus-color|#888|opacity|100%|<tiny|<screens-bar|<quote-arg|body>>>>>>

  <assign|tit|<\macro|body>
    <\surround|<assign|page-this-top|0mm>|<assign|gpag-length|<macro|<minus|1pag|<plus|<times|2|<value|title-vpadding>>|<times|<value|title-font-size>|1.5fn>|0.8fn|<value|par-par-sep>>>>>>
      <\with|par-left|<minus|<value|page-odd>>|par-right|<minus|<value|page-right>>|par-par-sep|0fn>
        <with|color|<value|title-shadow-color>|font-size|<if|<value|title-old-style>|1|<value|title-font-size>>|<resize|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|-1|cell-tsep|<value|title-vpadding>>|<cwith|1|1|1|-1|cell-bsep|<value|title-vpadding>>|<cwith|1|1|1|-1|cell-background|<title-bar-color>>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-vcorrect|n>|<cwith|2|2|1|1|cell-halign|l>|<twith|table-valign|T>|<cwith|1|1|1|1|cell-bborder|<value|title-border>>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-lsep|0spc>|<cwith|2|2|1|1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-bsep|0spc>|<cwith|2|2|1|1|cell-tsep|0spc>|<table|<row|<\cell>
          <with-tit-color|<title-left|<arg|body>><htab|5mm><arg|body><htab|5mm><title-right|<arg|body>>>
        </cell>>|<row|<\cell>
          <title-sub-bar-frame|<title-sub-bar-contents|<arg|body>>>
        </cell>>>>>||<plus|1b|0.2fn>||>>
      </with>
    </surround>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>