<TeXmacs|1.0.7.9>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|framed-program|1.0>

    <\src-purpose>
      Variant of env-program for centered algorithms with horizontal rulers.
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
      Extra style parameters
    </src-comment>
  </active*>

  <assign|intro-color|>

  <assign|body-color|>

  <assign|frame-color|black>

  <\active*>
    <\src-comment>
      Framed programs
    </src-comment>
  </active*>

  <assign|framed-quoted|<\macro|body>
    <\padded-normal|1fn|1fn>
      <\indent-both|1.5fn|1.5fn>
        <arg|body>
      </indent-both>
    </padded-normal>
  </macro>>

  <assign|framed-program|<macro|body|<\surround|<no-indent>|>
    <with|old-color|<value|color>|color|<value|frame-color>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<table|<row|<\cell>
      <with|color|<value|old-color>|<arg|body>>
    </cell>>>>>>
  </surround>>>

  <assign|framed-program*|<macro|intro|body|<\surround|<no-indent>|>
    <with|old-color|<value|color>|color|<value|frame-color>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|1|1|-1|cell-background|<value|intro-color>>|<cwith|2|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<table|<row|<\cell>
      <with|color|<value|old-color>|<arg|intro>>
    </cell>>|<row|<\cell>
      <with|color|<value|old-color>|<arg|body>>
    </cell>>>>>>
  </surround>>>

  <\active*>
    <\src-comment>
      Customizations of algorithmic environments
    </src-comment>
  </active*>

  <assign|pseudo-code|<\macro|body>
    <\framed-quoted>
      <\with|par-first|0fn|par-par-sep|0fn|par-mode|>
        <\framed-program>
          <arg|body>
        </framed-program>
      </with>
    </framed-quoted>
  </macro>>

  <assign|render-algorithm|<\macro|name|body>
    <\framed-quoted>
      <\with|par-first|0fn|par-par-sep|0fn>
        <\framed-program*|<algorithm-name|<arg|name>>>
          <arg|body>
        </framed-program*>
      </with>
    </framed-quoted>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\framed-quoted>
      <\with|par-first|0fn|par-par-sep|0fn>
        <\framed-program*>
          <algorithm-name|<arg|name>>

          <arg|intro>
        <|framed-program*>
          <arg|body>
        </framed-program*>
      </with>
    </framed-quoted>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>