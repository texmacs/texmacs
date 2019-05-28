<TeXmacs|1.99.9>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|centered-program|1.0>

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

  <\active*>
    <\src-comment>
      Centered environments with horizontal rulers
    </src-comment>
  </active*>

  <assign|narrow-code|<macro|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-bsep|1spc>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|tmlatex-narrow-code|<\macro|body>
    <hrule>

    <arg|body>

    <hrule>
  </macro>>

  <assign|narrow-bothlined|<macro|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-bsep|1spc>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|tmlatex-narrow-bothlined|<\macro|body>
    <hrule>

    <arg|body>

    <hrule>
  </macro>>

  <assign|narrow-bothlined*|<macro|intro|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|1|1|-1|cell-background|<value|intro-color>>|<cwith|2|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|-1|1|1|cell-lsep|0pt>|<cwith|1|-1|1|1|cell-rsep|0pt>|<cwith|1|-1|1|1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-tsep|1spc>|<cwith|1|-1|1|1|cell-bsep|1spc>|<table|<row|<\cell>
    <arg|intro>
  </cell>>|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <assign|tmlatex-narrow-bothlined*|<\macro|intro|body>
    <hrule>

    <arg|intro>

    <hrule>

    <arg|body>

    <hrule>
  </macro>>

  <\active*>
    <\src-comment>
      Customizations of algorithmic environments
    </src-comment>
  </active*>

  <assign|render-code|<\macro|body>
    <\padded-centered|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
        <\narrow-bothlined>
          <arg|body>
        </narrow-bothlined>
      </with>
    </padded-centered>
  </macro>>

  <assign|render-algorithm|<\macro|name|body>
    <\padded-centered|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
        <\narrow-bothlined*|<algorithm-name|<arg|name>>>
          <arg|body>
        </narrow-bothlined*>
      </with>
    </padded-centered>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\padded-centered|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|1.5fn>
        <\narrow-bothlined*>
          <algorithm-name|<arg|name>>

          <arg|intro>
        <|narrow-bothlined*>
          <arg|body>
        </narrow-bothlined*>
      </with>
    </padded-centered>
  </macro>>

  <assign|pseudo-code|<\macro|body>
    <\padded-centered|1fn|1fn>
      <\with|par-first|0fn|par-par-sep|0fn|item-hsep|<macro|1.5fn>>
        <\narrow-code>
          <arg|body>
        </narrow-code>
      </with>
    </padded-centered>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>