<TeXmacs|1.0.7.9>

<style|source>

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
      Centered environments with horizontal rulers
    </src-comment>
  </active*>

  <assign|narrow-bothlined|<macro|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|1|1|1|1|cell-bborder|1ln>|<cwith|1|1|1|1|cell-tsep|1sep>|<cwith|1|1|1|1|cell-bsep|1sep>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|narrow-bothlined*|<macro|intro|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|1|1|cell-lsep|0pt>|<cwith|1|-1|1|1|cell-rsep|0pt>|<cwith|1|-1|1|1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-tsep|1sep>|<cwith|1|-1|1|1|cell-bsep|1sep>|<table|<row|<\cell>
    <arg|intro>
  </cell>>|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <\active*>
    <\src-comment>
      Customizations of algorithmic environments
    </src-comment>
  </active*>

  <assign|render-sole-algorithm|<\macro|body>
    <\padded>
      <\with|par-indent|0fn|par-par-sep|0fn|par-mode|center>
        <\narrow-bothlined>
          <\with|par-mode|justify>
            <arg|body>
          </with>
        </narrow-bothlined>
      </with>
    </padded>
  </macro>>

  <assign|render-algorithm|<\macro|name|body>
    <\padded>
      <\with|par-indent|0fn|par-par-sep|0fn|par-mode|center>
        <\narrow-bothlined*|<algorithm-name|<arg|name>>>
          <arg|body>
        </narrow-bothlined*>
      </with>
    </padded>
  </macro>>

  <assign|render-specified-algorithm|<\macro|name|intro|body>
    <\padded>
      <\with|par-indent|0fn|par-par-sep|0fn|par-mode|center>
        <\narrow-bothlined*>
          <algorithm-name|<arg|name>>

          <arg|intro>
        <|narrow-bothlined*>
          <arg|body>
        </narrow-bothlined*>
      </with>
    </padded>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>