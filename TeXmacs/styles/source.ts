<TeXmacs|1.99.8>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|source|1.0>

      <\src-purpose>
        This style should be used for editing style files and packages.
      </src-purpose>

      <\src-copyright|2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <assign|source-dtd|1.0>

  <\active*>
    <\src-comment>
      Environment variables
    </src-comment>
  </active*>

  <assign|preamble|true>

  <assign|mode|src>

  <assign|par-first|0fn>

  <assign|par-par-sep|0.5fn>

  <assign|padded-par-par-sep|0.5fn>

  <assign|indent-par-first|1.5fn>

  <\active*>
    <\src-comment>
      Copy a few environments from std-markup.
    </src-comment>
  </active*>

  <assign|TeXmacs|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>>>>

  <assign|right-flush|<macro|<htab|0fn|first>>>

  <assign|verbatim|<macro|body|<with|font-family|tt|language|verbatim|<arg|body>>>>

  <assign|block|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Tags for entering the name of the style file or package and further
      information
    </src-comment>
  </active*>

  <assign|src-title|<\macro|x>
    <\surround|<vspace*|1.5fn>|<vspace|1.5fn>>
      <with|color|dark blue|<block|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-bsep|1.5spc>|<cwith|1|1|1|1|cell-tsep|1.5spc>|<cwith|1|1|1|1|cell-background|pastel
      blue>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-hpart|1>|<table|<row|<\cell>
        <with|font-size|1|font-family|ss|color|dark
        blue|par-par-sep|0fn|<arg|x>>
      </cell>>>>>>
    </surround>
  </macro>>

  <assign|src-title-line|<\macro|x|y>
    <tabular|<tformat|<twith|table-width|1par>|<twith|table-valign|T>|<cwith|1|1|1|1|cell-width|5fn>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-hpart|1>|<table|<row|<cell|<with|font-series|bold|<arg|x>:
    >>|<\cell>
      <arg|y>
    </cell>>>>>
  </macro>>

  <assign|src-style-file|<macro|x|y|<\surround|<assign|<merge|<arg|x>|-style>|<arg|y>>|>
    <src-title-line|Style|<arg|x>-<arg|y>>
  </surround>>>

  <assign|src-package|<macro|x|y|<\surround|<assign|<merge|<arg|x>|-package>|<arg|y>><assign|<merge|<arg|x>|-dtd>|<arg|y>>|>
    <src-title-line|Package|<arg|x>-<with|font-shape|italic|<arg|y>>>
  </surround>>>

  <assign|src-package-dtd|<macro|x|y|a|b|<\surround|<assign|<merge|<arg|x>|-package>|<arg|y>><assign|<merge|<arg|a>|-dtd>|<arg|b>>|>
    <src-title-line|Package|<arg|x>-<with|font-shape|italic|<arg|y>>>

    <src-title-line|Dtd|<arg|a>-<with|font-shape|italic|<arg|b>>>
  </surround>>>

  <assign|src-purpose|<\macro|x>
    <src-title-line|Purpose|<arg|x>>
  </macro>>

  <assign|src-copyright|<\macro|x|y>
    <src-title-line|Copyright|<\surround|<active*|<with|font|tcx|©>> <arg|x>
    by |>
      <arg|y>
    </surround>>
  </macro>>

  <assign|src-license|<\macro|x>
    <src-title-line|License|<with|font-size|0.84|<arg|x>>>
  </macro>>

  <\active*>
    <\src-comment>
      An environment for comments in style files or packages.
    </src-comment>
  </active*>

  <assign|src-short-comment|<macro|x|<with|color|dark grey|<htab|5mm>{
  <arg|x> }>>>

  <assign|src-comment|<\macro|x>
    <surround|<vspace*|1.5fn>|<vspace|0.5fn>|<with|color|dark
    grey|<block|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-bsep|1spc>|<cwith|1|1|1|1|cell-tsep|1spc>|<cwith|1|1|1|1|cell-background|pastel
    yellow>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<\cell>
      <with|font-size|0.84|font-shape|slanted|<arg|x>>
    </cell>>>>>>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>