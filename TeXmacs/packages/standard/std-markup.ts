<TeXmacs|1.0.7.12>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-markup|1.0>

    <\src-purpose>
      This package contains several frequently used macros.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      The following macros don't take any arguments.
    </src-comment>
  </active*>

  <assign|TeXmacs|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>>>>

  <assign|TeXmacs-version|<macro|<extern|texmacs-version>>>

  <assign|TeXmacs-version-release|<macro|type|<extern|texmacs-version-release*|<quote-arg|type>>>>

  <assign|made-by-TeXmacs|<macro|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<active*|<move|<image|local:$TEXMACS_PATH/misc/images/tm_gnu3.ps||1fn||>|0fn|-0.2fn>><space|2spc><localize|This
  document has been produced using> GNU <TeXmacs> (<localize|see>
  <with|font-family|tt|http://www.texmacs.org>).<right-flush>>>>>

  <assign|TeX|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|LaTeX|<macro|<active*|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|BibTeX|<macro|<active*|<with|font-shape|small-caps|Bib>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|hrule|<macro|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|-1|cell-height|1ln>|<cwith|1|-1|1|-1|cell-lsep|0fn>|<cwith|1|-1|1|-1|cell-rsep|0fn>|<cwith|1|-1|1|-1|cell-bsep|0fn>|<cwith|1|-1|1|-1|cell-tsep|0fn>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>>

  <assign|pre-edit|<macro|body|<with|ornament-color|#ffffc0|ornament-sunny-color|#fff0e0|ornament-shadow-color|#ffc080|ornament-border|1px|ornament-hpadding|2px|ornament-vpadding|2px|<smash|<ornament|<with|color|#4040c0|<arg|body>>>>>>>

  <drd-props|pre-edit|arity|1|border|no>

  <\active*>
    <\src-comment>
      Size tags (very-small: scriptsize, smaller: footnotesize, larger:
      Large, very-large: LARGE, really-huge: HUGE)
    </src-comment>
  </active*>

  <assign|really-tiny|<macro|body|<with|font-size|0.5|<arg|body>>>>

  <assign|very-tiny|<macro|body|<with|font-size|0.545|<arg|body>>>>

  <assign|tiny|<macro|body|<with|font-size|0.595|<arg|body>>>>

  <assign|really-small|<macro|body|<with|font-size|0.648|<arg|body>>>>

  <assign|very-small|<macro|body|<with|font-size|0.707|<arg|body>>>>

  <assign|smaller|<macro|body|<with|font-size|0.771|<arg|body>>>>

  <assign|small|<macro|body|<with|font-size|0.841|<arg|body>>>>

  <assign|flat-size|<macro|body|<with|font-size|0.917|<arg|body>>>>

  <assign|normal-size|<macro|body|<with|font-size|1|<arg|body>>>>

  <assign|sharp-size|<macro|body|<with|font-size|1.091|<arg|body>>>>

  <assign|large|<macro|body|<with|font-size|1.189|<arg|body>>>>

  <assign|larger|<macro|body|<with|font-size|1.297|<arg|body>>>>

  <assign|very-large|<macro|body|<with|font-size|1.414|<arg|body>>>>

  <assign|really-large|<macro|body|<with|font-size|1.542|<arg|body>>>>

  <assign|huge|<macro|body|<with|font-size|1.682|<arg|body>>>>

  <assign|very-huge|<macro|body|<with|font-size|1.834|<arg|body>>>>

  <assign|really-huge|<macro|body|<with|font-size|2|<arg|body>>>>

  <assign|font-zoom|<macro|factor|body|<with|font-size|<times|<value|font-size>|<arg|factor>>|<arg|body>>>>

  <assign|font-unzoom|<macro|factor|body|<with|font-size|<over|<value|font-size>|<arg|factor>>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Below follow some frequently used content tags.
    </src-comment>
  </active*>

  <assign|strong|<macro|body|<with|font-series|bold|math-font-series|bold|<arg|body>>>>

  <assign|em|<macro|body|<with|font-shape|<if|<equal|<value|font-shape>|italic>|right|italic>|<arg|body>>>>

  <assign|tt|<macro|body|<with|font-family|tt|math-font-family|ttt|<arg|body>>>>

  <assign|name|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  <assign|samp|<macro|body|<with|font-family|ss|<arg|body>>>>

  <assign|abbr|<macro|body|<rigid|<arg|body>>>>

  <assign|math|<macro|body|<with|mode|math|<arg|body>>>>

  <assign|text|<macro|body|<with|mode|text|<arg|body>>>>

  <assign|cite*|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <assign|dfn|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <assign|code*|<macro|body|<with|font-family|tt|<arg|body>>>>

  <assign|kbd|<macro|body|<with|font-family|tt|<arg|body>>>>

  <assign|var|<macro|body|<with|font-family|tt|font-shape|italic|<arg|body>>>>

  <assign|acronym|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  <assign|person|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  <assign|op|<macro|body|<with|math-condensed|true|<syntax|<arg|body>|x>>>>

  <drd-props|op|arity|1|syntax|<macro|body|x>>

  <\active*>
    <\src-comment>
      Common language tags.
    </src-comment>
  </active*>

  <assign|british|<macro|body|<with|language|british|<arg|body>>>>

  <assign|bulgarian|<macro|body|<with|language|bulgarian|<arg|body>>>>

  <assign|chinese|<macro|body|<with|language|chinese|<arg|body>>>>

  <assign|czech|<macro|body|<with|language|czech|<arg|body>>>>

  <assign|danish|<macro|body|<with|language|danish|<arg|body>>>>

  <assign|dutch|<macro|body|<with|language|dutch|<arg|body>>>>

  <assign|english|<macro|body|<with|language|english|<arg|body>>>>

  <assign|finnish|<macro|body|<with|language|finnish|<arg|body>>>>

  <assign|french|<macro|body|<with|language|french|<arg|body>>>>

  <assign|german|<macro|body|<with|language|german|<arg|body>>>>

  <assign|hungarian|<macro|body|<with|language|hungarian|<arg|body>>>>

  <assign|italian|<macro|body|<with|language|italian|<arg|body>>>>

  <assign|japanese|<macro|body|<with|language|japanese|<arg|body>>>>

  <assign|korean|<macro|body|<with|language|korean|<arg|body>>>>

  <assign|polish|<macro|body|<with|language|polish|<arg|body>>>>

  <assign|portuguese|<macro|body|<with|language|portuguese|<arg|body>>>>

  <assign|romanian|<macro|body|<with|language|romanian|<arg|body>>>>

  <assign|russian|<macro|body|<with|language|russian|<arg|body>>>>

  <assign|slovene|<macro|body|<with|language|slovene|<arg|body>>>>

  <assign|spanish|<macro|body|<with|language|spanish|<arg|body>>>>

  <assign|swedish|<macro|body|<with|language|swedish|<arg|body>>>>

  <assign|taiwanese|<macro|body|<with|language|taiwanese|<arg|body>>>>

  <assign|ukrainian|<macro|body|<with|language|ukrainian|<arg|body>>>>

  <\active*>
    <\src-comment>
      Below follow some frequently used content environments.
    </src-comment>
  </active*>

  <assign|padded|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <arg|body>
    </padded-normal>
  </macro>>

  <assign|underlined|<\macro|body>
    <padded|<wide-std-underlined|<arg|body>>>
  </macro>>

  <assign|bothlined|<\macro|body>
    <padded|<wide-std-bothlined|<arg|body>>>
  </macro>>

  <assign|framed|<\macro|body>
    <padded|<wide-std-framed|<arg|body>>>
  </macro>>

  <assign|center|<macro|body|<with|par-mode|center|<arg|body>>>>

  <assign|quote-env|<\macro|body>
    <\padded>
      <\indent-both|3fn|3fn>
        <with|par-first|0fn|par-par-sep|0.25fn|<arg|body>>
      </indent-both>
    </padded>
  </macro>>

  <assign|quotation|<\macro|body>
    <\padded>
      <\indent-both|3fn|3fn>
        <surround|<yes-indent>||<arg|body>>
      </indent-both>
    </padded>
  </macro>>

  <assign|verse|<\macro|body>
    <\padded>
      <\indent-both|4.5fn|3fn>
        <with|par-first|-1.5fn|par-par-sep|0.fn|<surround|<yes-indent>||<arg|body>>>
      </indent-both>
    </padded>
  </macro>>

  <assign|verbatim|<macro|body|<with|font-family|tt|language|verbatim|<arg|body>>>>

  <assign|code|<\macro|body>
    <\padded-normal|1fn|1fn>
      <surround||<htab|5mm>|<with|font-family|tt|language|verbatim|par-first|0fn|<arg|body>>>
    </padded-normal>
  </macro>>

  <\active*>
    <\src-comment>
      The following environments complete the most basic tabular
      environments.
    </src-comment>
  </active*>

  <assign|tabular*|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|body>>>>

  <assign|block|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|body>>>>

  <assign|block*|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|c>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Other presentation tags.
    </src-comment>
  </active*>

  <assign|with-opacity|<macro|fact|body|<with|opacity|<times|<arg|fact>|<value|opacity>>|<arg|body>>>>

  <assign|pastel|<macro|body|<with-opacity|0.1|<arg|body>>>>

  <assign|greyed|<macro|body|<with-opacity|0.33333|<arg|body>>>>

  <assign|light|<macro|body|<with-opacity|0.5|<arg|body>>>>

  \;

  <assign|hresize|<macro|body|left|right|<resize|<arg|body>|<arg|left>||<arg|right>|>>>

  <assign|hextend|<macro|body|right|<resize|<arg|body>|||<maximum|1r|<arg|right>>|>>>

  <assign|vresize|<macro|body|bottom|top|<resize|<arg|body>||<arg|bottom>||<arg|top>>>>

  <assign|bcorrect|<macro|body|<resize|<arg|body>||<minimum|1b|1fnbot>||>>>

  <assign|tcorrect|<macro|body|<resize|<arg|body>||||<maximum|1t|1fntop>>>>

  <assign|vcorrect|<macro|body|<resize|<arg|body>||<minimum|1b|1fnbot>||<maximum|1t|1fntop>>>>

  <assign|smash|<macro|body|<vresize|<arg|body>|0ex|1ex>>>

  <assign|smash-top|<macro|body|<vresize|<arg|body>||1ex>>>

  <assign|smash-bottom|<macro|body|<vresize|<arg|body>|0ex|>>>

  <assign|phantom|<macro|body|<if*|false|<arg|body>>>>

  <drd-props|phantom|arity|1|accessible|none|syntax|<macro|body|>>

  <assign|hphantom|<macro|body|<vresize|<phantom|<arg|body>>|0ex|1ex>>>

  <assign|vphantom|<macro|body|<hresize|<phantom|<arg|body>>|0em|0em>>>

  <assign|mini-paragraph|<macro|width|body|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<twith|table-width|<arg|width>>|<twith|table-hmode|exact>|<twith|table-valign|T>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <drd-props|mini-paragraph|arity|2|length|0|accessible|1>

  \;

  <assign|frame|<macro|body|<block|<tformat|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|colored-frame|<macro|col|body|<block|<tformat|<cwith|1|1|1|1|cell-background|<arg|col>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|overline|<macro|body|<eval|<quasiquote|<style-with|src-compact|none|<datoms|<macro|body|<with|color|<unquote|<value|color>>|<wide|<arg|body>|\<wide-bar\>>>>|<arg|body>>>>>>>

  <drd-props|overline|with-like|yes|arity|1|accessible|all>

  <assign|underline|<macro|body|<eval|<quasiquote|<style-with|src-compact|none|<datoms|<macro|body|<with|color|<unquote|<value|color>>|<wide*|<arg|body>|\<wide-bar\>>>>|<arg|body>>>>>>>

  <drd-props|underline|with-like|yes|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Tags for HTML generation.
    </src-comment>
  </active*>

  <assign|html-div|<macro|name|body|<arg|body>>>

  <assign|html-style|<macro|style|body|<arg|body>>>

  <assign|html-javascript|<macro|script|<small|<colored-frame|pastel
  yellow|<with|font-family|ss|Javascript:
  ><with|font-family|tt|<arg|script>>>>>>

  <assign|html-javascript-src|<macro|source-file|<small|<colored-frame|pastel
  yellow|<with|font-family|ss|Javascript source:
  ><href|<arg|source-file>>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|href|<macro|body|<hlink|<with|font-family|tt|<arg|body>>|<arg|body>>>>

  <assign|slink|<macro|body|<hlink|<with|font-family|tt|<arg|body>>|<arg|body>>>>

  <assign|square|<macro|x|<times|<arg|x>|<arg|x>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>