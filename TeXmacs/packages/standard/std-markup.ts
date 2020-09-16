<TeXmacs|1.99.13>

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

  <assign|tm-made|<macro|<with|ornament-color||ornament-shadow-color|<value|color>|ornament-sunny-color|<value|color>|ornament-border|1ln|ornament-shape|rounded|ornament-hpadding|0.5spc|ornament-vpadding|0.5spc|<ornament|<math|\<Backsigma\>>>>>>

  <assign|TeX|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|LaTeX|<macro|<active*|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|LaTeXe|<macro|<active*|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<compound|math|>><space|0.2spc>2<move|<math|\<varepsilon\>>|-0.0fn|-0.15fn>>>

  <assign|LaTeX*|<macro|<active*|(L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>)<space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|BibTeX|<macro|<active*|<with|font-shape|small-caps|Bib>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|old-hrule|<macro|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|-1|cell-height|1ln>|<cwith|1|-1|1|-1|cell-lsep|0fn>|<cwith|1|-1|1|-1|cell-rsep|0fn>|<cwith|1|-1|1|-1|cell-bsep|0fn>|<cwith|1|-1|1|-1|cell-tsep|0fn>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>>

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
      Frequently used content tags.
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

  <assign|croatian|<macro|body|<with|language|croatian|<arg|body>>>>

  <assign|czech|<macro|body|<with|language|czech|<arg|body>>>>

  <assign|danish|<macro|body|<with|language|danish|<arg|body>>>>

  <assign|dutch|<macro|body|<with|language|dutch|<arg|body>>>>

  <assign|english|<macro|body|<with|language|english|<arg|body>>>>

  <assign|esperanto|<macro|body|<with|language|esperanto|<arg|body>>>>

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
      Frequently used content environments.
    </src-comment>
  </active*>

  <assign|tab-length|<macro|1.5fn>>

  <assign|quote-left-indentation|2tab>

  <assign|quote-right-indentation|2tab>

  <assign|quote-interparagraph|0.25fn>

  <assign|verse-hangover|1tab>

  <assign|jump-in-hangover|1tab>

  \;

  <assign|quote-env|<\macro|body>
    <\padded>
      <\indent-both|<value|quote-left-indentation>|<value|quote-right-indentation>>
        <with|par-first|0fn|par-par-sep|<value|quote-interparagraph>|<arg|body>>
      </indent-both>
    </padded>
  </macro>>

  <assign|quotation|<\macro|body>
    <\padded>
      <\indent-both|<value|quote-left-indentation>|<value|quote-right-indentation>>
        <surround|<yes-indent>||<arg|body>>
      </indent-both>
    </padded>
  </macro>>

  <assign|verse|<\macro|body>
    <\padded>
      <\indent-both|<plus|<value|quote-left-indentation>|<value|verse-hangover>>|<value|quote-right-indentation>>
        <with|par-first|<minus|<value|verse-hangover>>|par-par-sep|0.fn|<surround|<yes-indent>||<arg|body>>>
      </indent-both>
    </padded>
  </macro>>

  <assign|jump-in|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-left|<plus|<value|par-left>|<value|jump-in-hangover>>|par-first|<minus|<value|jump-in-hangover>>>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|ultra-compact|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-sep|0fn|par-ver-sep|0fn|par-line-sep|0fn|par-par-sep|0fn>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|compact|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-par-sep|0fn>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|compressed|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-par-sep|<times|0.5|<value|par-par-sep>>>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|amplified|<\macro|body>
    <\surround||<right-flush>>
      <\with|par-par-sep|<times|1.5|<value|par-par-sep>>>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|vgroup|<\macro|body>
    <\surround|<no-break-start>|<no-break-end><right-flush>>
      <\with|par-min-penalty|100000000>
        <arg|body>
      </with>
    </surround>
  </macro>>

  <assign|indivisible|<value|vgroup>>

  \;

  <assign|center|<macro|body|<with|par-mode|center|<arg|body>>>>

  <assign|left-aligned|<macro|body|<with|par-mode|left|<arg|body>>>>

  <assign|right-aligned|<macro|body|<with|par-mode|right|<arg|body>>>>

  \;

  <assign|verbatim|<macro|body|<with|font-family|tt|language|verbatim|<arg|body>>>>

  <assign|code|<\macro|body>
    <\padded*>
      <surround||<htab|5mm>|<with|font-family|tt|language|verbatim|par-first|0fn|par-par-sep|0fn|<arg|body>>>
    </padded*>
  </macro>>

  <\active*>
    <\src-comment>
      Basic tabular environments.
    </src-comment>
  </active*>

  <assign|tabular*|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|body>>>>

  <assign|block|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|body>>>>

  <assign|block*|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|c>|<arg|body>>>>

  <assign|wide-tabular|<macro|body|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<twith|table-block|yes>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-hpart|0.001>|<cwith|1|-1|1|1|cell-lsep|0fn>|<cwith|1|-1|-1|-1|cell-rsep|0fn>|<arg|body>>>>

  <assign|wide-block|<macro|body|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<twith|table-block|yes>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-hpart|0.001>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|body>>>>

  <assign|par-block|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Transparency.
    </src-comment>
  </active*>

  <assign|with-opacity|<macro|factor|body|<with|opacity|<times|<arg|factor>|<value|opacity>>|<arg|body>>>>

  <assign|pastel|<macro|body|<with-opacity|0.1|<arg|body>>>>

  <assign|greyed|<macro|body|<with-opacity|0.33333|<arg|body>>>>

  <assign|light|<macro|body|<with-opacity|0.5|<arg|body>>>>

  \;

  <assign|phantom|<macro|body|<if*|false|<arg|body>>>>

  <drd-props|phantom|arity|1|accessible|none|syntax|<macro|body|>>

  <assign|hphantom|<macro|body|<vresize|<phantom|<arg|body>>|0ex|1ex>>>

  <assign|vphantom|<macro|body|<hresize|<phantom|<arg|body>>|0em|0em>>>

  <assign|concealed|<macro|body|>>

  <\active*>
    <\src-comment>
      Adjusting the sizes of boxes.
    </src-comment>
  </active*>

  <assign|hresize|<macro|body|left|right|<resize|<arg|body>|<arg|left>||<arg|right>|>>>

  <assign|vresize|<macro|body|bottom|top|<resize|<arg|body>||<arg|bottom>||<arg|top>>>>

  <assign|smash|<macro|body|<vresize|<arg|body>|0ex|1ex>>>

  <assign|smash-top|<macro|body|<vresize|<arg|body>|1b|1ex>>>

  <assign|smash-bottom|<macro|body|<vresize|<arg|body>|0ex|1t>>>

  <assign|reduce-by|<macro|body|by|<vresize|<arg|body>|<plus|1b|<arg|by>>|<minus|1t|<arg|by>>>>>

  <assign|reduce-top-by|<macro|body|by|<vresize|<arg|body>||<minus|1t|<arg|by>>>>>

  <assign|reduce-bottom-by|<macro|body|by|<vresize|<arg|body>|<plus|1b|<arg|by>>|>>>

  \;

  <assign|extend|<macro|body|left|bottom|right|top|<style-with|src-compact|none|<resize|<arg|body>|<if|<equal|<arg|left>|>|1l|<minimum|1l|<arg|left>>>|<if|<equal|<arg|bottom>|>|1b|<minimum|1b|<arg|bottom>>>|<if|<equal|<arg|right>|>|1r|<maximum|1r|<arg|right>>>|<if|<equal|<arg|top>|>|1t|<maximum|1t|<arg|top>>>>>>>

  <drd-props|extend|arity|5|accessible|0|length|1|length|2|length|3|length|4>

  <assign|extend-right|<macro|body|right|<resize|<arg|body>|||<maximum|1r|<arg|right>>|>>>

  <assign|inflate|<macro|body|<resize|<arg|body>||<minimum|1b|1fnbot>||<maximum|1t|1fntop>>>>

  <assign|inflate-bottom|<macro|body|<resize|<arg|body>||<minimum|1b|1fnbot>||>>>

  <assign|inflate-top|<macro|body|<resize|<arg|body>||||<maximum|1t|1fntop>>>>

  \;

  <assign|mini-paragraph|<macro|width|body|<tabular|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<twith|table-width|<arg|width>>|<twith|table-hmode|exact>|<twith|table-valign|T>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <drd-props|mini-paragraph|arity|2|length|0|accessible|1>

  <\active*>
    <\src-comment>
      Frames.
    </src-comment>
  </active*>

  <assign|frame|<macro|body|<block|<tformat|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|colored-frame|<macro|col|body|<block|<tformat|<cwith|1|1|1|1|cell-background|<arg|col>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|tmfs-title|<\macro|name>
    <surround||<vspace|1fn>|<block*|<tformat|<cwith|1|1|1|1|cell-background|pastel
    blue>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|2spc>|<cwith|1|1|1|1|cell-rsep|2spc>|<cwith|1|1|1|1|cell-bsep|2spc>|<cwith|1|1|1|1|cell-tsep|2spc>|<table|<row|<cell|<large|<strong|<arg|name>>>>>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Decorated text.
    </src-comment>
  </active*>

  <assign|overline|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|color>>|<wide|<arg|x>|\<wide-bar\>>>>|<arg|body>>>>>>

  <drd-props|overline|with-like|yes|arity|1|accessible|all>

  <assign|underline|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|color>>|<wide*|<arg|x>|\<wide-bar\>>>>|<arg|body>>>>>>

  <drd-props|underline|with-like|yes|arity|1|accessible|all>

  <assign|stressed-color|black>

  <assign|stressed-distance|0.2fn>

  <assign|stressed|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|stressed-color>>|<repeat*|<with|color|<unquote|<value|color>>|<arg|x>>|<move|<resize|-|<plus|0.6667l|0.3333r>||<plus|0.3333l|0.6667r>|>||<minus|-0.5ex|<unquote|<value|stressed-distance>>>>>>>|<arg|body>>>>>>

  <drd-props|stressed|with-like|yes|arity|1|accessible|all>

  <assign|strike-through|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|color>>|<repeat|<arg|x>|<resize|-|<plus|0.6667l|0.3333r>||<plus|0.3333l|0.6667r>|>>>>|<arg|body>>>>>>

  <drd-props|strike-through|with-like|yes|arity|1|accessible|all>

  <assign|hrule|<macro|<strike-through|<htab|5mm>>>>

  <assign|repeat-through|<macro|what|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<repeat|<arg|x>|<unquote|<arg|what>>>>|<arg|body>>>>>>

  <drd-props|repeat-through|arity|2|accessible|1>

  \;

  <assign|deleted-color|red>

  <assign|deleted|<macro|body|<repeat-through|<with|color|<value|deleted-color>|/>|<arg|body>>>>

  <drd-props|deleted|with-like|yes|arity|1|accessible|all>

  \;

  <assign|fill-out-color|black>

  <assign|fill-out-distance|0.2fn>

  <assign|fill-out-hspace|0fn>

  <assign|fill-out|<macro|body|<repeat-through|<with|color|<value|fill-out-color>|<move|.||<minus|<value|fill-out-distance>>><hspace|<value|fill-out-hspace>>>|<arg|body>>>>

  <assign|fill-out*|<macro|body|<fill-out|<arg|body><htab|5mm>>>>

  <assign|fill-dots|<macro|<fill-out|<htab|5mm>>>>

  <drd-props|fill-out|with-like|yes|arity|1|accessible|all>

  \;

  <assign|marked-color|yellow>

  <assign|marked-padding|0.2fn>

  <assign|marked|<macro|body|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<resize|<tabular|<tformat|<cwith|1|1|1|1|cell-background|<unquote|<value|marked-color>>>|<cwith|1|1|1|1|cell-lsep|<unquote|<value|marked-padding>>>|<cwith|1|1|1|1|cell-rsep|<unquote|<value|marked-padding>>>|<cwith|1|1|1|1|cell-bsep|<unquote|<value|marked-padding>>>|<cwith|1|1|1|1|cell-tsep|<unquote|<value|marked-padding>>>|<table|<row|<cell|<arg|x>>>>>>|<plus|1l|<unquote|<value|marked-padding>>>|<plus|1b|<unquote|<value|marked-padding>>>|<minus|1r|<unquote|<value|marked-padding>>>|<minus|1t|<unquote|<value|marked-padding>>>>>|<arg|body>>>>>>

  <drd-props|marked|with-like|yes|arity|1|accessible|all>

  \;

  <assign|todo-color|dark red>

  <assign|todo-bg-color|pastel red>

  <assign|render-todo|<macro|fg|bg|body|<with|color|<arg|fg>|<quasi|<style-with|src-compact|none|<datoms|<macro|x|<resize|<tabular|<tformat|<cwith|1|1|1|1|cell-background|<unquote|<arg|bg>>>|<cwith|1|1|1|1|cell-lsep|0fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-bsep|<unquote|<value|marked-padding>>>|<cwith|1|1|1|1|cell-tsep|<unquote|<value|marked-padding>>>|<table|<row|<cell|<arg|x>>>>>>|<plus|1l|0fn>|<plus|1b|<unquote|<value|marked-padding>>>|<minus|1r|0fn>|<minus|1t|<unquote|<value|marked-padding>>>>>|[<arg|body>]>>>>>>

  <assign|todo|<macro|body|<render-todo|<value|todo-color>|<value|todo-bg-color>|<arg|body>>>>

  <assign|todo*|<macro|body|<render-todo|dark orange|pastel
  orange|<arg|body>>>>

  <drd-props|render-todo|arity|3|color|0|color|1|accessible|2>

  <\active*>
    <\src-comment>
      Marginal notes.
    </src-comment>
  </active*>

  <assign|marginal-note-width|2cm>

  <assign|marginal-note-sep|5mm>

  <assign|marginal-note-table|<macro|body|hpos|hhyph|vpos|vhyph|<tabular|<tformat|<twith|table-width|<value|marginal-note-width>>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|<arg|vhyph>>|<twith|table-valign|<arg|vpos>>|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-halign|<arg|hpos>>|<table|<row|<\cell>
    <\with|par-par-sep|0em|par-line-sep|0em|par-mode|<arg|hhyph>>
      <arg|body>
    </with>
  </cell>>>>>>>

  <assign|marginal-even-left-note|<macro|vpos|body|<line-note|<specific|even|<marginal-note-table|<arg|body>|r|right|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>>|<minus|0cm|<plus|<value|marginal-note-width>|<value|marginal-note-sep>>>|0cm><line-note|<specific|odd|<marginal-note-table|<arg|body>|l|left|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>>|<plus|1par|<value|marginal-note-sep>|<value|par-left>|<value|par-right>>|0cm><flag|marginal
  note|dark brown>>>

  <assign|marginal-even-right-note|<macro|vpos|body|<line-note|<specific|odd|<marginal-note-table|<arg|body>|r|right|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>>|<minus|0cm|<plus|<value|marginal-note-width>|<value|marginal-note-sep>>>|0cm><line-note|<specific|even|<marginal-note-table|<arg|body>|l|left|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>>|<plus|1par|<value|marginal-note-sep>|<value|par-left>|<value|par-right>>|0cm><flag|marginal
  note|dark brown>>>

  <assign|marginal-left-note|<macro|vpos|body|<line-note|<marginal-note-table|<arg|body>|r|right|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>|<minus|0cm|<plus|<value|marginal-note-width>|<value|marginal-note-sep>>>|0cm><flag|marginal
  note|dark brown>>>

  <assign|marginal-right-note|<macro|vpos|body|<line-note|<marginal-note-table|<arg|body>|l|left|<change-case|<arg|vpos>|UPCASE>|<arg|vpos>>|<plus|1par|<value|marginal-note-sep>|<value|par-left>|<value|par-right>>|0cm><flag|marginal
  note|dark brown>>>

  <assign|marginal-normal-note|<macro|vpos|body|<compound|<if|<and|<equal|<value|page-odd>|<value|page-even>>|<equal|<value|page-odd-shift>|<value|page-even-shift>>>|marginal-left-note|marginal-even-left-note>|<arg|vpos>|<arg|body>>>>

  <assign|marginal-note|<macro|hpos|vpos|body|<with|dummy1|<value|marginal-note-width>|dummy2|<value|marginal-note-sep>|<compound|<merge|marginal-|<arg|hpos>|-note>|<arg|vpos>|<arg|body>|<arg|hpos>>>>>

  <drd-props|marginal-note|arity|3|accessible|none>

  <\active*>
    <\src-comment>
      Tags for HTML generation.
    </src-comment>
  </active*>

  <assign|web-title|<macro|title|>>

  <assign|html-tag|<macro|name|body|<arg|body>>>

  <assign|html-attr|<macro|attr|val|body|<arg|body>>>

  <assign|html-div-style|<macro|name|body|<arg|body>>>

  <assign|html-div-class|<macro|name|body|<arg|body>>>

  <assign|html-style|<macro|style|body|<arg|body>>>

  <assign|html-class|<macro|style|body|<arg|body>>>

  <assign|html-javascript|<macro|script|<small|<colored-frame|pastel
  yellow|<with|font-family|ss|Javascript:
  ><with|font-family|tt|<arg|script>>>>>>

  <assign|html-javascript-src|<macro|source-file|<small|<colored-frame|pastel
  yellow|<with|font-family|ss|Javascript source:
  ><href|<arg|source-file>>>>>>

  <assign|html-video|<macro|dest|width|height|<small|<colored-frame|pastel
  yellow|<tabular*|<tformat|<table|<row|<cell|<small|<with|font-family|tt|<arg|dest>>>>>|<row|<cell|<arg|width>
  <math|\<times\>> <arg|height>>>>>>>>>>

  <\active*>
    <\src-comment>
      Poor man font effects.
    </src-comment>
  </active*>

  <assign|add-font-effect|<macro|prop|val|body|<with|font-effects|<merge|<value|font-effects>|<if|<equal|<value|font-effects>|>||,>|<arg|prop>|=|<arg|val>>|<arg|body>>>>

  \;

  <assign|embold-strength|2>

  <assign|embbb-strength|3>

  <assign|slanted-slope|0.25>

  <assign|hmagnified-factor|1.5>

  <assign|vmagnified-factor|1.5>

  <assign|condensed-factor|0.8>

  <assign|extended-factor|1.2>

  <assign|monospaced-factor|0.75>

  <assign|degraded-threshold|0.667>

  <assign|degraded-frequency|1.0>

  <assign|distorted-strength|0.5>

  <assign|distorted-frequency|1.0>

  <assign|gnawed-strength|0.6>

  <assign|gnawed-frequency|1.0>

  \;

  <assign|embold|<macro|body|<add-font-effect|bold|<value|embold-strength>|<arg|body>>>>

  <assign|embbb|<macro|body|<add-font-effect|bbb|<value|embbb-strength>|<arg|body>>>>

  <assign|slanted|<macro|body|<add-font-effect|slant|<value|slanted-slope>|<arg|body>>>>

  <assign|hmagnified|<macro|body|<add-font-effect|hmagnify|<value|hmagnified-factor>|<arg|body>>>>

  <assign|vmagnified|<macro|body|<add-font-effect|vmagnify|<value|vmagnified-factor>|<arg|body>>>>

  <assign|condensed|<macro|body|<add-font-effect|hextended|<value|condensed-factor>|<arg|body>>>>

  <assign|extended|<macro|body|<add-font-effect|hextended|<value|extended-factor>|<arg|body>>>>

  <assign|monospaced|<macro|body|<add-font-effect|mono|<value|monospaced-factor>|<arg|body>>>>

  <assign|degraded|<macro|body|<add-font-effect|degraded|<merge|<value|degraded-threshold>|;|<value|degraded-frequency>>|<arg|body>>>>

  <assign|distorted|<macro|body|<add-font-effect|distorted|<merge|<value|distorted-strength>|;|<value|distorted-frequency>>|<arg|body>>>>

  <assign|gnawed|<macro|body|<add-font-effect|gnawed|<merge|<value|gnawed-strength>|;|<value|gnawed-frequency>>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Artistic font effects.
    </src-comment>
  </active*>

  <assign|blurred-radius|0.6pt>

  <assign|shadow-font-color|#222>

  <assign|sunny-font-color|#ddd>

  \;

  <assign|blurred|<macro|body|<add-font-effect|blurred|<value|blurred-radius>|<arg|body>>>>

  <assign|enhanced|<macro|body|<add-font-effect|enhanced|<merge|<value|blurred-radius>|;|<value|shadow-font-color>|;|<value|sunny-font-color>>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|href|<macro|body|<hlink|<with|font-family|tt|language|verbatim|<arg|body>>|<arg|body>>>>

  <assign|slink|<macro|body|<hlink|<with|font-family|tt|language|verbatim|<arg|body>>|<arg|body>>>>

  <assign|square|<macro|x|<times|<arg|x>|<arg|x>>>>

  <assign|text-dots-sep|0.3333spc>

  <assign|text-spc|<macro|<hspace|<value|text-dots-sep>>>>

  <assign|text-dots|<macro|<text-spc>.<text-spc>.<text-spc>.<text-spc>>>

  <assign|indent-par-first|1.5fn>

  <assign|padded-par-par-sep|0.6666fn>

  <assign|math-color|<value|color>>

  <assign|strong-color|<value|color>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|src-special|normal>
  </collection>
</initial>