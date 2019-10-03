<TeXmacs|1.99.11>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      Presentation style.
    </src-purpose>

    <src-copyright|2007--2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|alt-colors|ornaments|framed-session>

  <use-module|(dynamic fold-markup)>

  <\active*>
    <\src-comment>
      External scheme routines
    </src-comment>
  </active*>

  <assign|screens-index|<macro|body|<extern|screens-index|<quote-arg|body>>>>

  <assign|screens-arity|<macro|body|<extern|screens-arity|<quote-arg|body>>>>

  <assign|screens-summary|<macro|body|<if|<greater|<screens-arity|<quote-arg|body>>|0>|<move|<tiny|<plus|<screens-index|<quote-arg|body>>|1>/<screens-arity|<quote-arg|body>>>|0em|0.25ex>>>>

  <assign|screens-bar|<macro|body|<extern|screens-bar|<quote-arg|body>>>>

  <assign|screens-emphasize|<macro|body|<wide*|<arg|body>|\<wide-bar\>>>>

  <\active*>
    <\src-comment>
      Global document layout
    </src-comment>
  </active*>

  <assign|page-type|4:3>

  <assign|page-medium|beamer>

  <assign|page-orientation|landscape>

  <assign|page-odd|5mm>

  <assign|page-even|5mm>

  <assign|page-right|5mm>

  <assign|page-top|5mm>

  <assign|page-bot|5mm>

  <assign|page-screen-margin|false>

  <assign|page-screen-left|5mm>

  <assign|page-screen-right|5mm>

  <assign|page-screen-top|5mm>

  <assign|page-screen-bot|5mm>

  <assign|magnification|1.7>

  <assign|font-family|ss>

  <assign|name|<macro|body|<with|font-family|rm|font-shape|small-caps|<arg|body>>>>

  <assign|item-vsep|<macro|0fn>>

  <assign|page-odd-footer|>

  <assign|page-even-footer|>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|title-theme|title-bar>

  <assign|title-vpadding|1spc>

  <assign|title-font-size|1.414>

  <assign|title-old-style|false>

  <assign|title-bar-color|<macro|dark blue>>

  <assign|title-color|<macro|white>>

  <assign|title-border|0ln>

  <assign|title-sunny-color|#f4f4ff>

  <assign|title-shadow-color|#d8d8ff>

  <assign|title-left|<macro|body|>>

  <assign|title-right|<macro|body|>>

  <assign|title-left|<macro|body|<phantom|<screens-summary|<quote-arg|body>>>>>

  <assign|title-right|<macro|body|<screens-summary|<quote-arg|body>>>>

  <assign|title-|>

  <drd-props|title-vpadding|macro-parameter|length>

  <drd-props|title-font-size|macro-parameter|font-size>

  <drd-props|title-old-style|macro-parameter|boolean>

  <drd-props|title-bar-color|macro-parameter|color>

  <drd-props|title-color|macro-parameter|color>

  <assign|with-tit-color|<macro|body|<\with|color|<title-color>|math-color|<title-color>|strong-color|<title-color>>
    <arg|body>
  </with>>>

  <assign|tit|<macro|body|<with-tit-color|<with|ornament-color|<title-bar-color>|<ornament|<title-left|<arg|body>><htab|5mm><with|font-series|bold|math-font-series|bold|<large|<space|0em|-0.6ex|1.6ex><arg|body>>><htab|5mm><title-right|<arg|body>>>>>>>

  <assign|tit|<\macro|body>
    <\surround|<assign|page-this-top|0mm>|<assign|gpag-length|<macro|<minus|1pag|2.25fn>>><vspace|5mm>>
      <\with|par-left|<minus|<value|page-odd>>|par-right|<minus|<value|page-right>>|color|<value|title-shadow-color>>
        <with|<resize|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|-1|cell-background|<title-bar-color>>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-hyphen|t>|<twith|table-valign|T>|<cwith|1|1|1|1|cell-tborder|<value|title-border>>|<cwith|1|1|1|1|cell-bborder|<value|title-border>>|<cwith|1|1|1|1|cell-lborder|<value|title-border>>|<cwith|1|1|1|1|cell-rborder|<value|title-border>>|<table|<row|<\cell>
          <with-tit-color|<title-left|<arg|body>><htab|5mm><arg|body><htab|5mm><title-right|<arg|body>>>
        </cell>>>>>||0em||>>
      </with>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Customized screens
    </src-comment>
  </active*>

  <assign|with-screen-color|<\macro|col|body>
    <surround|<assign|page-this-bg-color|<arg|col>>||<arg|body>>
  </macro>>

  <drd-props|with-screen-color|arity|2|hidden|0|accessible|1|border|no>

  <\active*>
    <\src-comment>
      Customized session elements
    </src-comment>
  </active*>

  <assign|session|<\macro|language|session|body>
    <\with|prog-language|<arg|language>|prog-session|<arg|session>>
      <\small>
        <render-session|<arg|body>>
      </small>
    </with>
  </macro>>

  <assign|folded-body|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous
    </src-comment>
  </active*>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  <assign|TeXmacs*|<macro|<active*|<anim-repeat|<anim-compose|<anim-constant|<with|color|brown|T<space|-0.2spc><with|color|brown|<rsub|<with|math-level|0|font-shape|small-caps|<smash|E>>>><space|-0.1spc>X<space|-0.2spc>><with|color|dark
  green|<rsub|<with|math-level|0|font-shape|small-caps|ma<space|-0.2spc>cs>>>|1sec>|<anim-constant|<with|color|dark
  green|T<space|-0.2spc><with|color|brown|<rsub|<with|math-level|0|font-shape|small-caps|<smash|E>>>><space|-0.1spc>X<space|-0.2spc>><with|color|brown|<rsub|<with|math-level|0|font-shape|small-caps|ma<space|-0.2spc>cs>>>|1sec>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>