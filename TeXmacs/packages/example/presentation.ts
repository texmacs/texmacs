<TeXmacs|1.0.7.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      Presentation style.
    </src-purpose>

    <src-copyright|2007--2008|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|varsession|alt-colors>

  <\active*>
    <\src-comment>
      Customizations
    </src-comment>
  </active*>

  <assign|font-family|ss>

  <assign|name|<macro|x|<with|font-family|rm|font-shape|small-caps|<arg|x>>>>

  <assign|bg-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#f4eee8>>

  <assign|monochrome-bg-color|#f4eee8>

  <assign|ornament-border|2ln>

  <assign|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|ornament-sunny-color|#f0e0c0>

  <assign|ornament-shadow-color|#c07055>

  <assign|ornament-hpadding|1spc>

  <assign|ornament-vpadding|1spc>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  <assign|tit|<macro|body|<with|ornament-color|<pattern|paper-manila-medium.png|*3/5|*3/5|#e0e0e0>|<ornament|<with|font-series|bold|color|dark
  brown|<postscript|$TEXMACS_PATH/misc/images/right-head.png|/4.5|/4.5||||>><htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|dark
  brown|<arg|body>>>>|0fn|0.333fn><htab|5mm><postscript|$TEXMACS_PATH/misc/images/left-head.png|/4.5|/4.5||||>>>>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-2.5fn|<yes-indent>><resize|<arg|x>|r-2.5fn||r+0.0fn|>>>>

  <assign|render-bibitem|<macro|text|<aligned-item|<transform-bibitem|<arg|text>>>>>

  <assign|strong-color|#504000>

  <assign|greyed-math-color|#c08080>

  <assign|greyed|<macro|x|<with|color|#c08080|math-color|<value|greyed-math-color>|<arg|x>>>>

  <assign|granite|<macro|x|<with|ornament-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>|color|white|strong-color|#f0ffb0|math-color|#ffd4c0|ornament-sunny-color|light
  grey|ornament-shadow-color|dark grey|<ornament|<arg|x>>>>>

  <assign|pine|<macro|x|<with|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>|strong-color|#0c3000|math-color|#500000|<ornament|<arg|x>>>>>

  <assign|ridged|<macro|x|<with|ornament-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>|<ornament|<arg|x>>>>>

  <assign|input-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>>

  <assign|fold-bar-color|<pattern|wood-light.png|*3/5|*3/5|#e0b050>>

  <assign|fold-title-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|folded-body|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>