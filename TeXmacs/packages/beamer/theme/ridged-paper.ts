<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|ridged-paper|1.0|ridged-paper|1.0>

    <\src-purpose>
      Ridged paper motif for presentation style.
    </src-purpose>

    <src-copyright|2007--2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Background
    </src-comment>
  </active*>

  <assign|bg-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#f4eee8>>

  <assign|monochrome-bg-color|#f4eee8>

  <\active*>
    <\src-comment>
      Standard ornaments
    </src-comment>
  </active*>

  <assign|ornament-border|2ln>

  <assign|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|ornament-extra-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|ornament-sunny-color|#f0e0c0>

  <assign|ornament-shadow-color|#c07055>

  <assign|ornament-hpadding|1spc>

  <assign|ornament-vpadding|1spc>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|left-gnu|<macro|<move|<image|$TEXMACS_PATH/misc/images/left-head.png|0.222222w|0.222222h||>|0fn|-0.333fn>>>

  <assign|right-gnu|<macro|<move|<image|$TEXMACS_PATH/misc/images/right-head.png|0.222222w|0.222222h||>|0fn|-0.333fn>>>

  <assign|title-bar-color|<macro|<if|<equal|<value|title-theme>|title-bar>|<pattern|wood-medium.png|*3/5|*3/5|#804018>|<pattern|paper-manila-medium.png|*3/5|*3/5|#f4f0d8>>>>

  <assign|title-color|<macro|<if|<equal|<value|title-theme>|title-bar>|<value|bg-color>|dark
  brown>>>

  <assign|title-left|<macro|body|<if|<equal|<value|title-theme>|title-bar>||<right-gnu>>>>

  <assign|title-right|<macro|body|<if|<equal|<value|title-theme>|title-bar>||<left-gnu>>>>

  <\active*>
    <\src-comment>
      Colors of standard tags (strong, math, etc.)
    </src-comment>
  </active*>

  <assign|strong-color|#504000>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <assign|input-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>>

  <assign|fold-bar-color|<pattern|wood-light.png|*3/5|*3/5|#e0b050>>

  <assign|fold-title-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>