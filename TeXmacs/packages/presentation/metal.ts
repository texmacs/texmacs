<TeXmacs|1.0.7.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|metal|1.0|metal|1.0>

    <\src-purpose>
      Metal motif for presentation style.
    </src-purpose>

    <src-copyright|2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|alt-colors|ornaments|varsession|presentation>

  <\active*>
    <\src-comment>
      Background
    </src-comment>
  </active*>

  <assign|bg-color|<pattern|metal-brushed-light.png|*3/5|*3/5|#f0f0f0>>

  <assign|monochrome-bg-color|#f0f0f0>

  <\active*>
    <\src-comment>
      Standard ornaments
    </src-comment>
  </active*>

  <assign|ornament-border|2ln>

  <assign|ornament-color|<pattern|metal-rough-medium.png|*3/5|*3/5|#d0d0d0>>

  <assign|ornament-sunny-color|#e0e0e0>

  <assign|ornament-shadow-color|#a0a0a0>

  <assign|ornament-hpadding|1spc>

  <assign|ornament-vpadding|1spc>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|tit|<macro|body|<with|ornament-color|<pattern|granite-medium.png|*3/5|*3/5|#404040>|<ornament|<htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|#909080|<arg|body>>>>|0fn|0.333fn><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Colors of standard tags (strong, math, etc.)
    </src-comment>
  </active*>

  <assign|strong-color|#202048>

  <assign|math-color|#602020>

  <assign|greyed-math-color|#b8a0ac>

  <assign|greyed|<macro|x|<with|color|#a8a8ae|math-color|<value|greyed-math-color>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <assign|input-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#e0e0e8>>

  <assign|fold-bar-color|<pattern|metal-brushed-dark.png|*3/5|*3/5|#a0a0bc>>

  <assign|fold-title-color|<pattern|metal-brushed-medium.png|*3/5|*3/5|#e0e0e8>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>