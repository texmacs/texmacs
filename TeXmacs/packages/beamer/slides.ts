<TeXmacs|1.0.7.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|slides|1.0|slides|1.0>

    <\src-purpose>
      Style for paper versions of presentations.
    </src-purpose>

    <src-copyright|2009|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Global layout
    </src-comment>
  </active*>

  <assign|bg-color|white>

  <assign|page-medium|paper>

  <\active*>
    <\src-comment>
      Ornaments with no images
    </src-comment>
  </active*>

  <assign|ornament-border|2ln>

  <assign|ornament-color|#e0b050>

  <assign|ornament-sunny-color|#f0e0c0>

  <assign|ornament-shadow-color|#c07055>

  <assign|ornament-hpadding|1spc>

  <assign|ornament-vpadding|1spc>

  \;

  <assign|granite|<macro|x|<with|ornament-color|#101010|color|white|strong-color|#f0ffb0|math-color|#ffd4c0|ornament-sunny-color|light
  grey|ornament-shadow-color|dark grey|<ornament|<arg|x>>>>>

  <assign|manila-paper|<macro|x|<with|ornament-color|#d0d0c0|ornament-sunny-color|#e8e8e0|ornament-shadow-color|#acac90|<ornament|<arg|x>>>>>

  <assign|metal|<macro|x|<with|ornament-color|#c0c0d0|ornament-sunny-color|#e0e0e8|ornament-shadow-color|#9090ac|<ornament|<arg|x>>>>>

  <assign|pine|<macro|x|#0c3000>>

  <assign|ridged-paper|<macro|x|<with|ornament-color|#e8dcdc|ornament-sunny-color|#f0e0e0|ornament-shadow-color|#d0a0a0|<ornament|<arg|x>>>>>

  <assign|rough-paper|<macro|x|<with|ornament-color|#dcdcdc|ornament-sunny-color|#e0e0e0|ornament-shadow-color|#a0a0a0|<ornament|<arg|x>>>>>

  \;

  <assign|tit|<macro|body|<with|ornament-color|#f0f0d8|<ornament|<with|font-series|bold|color|dark
  brown|><htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|dark
  brown|<arg|body>>>>|0fn|0.333fn><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Further customizations
    </src-comment>
  </active*>

  <assign|slide|<\macro|body>
    <\surround||<new-page><right-flush>>
      <arg|body>
    </surround>
  </macro>>

  <assign|xtit|<macro|body|<section|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>