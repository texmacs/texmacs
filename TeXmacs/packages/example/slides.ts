<TeXmacs|1.0.7.2>

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

  <use-package|presentation>

  <\active*>
    <\src-comment>
      Customizations
    </src-comment>
  </active*>

  <assign|font-family|ss>

  <assign|bg-color|white>

  <assign|page-medium|paper>

  <assign|slide|<\macro|body>
    <\surround||<new-page><right-flush>>
      <arg|body>
    </surround>
  </macro>>

  <assign|xtit|<macro|body|<section|<arg|body>>>>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  <assign|granite|<macro|x|<with|ornament-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>|color|white|strong-color|#f0ffb0|math-color|#ffd4c0|ornament-sunny-color|light
  grey|ornament-shadow-color|dark grey|<ornament|<arg|x>>>>>

  <assign|pine|<macro|x|<with|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>|strong-color|#0c3000|math-color|#500000|<ornament|<arg|x>>>>>

  <assign|ridged|<macro|x|<with|ornament-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>|<ornament|<arg|x>>>>>

  <assign|input-color|<pattern|paper-ridged-medium.png|*3/5|*3/5|#e8dcd0>>

  <assign|fold-bar-color|<pattern|wood-light.png|*3/5|*3/5|#e0b050>>

  <assign|fold-title-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>