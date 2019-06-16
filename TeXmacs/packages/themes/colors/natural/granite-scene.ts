<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|granite-scene|1.0|granite-scene|1.0>

    <\src-purpose>
      Coloring schemes on granite backgrounds
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-scene>

  <\active*>
    <\src-comment>
      Extra dark granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite-xdark-scene|dark-scene>

  <assign|stone-granite-xdark-scene-bg-color|<pattern|granite-xdark.png|*3/5|*3/5|#080808>>

  <assign|stone-granite-xdark-scene-monochrome-bg-color|#080808>

  <assign|stone-granite-xdark-scene-math-color|#ffd4c0>

  <assign|stone-granite-xdark-scene-strong-color|#f0ffb0>

  <\active*>
    <\src-comment>
      Dark granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite-dark-scene|dark-scene>

  <assign|stone-granite-dark-scene-bg-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>>

  <assign|stone-granite-xdark-scene-monochrome-bg-color|#101010>

  <assign|stone-granite-dark-scene-math-color|#ffd4c0>

  <assign|stone-granite-dark-scene-strong-color|#f0ffb0>

  <\active*>
    <\src-comment>
      Medium dark granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite-medium-scene|dark-scene>

  <assign|stone-granite-medium-scene-bg-color|<pattern|granite-medium.png|*3/5|*3/5|#202020>>

  <assign|stone-granite-medium-scene-monochrome-bg-color|#202020>

  <assign-uniform|stone-granite-medium-scene|gold>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>