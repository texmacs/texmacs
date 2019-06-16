<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|stone-granite-deco|1.0|stone-granite-deco|1.0>

    <\src-purpose>
      Granite ornament for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-combo>

  <copy-theme|stone-granite|dark-deco>

  <new-deco|stone-granite>

  <\active*>
    <\src-comment>
      Dark granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite-dark-scene|dark-scene>

  <assign|stone-granite-dark-scene-bg-color|<pattern|granite-dark.png|*3/5|*3/5|#101010>>

  <assign|stone-granite-dark-scene-color|white>

  <assign|stone-granite-dark-scene-math-color|#ffd4c0>

  <assign|stone-granite-dark-scene-strong-color|#f0ffb0>

  <\active*>
    <\src-comment>
      Medium dark granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite-medium-scene|dark-scene>

  <assign|stone-granite-medium-scene-bg-color|<pattern|granite-medium.png|*3/5|*3/5|#202020>>

  <assign|stone-granite-medium-scene-color|gold>

  <assign|stone-granite-medium-scene-math-color|gold>

  <assign|stone-granite-medium-scene-strong-color|gold>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|stone-granite-ornament-render-title|<value|with-stone-granite-medium-scene>>

  <assign|stone-granite-ornament-render-body|<value|with-stone-granite-dark-scene>>

  <assign|stone-granite-ornament-sunny-color|light grey>

  <assign|stone-granite-ornament-shadow-color|dark grey>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>