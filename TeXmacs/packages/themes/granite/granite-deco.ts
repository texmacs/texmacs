<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|granite-deco|1.0|granite-deco|1.0>

    <\src-purpose>
      Granite decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-deco|granite-scene>

  <\active*>
    <\src-comment>
      Granite
    </src-comment>
  </active*>

  <copy-theme|stone-granite|dark-deco>

  <new-deco|stone-granite>

  <assign|stone-granite-ornament-render-title|<value|with-stone-granite-medium-scene>>

  <assign|stone-granite-ornament-render-body|<value|with-stone-granite-dark-scene>>

  <assign|stone-granite-ornament-sunny-color|light grey>

  <assign|stone-granite-ornament-shadow-color|dark grey>

  <assign|stone-granite-deco-shadow-effect|<eff-recolor|0|grey>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>