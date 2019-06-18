<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|transparent-deco|1.0|transparent-deco|1.0>

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

  <use-package|light-deco|dark-deco|transparent-scene>

  <\active*>
    <\src-comment>
      Light transparent
    </src-comment>
  </active*>

  <copy-theme|transparent-light-deco|light-deco>

  <new-deco|transparent-light-deco>

  <assign|transparent-light-deco-ornament-render-title|<value|with-transparent-light-scene>>

  <assign|transparent-light-deco-ornament-render-body|<value|with-transparent-light-scene>>

  <assign|transparent-light-deco-ornament-sunny-color|dark grey>

  <assign|transparent-light-deco-ornament-shadow-color|dark grey>

  <complete-deco|transparent-light-deco>

  <\active*>
    <\src-comment>
      Dark transparent
    </src-comment>
  </active*>

  <copy-theme|transparent-dark-deco|dark-deco>

  <new-deco|transparent-dark-deco>

  <assign|transparent-dark-deco-ornament-render-title|<value|with-transparent-dark-scene>>

  <assign|transparent-dark-deco-ornament-render-body|<value|with-transparent-dark-scene>>

  <assign|transparent-dark-deco-ornament-sunny-color|light grey>

  <assign|transparent-dark-deco-ornament-shadow-color|light grey>

  <complete-deco|transparent-dark-deco>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>