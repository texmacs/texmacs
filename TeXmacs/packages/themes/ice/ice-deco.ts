<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|ice-deco|1.0|ice-deco|1.0>

    <\src-purpose>
      Icy decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|dark-deco|ice-scene>

  <\active*>
    <\src-comment>
      Ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice|light-deco>

  <new-deco|natural-ice>

  <assign|natural-ice-ornament-render-title|<value|with-natural-ice-dark-scene>>

  <assign|natural-ice-ornament-render-body|<value|with-natural-ice-light-scene>>

  <assign|natural-ice-ornament-sunny-color|#f4f4ff>

  <assign|natural-ice-ornament-shadow-color|#d8d8ff>

  <\active*>
    <\src-comment>
      Light ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-light|natural-ice>

  <new-deco|natural-ice-light>

  <assign|natural-ice-light-ornament-render-body|<value|with-natural-ice-lighter-scene>>

  <assign|natural-ice-light-ornament-shadow-color|#e4e4f8>

  <\active*>
    <\src-comment>
      Dark ice
    </src-comment>
  </active*>

  <copy-theme|natural-ice-dark|dark-deco>

  <new-deco|natural-ice-dark>

  <assign|natural-ice-dark-ornament-render-title|<value|with-natural-ice-dark-scene>>

  <assign|natural-ice-dark-ornament-render-body|<value|with-natural-ice-dark-scene>>

  <assign|natural-ice-dark-ornament-sunny-color|#7070a0>

  <assign|natural-ice-dark-ornament-shadow-color|#404080>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>