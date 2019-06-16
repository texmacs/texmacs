<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-ridged-deco|1.0|paper-ridged-deco|1.0>

    <\src-purpose>
      Ridged paper ornament for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|paper-vintage-scene>

  <copy-theme|paper-ridged|light-deco>

  <new-deco|paper-ridged>

  <\active*>
    <\src-comment>
      The ornament
    </src-comment>
  </active*>

  <assign|paper-ridged-ornament-render-title|<value|with-paper-ridged-dark-scene>>

  <assign|paper-ridged-ornament-render-body|<value|with-paper-ridged-light-scene>>

  <assign|paper-ridged-ornament-sunny-color|#f0e0e0>

  <assign|paper-ridged-ornament-shadow-color|#d0a0a0>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>