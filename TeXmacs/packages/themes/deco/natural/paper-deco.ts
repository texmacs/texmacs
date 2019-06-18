<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|paper-deco|1.0|paper-deco|1.0>

    <\src-purpose>
      Paper decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|paper-scene>

  <\active*>
    <\src-comment>
      Manila paper
    </src-comment>
  </active*>

  <copy-theme|paper-manila|light-deco>

  <new-deco|paper-manila>

  <assign|paper-manila-ornament-render-title|<value|with-paper-manila-dark-scene>>

  <assign|paper-manila-ornament-render-body|<value|with-paper-manila-light-scene>>

  <assign|paper-manila-ornament-sunny-color|#e8e8e0>

  <assign|paper-manila-ornament-shadow-color|#acac90>

  \;

  <copy-theme|paper-manila-light|paper-manila>

  <new-deco|paper-manila-light>

  <assign|paper-manila-light-ornament-render-body|<value|with-paper-manila-lighter-scene>>

  <assign|paper-manila-light-ornament-sunny-color|#f4f4f0>

  <assign|paper-manila-light-ornament-shadow-color|#dedec8>

  <\active*>
    <\src-comment>
      Ridged paper
    </src-comment>
  </active*>

  <copy-theme|paper-ridged|light-deco>

  <new-deco|paper-ridged>

  <assign|paper-ridged-ornament-render-title|<value|with-paper-ridged-dark-scene>>

  <assign|paper-ridged-ornament-render-body|<value|with-paper-ridged-light-scene>>

  <assign|paper-ridged-ornament-sunny-color|#f0e0e0>

  <assign|paper-ridged-ornament-shadow-color|#d0a0a0>

  \;

  <copy-theme|paper-ridged-light|paper-ridged>

  <new-deco|paper-ridged-light>

  <assign|paper-ridged-light-ornament-render-body|<value|with-paper-ridged-bright-scene>>

  <assign|paper-ridged-light-ornament-sunny-color|#f8f0f0>

  <assign|paper-ridged-light-ornament-shadow-color|#e8d0d0>

  <\active*>
    <\src-comment>
      Rough paper
    </src-comment>
  </active*>

  <copy-theme|paper-rough|light-deco>

  <new-deco|paper-rough>

  <assign|paper-rough-ornament-render-title|<value|with-paper-rough-dark-scene>>

  <assign|paper-rough-ornament-render-body|<value|with-paper-rough-light-scene>>

  <assign|paper-rough-ornament-sunny-color|#e0e0e0>

  <assign|paper-rough-ornament-shadow-color|#a0a0a0>

  \;

  <copy-theme|paper-rough-light|paper-rough>

  <new-deco|paper-rough-light>

  <assign|paper-rough-light-ornament-render-body|<value|with-paper-rough-lighter-scene>>

  <assign|paper-rough-light-ornament-sunny-color|#f8f8f0>

  <assign|paper-rough-light-ornament-shadow-color|#e8d0d0>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>