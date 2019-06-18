<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|xperiment-deco|1.0|xperiment-deco|1.0>

    <\src-purpose>
      Experimental decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-deco|dark-deco|xperiment-scene>

  <\active*>
    <\src-comment>
      Experiment
    </src-comment>
  </active*>

  <copy-theme|xperiment-deco|light-deco>

  <new-deco|xperiment-deco>

  <assign|xperiment-deco-ornament-render-title|<value|with-xperiment-dark-scene>>

  <assign|xperiment-deco-ornament-render-body|<value|with-xperiment-light-scene>>

  <assign|xperiment-deco-ornament-sunny-color|pastel grey>

  <assign|xperiment-deco-ornament-shadow-color|grey>

  <\active*>
    <\src-comment>
      Dark variant
    </src-comment>
  </active*>

  <copy-theme|xperiment-dark-deco|dark-deco>

  <new-deco|xperiment-dark-deco>

  <assign|xperiment-dark-deco-ornament-render-title|<value|with-xperiment-dark-scene>>

  <assign|xperiment-dark-deco-ornament-render-body|<value|with-xperiment-dark-scene>>

  <assign|xperiment-dark-deco-ornament-sunny-color|#606080>

  <assign|xperiment-dark-deco-ornament-shadow-color|dark blue>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>