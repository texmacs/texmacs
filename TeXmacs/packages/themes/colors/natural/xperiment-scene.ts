<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|xperiment-scene|1.0|xperiment-scene|1.0>

    <\src-purpose>
      Experimental coloring scheme
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-scene|dark-scene>

  <\active*>
    <\src-comment>
      Bright experiment
    </src-comment>
  </active*>

  <copy-theme|xperiment-bright-scene|light-scene>

  <assign|xperiment-bright-scene-bg-color|<pattern|textile/subtle_white_feathers.png|*1/5|*1/5>>

  <\active*>
    <\src-comment>
      Light experiment
    </src-comment>
  </active*>

  <copy-theme|xperiment-light-scene|xperiment-bright-scene>

  <\active*>
    <\src-comment>
      Dark experiment
    </src-comment>
  </active*>

  <copy-theme|xperiment-dark-scene|dark-scene>

  <assign|xperiment-dark-scene-bg-color|<pattern|textile/tex2res4.png|*1/5|*1/5|<eff-monochrome|0|#000020|0>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>