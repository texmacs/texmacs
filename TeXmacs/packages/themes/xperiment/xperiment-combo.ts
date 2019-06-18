<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|xperiment-combo|1.0|xperiment-combo|1.0>

    <\src-purpose>
      Experimental theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|reddish-combo|xperiment-deco>

  <copy-theme|xperiment|reddish>

  <select-theme|xperiment|xperiment-deco>

  <select-theme|xperiment|xperiment-bright-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <select-title-deco|xperiment|xperiment-deco>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|xperiment|xperiment-deco>

  <select-fold-title-deco|xperiment|xperiment-dark-deco>

  <select-fold-bar-deco|xperiment|xperiment-dark-deco>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|xperiment-title-block|<value|xperiment-deco-block>>

  <assign|xperiment-framed-block|<value|xperiment-deco-block>>

  <assign|xperiment-framed-block*|<value|xperiment-deco-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>