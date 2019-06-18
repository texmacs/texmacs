<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|rough-paper-combo|1.0|rough-paper-combo|1.0>

    <\src-purpose>
      Ridged paper theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|reddish-combo|paper-deco>

  <copy-theme|rough-paper|reddish>

  <select-theme|rough-paper|paper-rough>

  <select-theme|rough-paper|paper-rough-bright-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <select-title-deco|rough-paper|paper-rough>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|rough-paper|paper-rough-light>

  <select-fold-title-deco|rough-paper|paper-rough>

  <select-fold-bar-deco|rough-paper|paper-rough>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|rough-paper-title-block|<value|paper-rough-block>>

  <assign|rough-paper-framed-block|<value|paper-rough-block>>

  <assign|rough-paper-framed-block*|<value|paper-rough-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>