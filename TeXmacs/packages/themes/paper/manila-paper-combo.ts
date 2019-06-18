<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|manila-paper-combo|1.0|manila-paper-combo|1.0>

    <\src-purpose>
      Manilla paper theme for presentations and posters.
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

  <copy-theme|manila-paper|reddish>

  <select-theme|manila-paper|paper-manila>

  <select-theme|manila-paper|paper-manila-bright-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|manila-paper-title-bar-color|<macro|<pattern|paper-manila-medium.png|*3/5|*3/5|#d0d0c0>>>

  <assign|manila-paper-title-color|<macro|dark brown>>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|manila-paper|paper-manila-light>

  <select-fold-title-deco|manila-paper|paper-manila>

  <select-fold-bar-deco|manila-paper|paper-manila>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|manila-paper-title-block|<value|paper-manila-block>>

  <assign|manila-paper-framed-block|<value|paper-manila-block>>

  <assign|manila-paper-framed-block*|<value|paper-manila-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>