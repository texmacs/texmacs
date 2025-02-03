<TeXmacs|2.1.2>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package|gui-dark|1.0>

    <\src-purpose>
      Dark theme
    </src-purpose>

    <src-copyright|2023|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|gui-button>

  <\active*>
    <\src-comment>
      GUI color scheme
    </src-comment>
  </active*>

  <assign|color|white>

  <assign|locus-color|white>

  <assign|visited-color|white>

  <assign|bg-color|#404040>

  <assign|gui-bg-color|#404040>

  <assign|gui-sunny-color|#606060>

  <assign|gui-shadow-color|#202020>

  <assign|gui-blur-color|#c0c0ff>

  <assign|gui-select-color|#205080>

  <assign|button-bg-color|#606060>

  <assign|gui-input-color|#606060>

  <assign|gui-input-list-color|>

  <assign|gui-input-sunny-color|#000000>

  <assign|gui-input-shadow-color|#202020>

  <assign|gui-title-bg-color|#2c2c2c>

  <assign|gui-contour*|<macro|body|<with|marked-color|#8080ff80|<marked|<arg|body>>>>>

  <assign|toggle-off-hover|<gui-contour*|<toggle-off>>>

  <assign|toggle-on-hover|<gui-contour*|<toggle-on>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>