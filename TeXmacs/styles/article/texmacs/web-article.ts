<TeXmacs|1.99.13>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|web|article-1.0>

      <\src-purpose>
        Article style that mimicks web pages.
      </src-purpose>

      <\src-copyright|2020>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|generic|biolinum-font>

  <\active*>
    <\src-comment>
      Global page style.
    </src-comment>
  </active*>

  <assign|page-medium|automatic>

  <assign|page-screen-margin|false>

  \;

  <assign|page-odd|5em>

  <assign|page-even|5em>

  <assign|page-right|5em>

  <assign|page-top|5em>

  <assign|page-bot|5em>

  \;

  <assign|page-screen-left|5em>

  <assign|page-screen-right|5em>

  <assign|page-screen-top|5em>

  <assign|page-screen-bot|5em>

  \;

  <assign|page-odd-footer|>

  <assign|page-even-footer|>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>