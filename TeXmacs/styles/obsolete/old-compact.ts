<TeXmacs|1.99.8>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|compact|1.0>

      <\src-purpose>
        A compact text style for fitting a maximal amount of text on every
        page
      </src-purpose>

      <\src-copyright|2013>
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

  <use-package|std|env|title-generic|header-exam|section-beamer|compact-list|reduced-margins>

  \;

  <assign|padding-above|0.25fn>

  <assign|padding-below|0.25fn>

  <assign|large-padding-above|0.5fn>

  <assign|large-padding-below|0.5fn>

  \;

  <assign|title*|<macro|body|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|<large|<arg|body>>>>>

  <assign|title|<\macro|body>
    <\with|par-mode|center>
      <surround|<vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<title*|<arg|body>>>
    </with>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <\padded*>
      <surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>
    </padded*>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>