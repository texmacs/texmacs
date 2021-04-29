<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|ieeeconf|0.1>

      <\src-purpose>
        The IEEEconf style.
      </src-purpose>

      <\src-copyright|2013>
        François Poulain, Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <active*|<src-comment|IEEEconf is quite imcomplete for the moment.
  Currently, it is essentially a hacked version of article>>

  <use-package|article|std-latex|two-columns|termes-font>

  <active*|<src-comment|Global layout parameters>>

  <assign|tex-odd-side-margin|<macro|-.304in>>

  <assign|tex-even-side-margin|<macro|-.304in>>

  <assign|tex-text-width|<macro|6.875in>>

  <assign|tex-top-margin|<macro|0in>>

  <assign|tex-head-height|<macro|0in>>

  <assign|tex-text-height|<macro|8.875in>>

  <assign|tex-column-sep|<macro|0.375in>>

  <assign|tex-column-sep|<macro|0.3125in>>

  <assign|par-first|<macro|0.25in>>

  <assign|par-first|<macro|1pc>>

  <\active*>
    <\src-comment>
      Title + abstract.
    </src-comment>
  </active*>

  <assign|doc-make-title|<\macro|body>
    <\with|par-columns|1>
      <\surround||<right-flush>>
        <doc-title-block|<arg|body>>

        \;

        \;
      </surround>
    </with>
  </macro>>

  <assign|render-abstract|<\macro|body>
    <sectional-centered-bold|<abstract-text>><vspace|0.5fn>

    <\with|font-shape|italic>
      <arg|body>
    </with>
  </macro>>

  <assign|render-abstract*|<\macro|body|note>
    <\quasi>
      <\render-abstract>
        <surround||<vspace|0.5fn>|<unquote|<quote-arg|body>>>

        <\with|par-par-sep|0.25fn|font-shape|right>
          <unquote*|<arg|note>>
        </with>
      </render-abstract>
    </quasi>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>