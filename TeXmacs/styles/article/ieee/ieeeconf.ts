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

  <use-package|article|std-latex|two-columns|termes-font>

  <active*|<src-comment|Global style parameters>>

  <assign|par-sep|0.2fn>

  <assign|par-ver-sep|0.1fn>

  <active*|<src-comment|Global layout parameters>>

  <assign|tex-odd-side-margin|<macro|-.304in>>

  <assign|tex-even-side-margin|<macro|-.304in>>

  <assign|tex-text-width|<macro|6.875in>>

  <assign|tex-top-margin|<macro|0in>>

  <assign|tex-head-height|<macro|0in>>

  <assign|tex-text-height|<macro|8.875in>>

  <assign|tex-column-sep|<macro|0.3125in>>

  <assign|par-first|<macro|0.25in>>

  <\active*>
    <\src-comment>
      Title.
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

  <assign|by-text|<macro|>>

  <assign|author-name|<macro|author|<doc-author-block|<author-by|<arg|author>>>>>

  <assign|author-email|<macro|email|<doc-author-block|<with|font-shape|italic|<email-text><localize|:>
  > <arg|email>>>>

  <assign|author-email-note|<macro|sym|id|email|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<with|font-shape|italic|<email-text><localize|:>
  ><arg|email>>>>>

  <\active*>
    <\src-comment>
      Abstract.
    </src-comment>
  </active*>

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

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<assign|page-odd-header|><assign|page-even-header|>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|><assign|page-even-header|>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|sectional-post-sep|<macro|<space|2spc>>>

  <assign|sectional-no-indent|<macro|>>

  <\active*>
    <\src-comment>
      Lists.
    </src-comment>
  </active*>

  <assign|transform-bibitem|<macro|body|[<arg|body>] >>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>