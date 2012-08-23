<TeXmacs|1.0.7.15>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|scheme-api|1.0>

    <\src-purpose>
      Macros for the <TeXmacs> scheme API documentation.
    </src-purpose>

    <src-copyright|2012-|>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|tmdoc-markup>

  <\active*>
    <\src-comment>
      Module documentation
    </src-comment>
  </active*>

  <assign|link-to-module-doc|<macro|module|<extern|doc-module-doc-link|<arg|module>>>>

  <assign|link-to-module-src|<macro|module|<extern|doc-module-src-link|<arg|module>>>>

  \;

  <assign|doc-module-header-body|<\macro|module|desc>
    <\explain>
      <tt|<arg|module>> <explain-synopsis|<extern|doc-module-synopsis|<arg|module>>>
    <|explain>
      <with|font-series|bold|Module family:><htab|><tt|<extern|doc-module-family|<arg|module>>>

      <with|font-series|bold|Dependencies:>

      <\right-aligned>
        <extern|doc-module-dependencies|<arg|module>>
      </right-aligned>

      <with|font-series|bold|Source:><htab|><link-to-module-src|<arg|module>>

      <with|font-series|bold|Total exported
      symbols:><htab|><with|font-series|bold|<extern|doc-module-count-exported|<arg|module>>>

      <with|font-series|bold|Undocumented
      symbols:><htab|><with|font-series|bold|<with|color|red|<extern|doc-module-count-undocumented|<arg|module>>>>

      \;

      <arg|desc>
    </explain>

    \;
  </macro>>

  <drd-props|doc-module-header-body|arity|2|accesible|all>

  \;

  <assign|doc-module-header|<\macro|module-name|module-description>
    <\with|frame-color|light-grey|body-color|#f2fffc>
      <\framed-table>
        <\with|color|black>
          <tabular|<tformat|<table|<row|<\cell>
            <compound|doc-module-header-body|<arg|module-name>|<arg|module-description>>
          </cell>>>>>
        </with>
      </framed-table>
    </with>
  </macro>>

  \;

  <assign|traverse-modules-doc|<macro|root|<extern|doc-module-traverse|<arg|root>>>>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|par-par-sep|0fn>
    <associate|preamble|true>
    <associate|sfactor|3>
  </collection>
</initial>