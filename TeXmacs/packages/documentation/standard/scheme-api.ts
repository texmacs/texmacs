<TeXmacs|1.0.7.20>

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

  <use-package|doc>

  <use-module|(doc apidoc-markup)>

  <\active*>
    <\src-comment>
      Module documentation
    </src-comment>
  </active*>

  <assign|link-to-module-doc|<macro|module|<extern|doc-module-doc-link|<quote-arg|module>>>>

  <assign|link-to-module-source|<macro|module|<extern|doc-module-source-link|<quote-arg|module>>>>

  \;

  <assign|doc-module-header-body|<\macro|module|desc>
    <\explain>
      <link-to-module-source|<arg|module>>
      <explain-synopsis|<extern|doc-module-synopsis|<quote-arg|module>>>
    <|explain>
      <with|font-series|bold|Dependencies:>

      <\right-aligned>
        <extern|doc-module-dependencies|<quote-arg|module>>
      </right-aligned>

      <with|font-series|bold|Exported: <extern|doc-module-count-exported|<quote-arg|module>>><htab|><with|font-series|bold|Undocumented:
      <with|color|red|<extern|doc-module-count-undocumented|<quote-arg|module>>>>

      <arg|desc>
    </explain>
  </macro>>

  <drd-props|doc-module-header-body|arity|2|accesible|all>

  \;

  <assign|doc-module-header|<\macro|module-name|module-description>
    <\with|frame-color|light grey|body-color|#f2fffc>
      <\framed-table>
        <table|<row|<\cell>
          <\with|color|black>
            <doc-module-header-body|<arg|module-name>|<arg|module-description>>
          </with>
        </cell>>>
      </framed-table>
    </with>
  </macro>>

  <drd-props|doc-module-header|arity|2>

  \;

  <assign|doc-module-traverse|<with|frame-color|light
  grey|body-color|#f2fffc|<framed-table|<table|<row|<cell|<with|color|black|<traverse|<macro|root|<extern|doc-module-traverse|<quote-arg|root>>>>>>>>>>>

  \;

  <drd-props|doc-module-traverse|arity|1>

  \;

  <assign|doc-glue-traverse|<traverse|<macro|root|<extern|doc-glue-traverse|<quote-arg|root>>>>>

  <drd-props|doc-glue-traverse|arity|1>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|par-par-sep|0fn>
    <associate|preamble|true>
    <associate|sfactor|3>
  </collection>
</initial>