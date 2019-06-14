<TeXmacs|1.99.9>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|old-jsc|1.0>

      <\src-purpose>
        The old jsc style used by Academic Press
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|std|env|title-generic|header-article|section-article>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\with|par-left|15mm|par-right|15mm>
      <\small>
        <\padded-bothlined|2.5bls|2.5bls|1ln|1ln|0.5bls|0.5bls>
          <surround|<yes-indent>||<arg|body>>
        </padded-bothlined>
      </small>
    </with>
  </macro>>

  <assign|author-by|<macro|body|<arg|body>>>

  <assign|author-render-name|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-even-header|<style-with|src-compact|none|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<unquote|<page-number>><space|4spc><unquote|<arg|name>>>>>>>>>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|<style-with|src-compact|none|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<htab|0mm><unquote|<arg|name>><space|4spc><unquote|<page-number>>>>>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  \;

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|1fn><arg|name><vspace|1fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|1fn><with|font-shape|small-caps|<arg|name>><vspace|1fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|1fn><with|font-shape|small-caps|<arg|name>><vspace|1fn>>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><with|font-shape|small-caps|<arg|name>>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><with|font-shape|small-caps|<arg|name>>>>>>

  <\active*>
    <\src-comment>
      Other customizations.
    </src-comment>
  </active*>

  <assign|enunciation-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>