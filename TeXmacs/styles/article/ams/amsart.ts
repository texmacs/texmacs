<TeXmacs|1.99.13>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|amsart|1.0>

      <\src-purpose>
        The amsart style.
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

  <use-package|std|env|title-generic|header-article|section-article|std-latex>

  <\active*>
    <\src-comment>
      Global style parameters.
    </src-comment>
  </active*>

  <assign|tex-text-width|<macro|30pc>>

  <assign|tex-head-height|<macro|8pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|50.5pc>>

  <assign|tex-foot-skip|<macro|12pt>>

  <assign|tex-column-sep|<macro|10pt>>

  <assign|tex-margin-par-width|<macro|90pt>>

  <assign|par-first|<macro|12pt>>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\padded-normal|2fn|1fn>
      <\with|par-left|15mm|par-right|15mm>
        <\small>
          <surround|<with|font-shape|small-caps|<abstract-text>>.
          ||<arg|body>>
        </small>
      </with>
    </padded-normal>
  </macro>>

  <assign|doc-render-title|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<style-with|src-compact|none|<doc-title-block|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|<large|<arg|x>>>>>>>>

  <assign|author-by|<macro|body|<arg|body>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><page-number><htab|5mm><with|font-shape|small-caps|<arg|name>><htab|0mm>>>>>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><htab|0mm><with|font-shape|small-caps|<arg|name>><htab|5mm><page-number>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|sectional-post-sep|<macro|. >>

  <assign|part-post-sep|<macro|>>

  <assign|chapter-post-sep|<macro|>>

  <assign|section-post-sep|<macro|>>

  <assign|enrich-subsection-long|false>

  <assign|enrich-subsubsection-long|false>

  \;

  <assign|sectional-normal|<macro|name|<wide-normal|<arg|name><no-page-break>>>>

  <assign|sectional-centered|<macro|name|<wide-centered|<arg|name><no-page-break>>>>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tmlen|1bls|1bls|2bls>><arg|name><vspace|0.5bls>>>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|<tmlen|1bls|1bls|2bls>><arg|name><vspace|0.5bls>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|<tmlen|0.7bls|0.7bls|1.7bls>><with|font-shape|small-caps|<arg|name>><vspace|0.5bls>>>>>

  <\active*>
    <\src-comment>
      Subections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection-title|<macro|name|<with|sectional-prefixed|<macro|prefix|name|<prefixed-line|<with|font-series|medium|<arg|prefix>>|<arg|name>>>|<style-with|src-compact|none|<sectional-short-bold|<vspace*|<tmlen|0.5bls|0.5bls|1.2bls>><arg|name>>>>>>

  <assign|subsubsection-title|<macro|name|<with|sectional-prefixed|<macro|prefix|name|<prefixed-line|<with|font-shape|right|<arg|prefix>>|<arg|name>>>|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tmlen|0.5bls|0.5bls|1.2bls>><arg|name>>>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<sectional-short|<arg|name>>>>

  <assign|subparagraph-title|<macro|name|<sectional-short|<arg|name>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>