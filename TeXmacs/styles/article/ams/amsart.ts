<TeXmacs|1.99.9>

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

  <use-package|std|env|title-generic|header-article|section-article>

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

  <assign|enrich-subsection-long|false>

  <assign|enrich-subsubsection-long|false>

  \;

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered|<vspace*|1fn><with|font-shape|small-caps|<arg|name>><vspace|0.5fn>>>>>

  <assign|var-section-title|<macro|name|<style-with|src-compact|none|<new-page><new-line><sectional-normal-bold|<vspace*|3fn><very-large|<arg|name>><vspace|2fn>>>>>

  <assign|appendix-title|<macro|name|<style-with|src-compact|none|<var-section-title|<appendix-text>.
  <arg|name>>>>>

  <assign|appendix-numbered-title|<macro|name|<style-with|src-compact|none|<var-section-title|<appendix-text>
  <the-appendix>. <arg|name>>>>>

  <\active*>
    <\src-comment>
      Subections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>>>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>