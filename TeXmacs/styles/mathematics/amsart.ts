<TeXmacs|1.0.4>

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
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|header-article|section-article>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-abstract|<\macro|body>
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

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><htab|0mm><with|font-shape|small-caps|<arg|s>><htab|5mm><quote|<page-the-page>>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><quote|<page-the-page>><htab|5mm><with|font-shape|small-caps|<arg|s>><htab|0mm>>>>>>

  \;

  <assign|header-title|<macro|name|<even-page-text|<arg|name>>>>

  <assign|header-author|<macro|name|<odd-page-text|<arg|name>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|enrich-subsection-long|<macro|false>>

  <assign|enrich-subsubsection-long|<macro|false>>

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

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>.
  >>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name>.
  >>>>

  <\active*>
    <\src-comment>
      Paragraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name><subparagraph-sep>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>