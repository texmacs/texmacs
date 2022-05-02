<TeXmacs|2.1.1>

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

  <assign|tex-odd-side-margin|<macro|<minus|<maximum|<over|<minus|1paw|<tex-text-width>>|2>|1.5in>|1in>>>

  <assign|tex-even-side-margin|<value|tex-odd-side-margin>>

  <assign|tex-head-height|<macro|8pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|50.5pc>>

  <assign|tex-foot-skip|<macro|12pt>>

  <assign|tex-column-sep|<macro|10pt>>

  <assign|tex-margin-par-width|<macro|90pt>>

  <assign|par-first|<macro|12pt>>

  <assign|page-fnote-barlen|6em>

  <\active*>
    <\src-comment>
      Footnotes for titles and abstracts.
    </src-comment>
  </active*>

  <assign|render-plain-footnote|<macro|body|<style-with|src-compact|none|<\float|footnote|>
    <\style-with|src-compact|none>
      <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|font-shape|right|dummy|<value|page-fnote-sep>|dummy|<value|page-fnote-barlen>|<style-with|src-compact|none|<style-with|src-compact|none|<arg|body>>>>>
    </style-with>
  </float>>>>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-title-name|<macro|x|<strong|<change-case|<arg|x>|UPCASE>>>>

  <assign|doc-title|<\macro|x>
    \;

    <\surround|<vspace*|<minus|<tex-len|42pt|5pc|5pc>|1bls>>|>
      <doc-title-block|<doc-title-name|<arg|x>>>
    </surround>
  </macro>>

  <assign|author-name|<macro|author|<surround|<vspace*|<minus|18pt|1em>>||<doc-author-block|<small|<change-case|<arg|author>|UPCASE>>>>>>

  \;

  <assign|date-text|<macro|<localize|Date>>>

  <assign|note-text|<macro|<localize|Note>>>

  <assign|email-text|<macro|<localize|E-mail address>>>

  <assign|homepage-text|<macro|<localize|Homepage>>>

  <assign|doc-date|<macro|x|<surround|<em|<date-text>><localize|:>
  ||<arg|x>>>>

  <assign|doc-note|<macro|x|<surround|<em|<note-text>><localize|:>
  ||<arg|x>>>>

  <assign|author-affiliation|<macro|x|<quasi|<concat-tuple|<unquote|<arg|x>>|,
  >>>>

  <assign|prepend-comma|<macro|x|, <arg|x>>>

  <assign|author-affiliation|<macro|x|<with|font-shape|small-caps|<arg|x|0><map-args|prepend-comma|concat|x|1>>>>

  <assign|author-email|<macro|x|<surround|<em|<email-text>><localize|:>
  ||<verbatim|<arg|x>>>>>

  <assign|author-homepage|<macro|x|<surround|<em|<homepage-text>><localize|:>
  ||<verbatim|<arg|x>>>>>

  <assign|author-note|<macro|x|<surround|<em|<note-text>><localize|:>
  ||<arg|x>>>>

  <assign|author-misc|<macro|x|<arg|x>>>

  \;

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>|<tuple|ams-title>>>>

  <\active*>
    <\src-comment>
      Abstracts.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\padded-normal|20pt|20pt>
      <\with|par-left|3pc|par-right|3pc>
        <\small>
          <surround|<with|font-shape|small-caps|<abstract-text>>.
          ||<arg|body>>
        </small>
      </with>
    </padded-normal>
  </macro>>

  \;

  <assign|AMS-class-text|<macro|<localize|Mathematics Subject
  Classification>>>

  <assign|keywords-text|<macro|<localize|Key words and phrases>>>

  <assign|abstract-acm|<xmacro|args|<em|<ACM-class-text>>.
  <concat-tuple|<quote-arg|args>|; >>>

  <assign|abstract-arxiv|<xmacro|args|<em|<arXiv-class-text>>.
  <concat-tuple|<map|arxiv-ref|<quote-arg|args>>|; >>>

  <assign|abstract-msc|<xmacro|args|1991 <em|<AMS-class-text>>.
  <concat-tuple|<map|msc-ref|<quote-arg|args>>|; >>>

  <assign|abstract-pacs|<xmacro|args|<em|<PACS-class-text>>.
  <concat-tuple|<quote-arg|args>|; >>>

  <assign|abstract-keywords|<xmacro|args|<em|<keywords-text>>.
  <concat-tuple|<quote-arg|args>|, >>>

  \;

  <assign|render-abstract*|<\macro|body|note>
    <\quasi>
      <\render-abstract>
        <\surround|<render-plain-footnote|<unquote|<quote-arg|note>>>|>
          <unquote|<quote-arg|body>>
        </surround>
      </render-abstract>
    </quasi>
  </macro>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-odd-header|<with|font-size|0.707|<style-with|src-compact|none|<no-indent><htab|0mm><change-case|<arg|name>|UPCASE><htab|5mm><page-number>>>>>>>

  <assign|header-author|<macro|name|<assign|page-even-header|<with|font-size|0.707|<style-with|src-compact|none|<no-indent><page-number><htab|5mm><change-case|<arg|name>|UPCASE><htab|0mm>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <assign|simple-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<no-indent><htab|5mm><with|font-size|0.707|<page-number>><htab|5mm>>>>>

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

  <\active*>
    <\src-comment>
      Bibliographies.
    </src-comment>
  </active*>

  <assign|transform-bibitem|<macro|body|[<arg|body>] >>

  <\active*>
    <\src-comment>
      Theorems.
    </src-comment>
  </active*>

  <assign|large-padding-above|0.6666fn>

  <assign|large-padding-below|0.6666fn>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>