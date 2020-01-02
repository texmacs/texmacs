<TeXmacs|1.99.12>

<style|<tuple|source|english>>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmmanual|1.0>

      <\src-purpose>
        Style for the <TeXmacs> manual(s).
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

  <use-package|tmbook|doc|pagella-font>

  <\active*>
    <\src-comment>
      Global style parameters.
    </src-comment>
  </active*>

  <assign|par-hyphen|professional>

  <assign|par-par-sep|0.5fn>

  <assign|par-first|0fn>

  <assign|padded-par-par-sep|0.5fn>

  <assign|indent-par-first|1.5fn>

  <assign|font-base-size|11>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-make-title|<\macro|name>
    <assign|page-this-header|><assign|page-this-footer|><vspace|0.33pag>

    <with|math-font-series|bold|font-series|bold|font-shape|small-caps|<style-with|src-compact|none|<really-huge|<doc-title-block|<arg|name>>>>>

    <new-page>

    <assign|page-this-header|><assign|page-this-footer|><vspace|0.33pag>

    <new-page>
  </macro>>

  <assign|title|<macro|name|<doc-make-title|<arg|name>>>>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-font|<macro|name|<with|font|Fira|font-shape|small-caps|<very-large|<arg|name>>>>>

  \;

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<new-dpage*><new-line><style-with|src-compact|none|<sectional-centered-bold|<vspace*|2fn><chapter-font|<arg|name>><vspace|3fn>>>>>>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|section-font|<macro|name|<with|font|Fira|<large|<arg|name>>>>>

  <assign|subsection-font|<macro|name|<with|font|Fira|<arg|name>>>>

  <assign|subsubsection-font|<macro|name|<with|font|Fira|<arg|name>>>>

  \;

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.25fn><section-font|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><subsection-font|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|0.5fn><subsubsection-font|<arg|name>><vspace|0.25fn>>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-font|<macro|name|<with|font|Fira|font-shape|italic|<arg|name>>>>

  <assign|subparagraph-font|<macro|name|<with|font|Fira|font-shape|italic|<arg|name>>>>

  \;

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><paragraph-font|<arg|name>>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><subparagraph-font|<arg|name>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>