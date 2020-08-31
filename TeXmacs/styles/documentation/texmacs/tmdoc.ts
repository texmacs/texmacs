<TeXmacs|1.99.13>

<style|<tuple|source|english>>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmdoc|1.0>

      <\src-purpose>
        Style for the <TeXmacs> documentation.
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

  <use-package|std|env|title-generic|header-article|section-article|doc|pagella-font>

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

  <assign|save-aux|false>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|sectional-post-sep|<macro|<space|2spc>>>

  <assign|html-css|http://www.texmacs.org/css/tmdoc.css>

  <assign|html-head-javascript-src|http://www.texmacs.org/javascript/texmacs_functions.js>

  <\active*>
    <\src-comment>
      Sections, subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|section-font|<macro|name|<fira-font|<large|<arg|name>>>>>

  <assign|subsection-font|<macro|name|<fira-font|<arg|name>>>>

  <assign|subsubsection-font|<macro|name|<fira-font|<arg|name>>>>

  \;

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.25fn><section-font|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><subsection-font|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|0.5fn><subsubsection-font|<arg|name>><vspace|0.25fn>>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-font|<macro|name|<fira-font|<with|font-shape|italic|<arg|name>>>>>

  <assign|subparagraph-font|<macro|name|<fira-font|<with|font-shape|italic|<arg|name>>>>>

  \;

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><paragraph-font|<arg|name>>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short|<vspace*|0.5fn><subparagraph-font|<arg|name>>>>>>

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