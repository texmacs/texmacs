<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-article|1.0>

    <\src-purpose>
      Sectional markup for articles.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <use-package|section-base>

  <\active*>
    <\src-comment>
      Parts and chapters.
    </src-comment>
  </active*>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|5fn><really-huge|<arg|name>><vspace|2fn>>>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|4fn><huge|<arg|name>><vspace|1.5fn>>>>>

  <\active*>
    <\src-comment>
      Sections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|3fn><very-large|<arg|name>><vspace|1fn>>>>>

  <assign|appendix-title|<macro|title|<style-with|src-compact|none|<section-title|<appendix-text><sectional-sep><arg|title>>>>>

  <assign|appendix-numbered-title|<macro|title|<style-with|src-compact|none|<section-title|<appendix-text>
  <the-appendix><sectional-sep><arg|title>>>>>

  <\active*>
    <\src-comment>
      Subsections and subsubsections.
    </src-comment>
  </active*>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|2fn><large|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><arg|name><vspace|0.5fn>>>>>

  <\active*>
    <\src-comment>
      Paragraphs and subparagraphs.
    </src-comment>
  </active*>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.5fn><arg|name><paragraph-sep>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.25fn><arg|name><subparagraph-sep>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>