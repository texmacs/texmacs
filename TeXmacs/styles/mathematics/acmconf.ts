<TeXmacs|1.0.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmconf|1.0>

      <\src-purpose>
        The acmconf style.
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

  <use-package|std|env|title-generic|header-article|section-article>

  <\active*>
    <\src-comment>
      Global lay-out.
    </src-comment>
  </active*>

  <assign|par-columns|2>

  <assign|font-base-size|9>

  <assign|page-odd|17.5mm>

  <assign|page-even|17.5mm>

  <assign|par-width|175mm>

  <assign|page-top|15mm>

  <assign|page-bot|20mm>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-make-title|<macro|body|<surround||<vspace|2fn>|<with|par-columns|1|<doc-title-block|<arg|body>>>>>>

  <assign|doc-abstract|<\macro|body>
    <\padded-normal|1.5fn|1.5fn>
      <with|font-series|bold|<abstract-text>><vspace|1.5fn><no-page-break>

      <surround|<no-indent>||<arg|body>>
    </padded-normal>
  </macro>>

  <assign|doc-footnote|<macro|body|<style-with|src-compact|none|<if|<unequal|<get-arity|<quote-arg|body>>|0>|<quasi|<with|par-columns|1|<style-with|src-compact|none|<render-footnote|<unquote|<doc-author-note-next>>|<arg|body|0><map-args|doc-footnote-sub|concat|body|1>>>>>>>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  <assign|page-even-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|3fn><large|<arg|name>><vspace|1fn>>>>>

  <\active*>
    <\src-comment>
      Sections, subsections, subsubsections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><arg|name><vspace|1.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><arg|name><vspace|0.75fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><arg|name><vspace|0.5fn>>>>>

  <\active*>
    <\src-comment>
      Other customizations.
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|math-font-series|bold|font-series|bold|<arg|x>>
  |r-3fn||r|>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>