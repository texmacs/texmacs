<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-acmconf|1.0>

    <\src-purpose>
      Sectional markup for the acmconf style.
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

  <use-package|section-article>

  <assign|section-article-2col-package|1.0>

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

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>