<TeXmacs|1.99.8>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-package|edu-compact|1.0>

      <\src-purpose>
        A compact text style for fitting a maximal amount of text on every
        page
      </src-purpose>

      <\src-copyright|2013>
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

  <use-package|compact-list|reduced-margins|padded-paragraphs>

  <\active*>
    <\src-comment>
      More compact equations and item lists
    </src-comment>
  </active*>

  <assign|padding-above|0.25fn>

  <assign|padding-below|0.25fn>

  <assign|large-padding-above|0.5fn>

  <assign|large-padding-below|0.5fn>

  <assign|item-vspace|0fn>

  <\active*>
    <\src-comment>
      More compact titles
    </src-comment>
  </active*>

  <assign|title*|<macro|body|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|<large|<arg|body>>>>>

  <assign|title|<\macro|body>
    <\with|par-mode|center>
      <surround|<vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<title*|<arg|body>>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      More compact chapters and sections
    </src-comment>
  </active*>

  <assign|part-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|2fn><huge|<arg|name>><vspace|1fn>>>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><very-large|<arg|name>><vspace|0.5fn>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><large|<arg|name>><vspace|0.3333fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|0.75fn><arg|name><vspace|0.25fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|0.5fn><arg|name>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>