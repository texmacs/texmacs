<TeXmacs|1.99.6>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|two-columns|1.0|two-columns|1.0>

    <\src-purpose>
      Standard customization for two column styles
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|par-columns|2>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|doc-make-title|<\macro|body>
    <\with|par-columns|1>
      <doc-title-block|<arg|body>>

      \;
    </with>
  </macro>>

  <assign|doc-make-rich-title|<\macro|notes|body>
    <\surround||<arg|notes>>
      <\doc-make-title>
        <arg|body>
      </doc-make-title>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Temporary hack.
    </src-comment>
  </active*>

  <assign|xcustom-footnote-text|<macro|sym|id|body|<custom-note-text|<arg|sym>|<arg|id>|<arg|body>><new-line>>>

  <assign|doc-make-rich-title|<\macro|notes|body>
    <\doc-make-title>
      <arg|body>
    </doc-make-title>

    <smaller|<arg|notes>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>