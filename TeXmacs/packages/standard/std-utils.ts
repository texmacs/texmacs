<TeXmacs|1.0.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-utils|1.0>

    <\src-purpose>
      This package contains useful macros for writing style files.
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

  <\active*>
    <\src-comment>
      Layout macros.
    </src-comment>
  </active*>

  <assign|hflush|<macro|<htab|0fn|0>>>

  <assign|right-flush|<macro|<htab|0fn|first>>>

  <assign|left-flush|<macro|<htab|0fn|last>>>

  <assign|wide-normal|<macro|body|<surround|<no-indent>|<right-flush>|<arg|body>>>>

  <assign|wide-centered|<macro|body|<surround|<no-indent><left-flush>|<right-flush>|<arg|body>>>>

  <assign|padded-normal|<macro|a|b|body|<surround|<vspace*|<arg|a>><no-indent>|<right-flush><vspace|<arg|b>>|<arg|body>>>>

  <assign|padded-centered|<macro|a|b|body|<surround|<vspace*|<arg|a>><no-indent><left-flush>|<right-flush><vspace|<arg|b>>|<arg|body>>>>

  <assign|indent-left|<\macro|l|body>
    <\with|par-left|<plus|<value|par-left>|<arg|l>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-right|<\macro|r|body>
    <\with|par-right|<plus|<value|par-right>|<arg|r>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-both|<\macro|l|r|body>
    <\with|par-left|<plus|<value|par-left>|<arg|l>>|par-right|<plus|<value|par-right>|<arg|r>>>
      <arg|body>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Other macros
    </src-comment>
  </active*>

  <assign|localize|<macro|x|<translate|<arg|x>|english|<value|language>>>>

  <assign|map|<macro|f|args|<extern|ext-map|<arg|f>|<arg|args>>>>

  <assign|set-header|<macro|s|<assign|page-odd-header|<arg|s>><assign|page-even-header|<arg|s>>>>

  <assign|set-footer|<macro|s|<assign|page-odd-footer|<arg|s>><assign|page-even-footer|<arg|s>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>