<TeXmacs|1.0.6.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|doxygen|1.0>

    <\src-purpose>
      Links to Doxygen documentation.
    </src-purpose>

    <src-copyright|2007|Gregoire Lecerf>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(utils misc doxygen)>

  <assign|doxygen-load|<macro|file|<extern|(lambda (x) (doxygen-load
  x))|<arg|file>>>>

  <assign|doxygen-ref|<macro|x|<extern|doxygen-ref|<arg|x>>>>

  <assign|doxygen-link|<macro|x|y|<hlink|<arg|x>|<doxygen-ref|<arg|y>>>>>

  <assign|doxygen-get|<macro|x|y|<extern|doxygen-get|<arg|x>|<arg|x>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>