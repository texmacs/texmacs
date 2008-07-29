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
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
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