<TeXmacs|1.0.7.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|slides|1.0|slides|1.0>

    <\src-purpose>
      Style for paper versions of presentations.
    </src-purpose>

    <src-copyright|2009|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|presentation>

  <\active*>
    <\src-comment>
      Global layout
    </src-comment>
  </active*>

  <assign|bg-color|white>

  <assign|page-medium|paper>

  <\active*>
    <\src-comment>
      Further customizations
    </src-comment>
  </active*>

  <assign|slide|<\macro|body>
    <\surround||<new-page><right-flush>>
      <arg|body>
    </surround>
  </macro>>

  <assign|xtit|<macro|body|<section|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>