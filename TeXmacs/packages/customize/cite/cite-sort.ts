<TeXmacs|1.99.8>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|cite-sort|1.0>

    <\src-purpose>
      This package contains macros for the automatic sorting of citations.
    </src-purpose>

    <src-copyright|2013|François Poulain, Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(utils cite cite-sort)>

  <assign|cite-sort|<macro|body|<extern|cite-sort|<arg|body>>>>

  <assign|cite-raw|<xmacro|keys|<cite-sort|<map-args|cite-arg|tuple|keys>>>>

  <assign|cite|<xmacro|keys|<render-cite|<cite-sort|<map-args|cite-arg|tuple|keys>>>>>
</body>

<initial|<\collection>
</collection>>