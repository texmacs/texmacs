<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-markup|1.0>

    <\src-purpose>
      Content markup and other macros for the <TeXmacs> documentation.
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

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
      <TeXmacs> menus.
    </src-comment>
  </active*>

  <assign|menu-item|<macro|x|<with|font-family|ss|<localize|<arg|x>>>>>

  <assign|menu-extra|<macro|x|<active*|<with|mode|math|\<rightarrow\>>><menu-item|<arg|x>>>>

  <assign|menu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|0>><map-args|menu-extra|concat|x|1><index-write|<map-args|menu-item|tuple|x>>>>>

  <\active*>
    <\src-comment>
      Content markup. Also used for indexing purposes. The <verbatim|markup>
      macro should be replaced by <verbatim|src-macro>.
    </src-comment>
  </active*>

  <assign|indexed|<macro|x|<arg|x><index|<arg|x>>>>

  <assign|markup|<macro|x|<src-macro|<arg|x>>>>

  <assign|tmstyle|<macro|x|<indexed|<with|font-family|tt|color|brown|<arg|x>>>>>

  <assign|tmpackage|<macro|x|<indexed|<with|font-family|tt|color|brown|<arg|x>>>>>

  <assign|tmdtd|<macro|x|<indexed|<with|font-family|tt|color|dark
  magenta|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Documentation of <TeXmacs> macros.
    </src-comment>
  </active*>

  <assign|explain|<\macro|what|body>
    <\with|par-first|0fn|par-par-sep|0fn>
      <surround|<vspace*|0.5fn>|<no-page-break>|<arg|what>>
    </with>

    <surround||<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>
  </macro>>

  <assign|explain-macro-sub|<macro|x|pos|<if|<equal|<arg|pos>|0>|<indexed|<src-macro|<arg|x>>>|<src-arg|<arg|x>>>>>

  <assign|explain-macro|<xmacro|args|<map-args|explain-macro-sub|inline-tag|args>>>

  <drd-props|explain-macro|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|explain-synopsis|<macro|x|<htab|5mm><with|color|dark
  grey|(<arg|x>)><vspace|0.25fn>>>

  <assign|src-value|<macro|x|<with|font-shape|right|color|black|<arg|x>>>>

  <assign|var-val|<macro|var|val|<src-var|<arg|var>><active*|<with|mode|math|\<assign\>>><with|font-family|tt|<arg|val>>>>

  <\active*>
    <\src-comment>
      Links inside the documentation and special types of links. We are not
      very happy about this yet, so part of these macros will probably be
      modified sometime in the future.
    </src-comment>
  </active*>

  <assign|simple-link|<macro|x|<hlink|<with|font-family|tt|<arg|x>>|<arg|x>>>>

  <assign|hyper-link|<macro|x|y|<hlink|<arg|x>|<arg|y>>>>

  <assign|hyper-link|<macro|x|y|<quasiquote|<style-with|src-compact|none|<datoms|<macro|x|<hlink|<arg|x>|<unquote|<arg|y>>>>|<with|color|blue|<arg|x>>>>>>>

  <assign|concept-link|<macro|x|<with|color|magenta|<arg|x>>>>

  <assign|only-index|<macro|x|<index|<arg|x>>>>

  <assign|def-index|<macro|x|<em|<arg|x>><index|<arg|x>>>>

  <assign|re-index|<macro|x|<arg|x><index|<arg|x>>>>

  <assign|example-plugin-link|<macro|x|<style-with|src-compact|none|<hlink|<with|font-family|tt|<arg|x>>|<merge|$TEXMACS_PATH/examples/plugins/|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Fragments of code.
    </src-comment>
  </active*>

  <assign|scheme|<name|Scheme>>

  <assign|cpp|<name|C++>>

  <assign|framed-table|<macro|x|<with|color|dark
  grey|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-background|pastel
  blue>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<arg|x>>>>>

  <assign|framed-fragment|<macro|x|<framed-table|<tformat|<table|<row|<\cell>
    <with|color|black|<arg|x>>
  </cell>>>>>>>

  <assign|framed-fragment*|<\macro|x>
    <framed-table|<tformat|<twith|table-width|0.45par>|<table|<row|<\cell>
      <with|color|black|<arg|x>>
    </cell>>>>>
  </macro>>

  <assign|scheme-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|cpp-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <assign|tm-fragment|<\macro|x>
    <quotation|<framed-fragment|<arg|x>>>
  </macro>>

  <assign|scheme-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|cpp-code|<macro|x|<verbatim|<arg|x>>>>

  <assign|todo|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<cell|To
  do: <arg|x>>>>>>>>

  <\active*>
    <\src-comment>
      Tabular environments.
    </src-comment>
  </active*>

  <assign|descriptive-table|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|-1|cell-background|pastel
  blue>|<twith|table-min-rows|2>|<twith|table-lborder|1ln>|<twith|table-rborder|1ln>|<twith|table-bborder|1ln>|<twith|table-tborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<twith|table-min-cols|2>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Other tags.
    </src-comment>
  </active*>

  <assign|icon|<macro|name|<postscript|<find-file|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<arg|name>>|/2|/2||||>>>

  <assign|screenshot|<macro|name|<postscript|<find-file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<arg|name>>|/2|/2||||>>>

  \;

  <new-theorem|question|Question>

  <assign|answer|<macro|body|<quotation|<surround|<theorem-name|<localize|Answer><theorem-sep>>||<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|par-par-sep|0fn>
    <associate|preamble|true>
  </collection>
</initial>