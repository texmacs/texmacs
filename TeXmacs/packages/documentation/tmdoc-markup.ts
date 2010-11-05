<TeXmacs|1.0.7.7>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-markup|1.0>

    <\src-purpose>
      Content markup and other macros for the <TeXmacs> documentation.
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
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

  <assign|explain-header|<\macro|what>
    <\with|par-first|0fn|par-par-sep|0fn>
      <surround|<vspace*|0.5fn>|<no-page-break>|<arg|what>>
    </with>
  </macro>>

  <assign|explain-body|<\macro|body>
    <surround||<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>
  </macro>>

  <assign|explain|<\macro|what|body>
    <explain-header|<arg|what>>

    <explain-body|<arg|body>>
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

  <assign|hyper-link*|<macro|x|y|<hlink|<arg|x>|<arg|y>>>>

  <assign|concept-link|<macro|x|<with|color|magenta|<arg|x>>>>

  <assign|only-index|<macro|x|<index|<arg|x>>>>

  <assign|def-index|<macro|x|<em|<arg|x>><index|<arg|x>>>>

  <assign|re-index|<macro|x|<arg|x><index|<arg|x>>>>

  <assign|example-plugin-link|<macro|x|<style-with|src-compact|none|<hlink|<with|font-family|tt|<arg|x>>|<merge|$TEXMACS_PATH/examples/plugins/|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Framed fragmentd.
    </src-comment>
  </active*>

  <assign|framed-table|<macro|x|<with|color|dark
  grey|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-background|pastel
  blue>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<arg|x>>>>>

  <assign|framed-fragment|<macro|x|<surround||<no-indent*>|<framed-table|<tformat|<table|<row|<\cell>
    <with|color|black|<arg|x>>
  </cell>>>>>>>>

  <assign|framed-fragment*|<\macro|x>
    <framed-table|<tformat|<twith|table-width|0.45par>|<table|<row|<\cell>
      <with|color|black|<arg|x>>
    </cell>>>>>
  </macro>>

  <assign|todo|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<cell|To
  do: <arg|x>>>>>>>>

  <\active*>
    <\src-comment>
      Fragments of <TeXmacs> code.
    </src-comment>
  </active*>

  <assign|tm-fragment|<\macro|x>
    <quotation|<framed-fragment|<arg|x>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of verbatim code.
    </src-comment>
  </active*>

  <assign|verbatim-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<verbatim|<arg|x>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of scheme code.
    </src-comment>
  </active*>

  <assign|scheme|<macro|<name|Scheme>>>

  <assign|scm|<macro|x|<with|mode|prog|prog-language|scheme|<arg|x>>>>

  <assign|scm-verb|<macro|x|<with|prog-language|verbatim|<arg|x>>>>

  <assign|scm-arg|<macro|x|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|x>>>>>>

  <assign|scm-args|<macro|x|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|x>>>><rsup|*>>>

  <assign|scm-opt-arg|<macro|x|<with|color|dark
  grey|[<style-with|<scm-arg|<arg|x>>>]>>>

  <assign|scm-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<scm|<arg|x>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of shell code.
    </src-comment>
  </active*>

  <assign|shell|<macro|x|<with|mode|prog|prog-language|shell|<arg|x>>>>

  <assign|shell-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<shell|<arg|x>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of C++ code.
    </src-comment>
  </active*>

  <assign|c++|<macro|<name|C++>>>

  <assign|cpp|<macro|x|<with|mode|prog|prog-language|cpp|<arg|x>>>>

  <assign|cpp-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<cpp|<arg|x>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of mathemagix code.
    </src-comment>
  </active*>

  <assign|mmxlib|<macro|<with|font-shape|small-caps|Mmxlib>>>

  <assign|mathemagix|<macro|<with|font-shape|small-caps|Mathemagix>>>

  <assign|mmx|<macro|x|<with|mode|prog|prog-language|mathemagix|<arg|x>>>>

  <assign|mmx-fragment|<\macro|x>
    <quote-env|<framed-fragment|<with|par-par-sep|0fn|<mmx|<arg|x>>>>>
  </macro>>

  <assign|mmx-fragment*|<\macro|x>
    <framed-fragment|<with|par-par-sep|0fn|<mmx|<arg|x>>>>
  </macro>>

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

  <assign|icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<arg|name>>|0.5w|0.5h||>>>

  <assign|screenshot|<macro|name|<image|<find-file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<arg|name>>|0.5w|0.5h||>>>

  \;

  <assign|cursor|<with|color|red|\|>>

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