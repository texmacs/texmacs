<TeXmacs|1.0.7.9>

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

  <use-package|framed-program>

  \;

  <assign|intro-color|pastel yellow>

  <assign|body-color|pastel blue>

  <assign|frame-color|dark grey>

  <\active*>
    <\src-comment>
      <TeXmacs> menus.
    </src-comment>
  </active*>

  <assign|menu-item|<macro|body|<with|font-family|ss|<localize|<arg|body>>>>>

  <assign|menu-extra|<macro|body|<active*|<with|mode|math|\<rightarrow\>>><menu-item|<arg|body>>>>

  <assign|menu|<xmacro|x|<style-with|src-compact|none|<menu-item|<arg|x|0>><map-args|menu-extra|concat|x|1><index-write|<map-args|menu-item|tuple|x>>>>>

  <\active*>
    <\src-comment>
      Content markup. Also used for indexing purposes. The <verbatim|markup>
      macro should be replaced by <verbatim|src-macro>.
    </src-comment>
  </active*>

  <assign|indexed|<macro|body|<arg|body><index|<arg|body>>>>

  <assign|markup|<macro|body|<src-macro|<arg|body>>>>

  <assign|tmstyle|<macro|body|<indexed|<with|font-family|tt|color|brown|<arg|body>>>>>

  <assign|tmpackage|<macro|body|<indexed|<with|font-family|tt|color|brown|<arg|body>>>>>

  <assign|tmdtd|<macro|body|<indexed|<with|font-family|tt|color|dark
  magenta|<arg|body>>>>>

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

  <assign|explain-synopsis|<macro|synopsis|<htab|5mm><with|color|dark
  grey|(<arg|synopsis>)><vspace|0.25fn>>>

  <assign|src-value|<macro|body|<with|font-shape|right|color|black|<arg|body>>>>

  <assign|var-val|<macro|var|val|<src-var|<arg|var>><active*|<with|mode|math|\<assign\>>><with|font-family|tt|<arg|val>>>>

  <\active*>
    <\src-comment>
      Links inside the documentation and special types of links. We are not
      very happy about this yet, so part of these macros will probably be
      modified sometime in the future.
    </src-comment>
  </active*>

  <assign|simple-link|<macro|destination|<hlink|<with|font-family|tt|<arg|destination>>|<arg|destination>>>>

  <assign|hyper-link*|<macro|body|destination|<hlink|<arg|body>|<arg|destination>>>>

  <assign|concept-link|<macro|body|<with|color|magenta|<arg|body>>>>

  <assign|only-index|<macro|body|<index|<arg|body>>>>

  <assign|def-index|<macro|body|<em|<arg|body>><index|<arg|body>>>>

  <assign|re-index|<macro|body|<arg|body><index|<arg|body>>>>

  <assign|example-plugin-link|<macro|plugin|<style-with|src-compact|none|<hlink|<with|font-family|tt|<arg|plugin>>|<merge|$TEXMACS_PATH/examples/plugins/|<arg|plugin>>>>>>

  <\active*>
    <\src-comment>
      Framed fragmentd.
    </src-comment>
  </active*>

  <assign|framed-table|<macro|body|<with|color|<value|frame-color>|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<arg|body>>>>>

  <assign|framed-fragment|<macro|body|<surround||<no-indent*>|<framed-table|<tformat|<table|<row|<\cell>
    <with|color|black|<arg|body>>
  </cell>>>>>>>>

  <assign|framed-fragment*|<\macro|body>
    <framed-table|<tformat|<twith|table-width|0.45par>|<table|<row|<\cell>
      <with|color|black|<arg|body>>
    </cell>>>>>
  </macro>>

  <assign|todo|<macro|body|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  red>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<table|<row|<cell|To
  do: <arg|body>>>>>>>>

  <\active*>
    <\src-comment>
      Fragments of <TeXmacs> code.
    </src-comment>
  </active*>

  <assign|tm-fragment|<\macro|body>
    <quotation|<framed-fragment|<arg|body>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of scheme code.
    </src-comment>
  </active*>

  <assign|scm-verb|<macro|body|<with|prog-language|verbatim|<arg|body>>>>

  <assign|scm-arg|<macro|body|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|body>>>>>>

  <assign|scm-args|<macro|body|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|body>>>><rsup|*>>>

  <assign|scm-opt-arg|<macro|body|<with|color|dark
  grey|[<style-with|<scm-arg|<arg|body>>>]>>>

  <\active*>
    <\src-comment>
      Fragments of mathemagix code.
    </src-comment>
  </active*>

  <assign|mmxlib|<macro|<with|font-shape|small-caps|Mmxlib>>>

  <assign|mmx-fragment*|<\macro|body>
    <framed-fragment|<with|par-par-sep|0fn|<mmx|<arg|body>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Tabular environments.
    </src-comment>
  </active*>

  <assign|descriptive-table|<macro|body|<tformat|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|-1|cell-background|pastel
  blue>|<twith|table-min-rows|2>|<twith|table-lborder|1ln>|<twith|table-rborder|1ln>|<twith|table-bborder|1ln>|<twith|table-tborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<twith|table-min-cols|2>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Other tags.
    </src-comment>
  </active*>

  <assign|icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/traditional/--x17|$TEXMACS_DOC_PATH/images/pixmaps|$TEXMACS_HOME_PATH/doc/images/pixmaps|$TEXMACS_PATH/doc/images/pixmaps|http://www.gnu.org/software/texmacs-doc/images/pixmaps|<arg|name>>||2ex||-0.333ex>>>

  <assign|screenshot|<macro|name|<image|<find-file|$TEXMACS_DOC_PATH/images/screenshots|$TEXMACS_HOME_PATH/doc/images/screenshots|$TEXMACS_PATH/doc/images/screenshots|http://www.gnu.org/software/texmacs-doc/images/screenshots|<arg|name>>|0.5w|0.5h||>>>

  \;

  <assign|cursor|<with|color|red|\|>>

  <assign|math-cursor|<with|color|magenta|\|>>

  <assign|small-focus|<macro|body|<with|color|cyan|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<table|<row|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|big-focus|<macro|body|<with|color|cyan|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0em>|<cwith|1|1|1|1|cell-rsep|0em>|<cwith|1|1|1|1|cell-bsep|0em>|<cwith|1|1|1|1|cell-tsep|0em>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|<arg|body>>
  </cell>>>>>>>>

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
    <associate|sfactor|3>
  </collection>
</initial>