<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Default serialization>

  Documents are generally written to disk using the standard <TeXmacs> syntax
  (which corresponds to the <verbatim|.tm> and <verbatim|.ts> file
  extensions). This syntax is designed to be unobtrusive and easy to read, so
  the content of a document can be easily understood from a plain text
  editor. For instance, the formula (<reference|tm-tree-ex>) is represented
  by

  <\quote-env>
    <framed-fragment|<verbatim|\<less\>with\|mode\|math\|x+y+\<less\>frac\|1\|2\<gtr\>+\<less\>sqrt\|y+z\<gtr\>\<gtr\>>>
  </quote-env>

  On the other hand, <TeXmacs> syntax makes style files difficult to read and
  is not designed to be hand-edited: whitespace has complex semantics and
  some internal structures are not obviously presented. Do not edit documents
  (and especially style files) in the <TeXmacs> syntax unless you know what
  you are doing.

  <paragraph*|Main serialization principle>

  The <TeXmacs> format uses the special characters <verbatim|\<less\>>,
  <verbatim|\|>, <verbatim|\<gtr\>>, <verbatim|\\> and <verbatim|/> in order
  to serialize trees. By default, a tree like

  <\equation>
    <label|gen-tree-tm><tree|f|x<rsub|1>|\<cdots\>|x<rsub|n>>
  </equation>

  is serialized as

  <\tm-fragment>
    <verbatim|\<less\>f\|x<rsub|1>\|...\|x<rsub|n>\<gtr\>>
  </tm-fragment>

  If one of the arguments <math|x<rsub|1>,\<ldots\>,x<rsub|n>> is a
  multi-paragraph tree (which means in this context that it contains a
  <markup|document> tag or a <markup|collection> tag), then an alternative
  long form is used for the serialization. If <verbatim|f> takes only
  multi-paragraph arguments, then the tree would be serialized as

  <\tm-fragment>
    <\verbatim>
      \<less\>\\f\<gtr\>

      \ \ x<rsub|1>

      \<less\>\|f\<gtr\>

      \ \ ...

      \<less\>\|f\<gtr\>

      \ \ x<rsub|n>

      \<less\>/f\<gtr\>
    </verbatim>
  </tm-fragment>

  In general, arguments which are not multi-paragraph are serialized using
  the short form. For instance, if <verbatim|n=5> and <verbatim|x<rsub|3>>
  and <verbatim|x<rsub|5>> are multi-paragraph, but not <verbatim|x<rsub|1>,>
  <verbatim|x<rsub|2>> and <verbatim|x<rsub|4>>, then
  (<reference|gen-tree-tm>) is serialized as

  <\tm-fragment>
    <\verbatim>
      \<less\>\\f\|x<rsub|1>\|x<rsub|2>\<gtr\>

      \ \ x<rsub|3>

      \<less\>\|f\|x<rsub|4>\<gtr\>

      \ \ x<rsub|5>

      \<less\>/f\<gtr\>
    </verbatim>
  </tm-fragment>

  The escape sequences <verbatim|\\\<less\>less\\\<gtr\>>, <verbatim|\\\|>,
  <verbatim|\\\<less\>gtr\\\<gtr\>> and <verbatim|\\\\> may be used to
  represent the characters <verbatim|\<less\>>, <verbatim|\|>,
  <verbatim|\<gtr\>> and <verbatim|\\>. For instance,
  <math|\<alpha\>+\<beta\>> is serialized as
  <verbatim|\\\<less\>alpha\\\<gtr\>+\\\<less\>beta\\\<gtr\>>.

  <paragraph*|Formatting and whitespace>

  The <markup|document> and <markup|concat> primitives are serialized in a
  special way. The <markup|concat> primitive is serialized as usual
  concatenation. For instance, the text \Pan <em|important> note\Q is
  serialized as

  <\tm-fragment>
    <\verbatim>
      an \<less\>em\|important\<gtr\> note
    </verbatim>
  </tm-fragment>

  The <markup|document> tag is serialized by separating successive paragraphs
  by double newline characters. For instance, the quotation

  <\quote-env>
    <\dutch>
      Ik ben de blauwbilgorgel.

      Als ik niet wok of worgel,
    </dutch>
  </quote-env>

  is serialized as

  <\tm-fragment>
    <\verbatim>
      \<less\>\\quote-env\<gtr\>

      \ \ Ik ben de blauwbilgorgel.

      \;

      \ \ Als ik niet wok of worgel,

      \<less\>/quote-env\<gtr\>
    </verbatim>
  </tm-fragment>

  Notice that whitespace at the beginning and end of paragraphs is ignored.
  Inside paragraphs, any amount of whitespace is considered as a single
  space. Similarly, more than two newline characters are equivalent to two
  newline characters. For instance, the quotation might have been stored on
  disk as

  <\tm-fragment>
    <\verbatim>
      \<less\>\\quote-env\<gtr\>

      \ \ Ik ben de \ \ \ \ \ \ \ \ \ \ blauwbilgorgel.

      \;

      \;

      \ \ Als ik niet wok of \ \ \ \ \ \ \ \ \ worgel,

      \<less\>/quote-env\<gtr\>
    </verbatim>
  </tm-fragment>

  The space character may be explicitly represented through the escape
  sequence \P<verbatim|\\ >\Q. Empty paragraphs are represented using the
  escape sequence \P<verbatim|\\;>\Q.

  <paragraph*|Raw data>

  The <markup|raw-data> primitive is used inside <TeXmacs> for the
  representation of binary data, like image files included into the document.
  Such binary data is serialized as

  <\tm-fragment>
    <\verbatim>
      \<less\>#<em|binary-data>\<gtr\>
    </verbatim>
  </tm-fragment>

  where the <verbatim|<em|binary-data>> is a string of hexadecimal numbers
  which represents a string of bytes.

  <tmdoc-copyright|2016|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>