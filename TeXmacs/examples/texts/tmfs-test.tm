<TeXmacs|1.99.11>

<style|generic>

<\body>
  <section|Reproducing the bug>

  <\session|scheme|default>
    <\input|Scheme] >
      (define (string-repeat str n)

      \ \ (do ((i 1 (1+ i))

      \ \ \ \ \ \ \ (ret "" (string-append ret str)))

      \ \ \ \ \ \ ((\<gtr\> i n) ret)))
    </input>

    <\unfolded-io|Scheme] >
      (tmfs-load-handler (test num)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document ,(string-repeat "\\\\" (string-\<gtr\>number
      num))))))
    <|unfolded-io>
      #\<less\>procedure #f (num)\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (load-document "tmfs://test/1")
    <|unfolded-io>
      #f
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (load-document "tmfs://test/2")
    <|unfolded-io>
      #f
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (load-document "tmfs://test/3")
    <|unfolded-io>
      #f
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  The <scm|load-document> action should open a new <TeXmacs> buffer with
  several backward slashes. The number of backwark slashes is determined by
  the <verbatim|num> part of <verbatim|tmfs://test/num>.

  Without revision 12172 (or using GNU TeXmacs \<less\>= 1.99.11), it will
  not work as expected.

  <section|The internals>

  Let us dive into <scm|(load-document "tmfs://test/2")>. With revision
  \<gtr\>= 12197, we will open a new buffer with two backward slashes, but
  with revision \<less\> 12197, we will open a new buffer with a single
  backward slashes.

  The key of the bug is <scm|object-\<gtr\>tmstring>.

  <\scm-code>
    (define (object-\<gtr\>tmstring s) (unescape-guile (object-\<gtr\>string
    s)))

    \;

    (object-\<gtr\>tmstring `(document

    \ \ \ \ \ (TeXmacs ,(texmacs-version))

    \ \ \ \ \ (style (tuple "generic"))

    \ \ \ \ \ (body (document "\\\\\\\\"))))))
  </scm-code>

  Before revision 12172, the result of the conversion is

  <\verbatim-code>
    (document (TeXmacs "1.99.11") (style (tuple "generic")) (body (document
    "\\\\\\\\")))
  </verbatim-code>

  After revision 12172, the result of the conversion is

  <\verbatim-code>
    (document (TeXmacs "1.99.11") (style (tuple "generic")) (body (document
    "\\\\\\\\\\\\\\\\")))
  </verbatim-code>

  Notice, if you save a buffer with a single backward slash to
  <shell|xyz.stm> and view the file, you will get the following text:

  <\verbatim-code>
    (document (TeXmacs "1.99.11") (style (tuple "generic")) (body (document
    "\\\\\\\\")) (initial (collection)))
  </verbatim-code>

  In the stm format, a single backward slash is represented as four backward
  slashes. This is the root of the bug. The solution introduced by 12172 aims
  to minimize the changes to the code base. <scm|unescape-guile> is not
  frequently used. Changing the semantics of <scm|unescape-guile> only
  changes code about tmfs loading.

  \;

  \;

  \;
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Reproducing
      the bug> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>The
      internals> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>