<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Indentation primitives>

  There are two main ways to distinguish between successive paragraphs:
  separate them by a small vertical space, or use an indentation for each new
  paragraph. The indentation can be explicitly controlled using the
  <markup|no-indent>, <markup|yes-indent>, <markup|no-indent*> and
  <markup|yes-indent*> tags. The <markup|no-indent> and <markup|yes-indent>
  primitives apply to the current paragraph, while the <markup|no-indent*>
  and <markup|yes-indent*> apply the next paragraph.

  <\explain>
    <explain-macro|no-indent>

    <explain-macro|yes-indent>
  <|explain>
    Disable or enable indentation for the current paragraph. For instance,
    the code

    <\tm-fragment>
      <\inactive*>
        <no-indent>This is a long paragraph which demonstrates the disabling
        indentation using the <markup|no-indent> primitive.

        <yes-indent>This is a long paragraph which demonstrates enabling
        indentation using the <markup|yes-indent> primitive.
      </inactive*>
    </tm-fragment>

    typically produces

    <\tm-fragment>
      <\with|par-first|2fn>
        <no-indent>This is a long paragraph which demonstrates the disabling
        indentation using the <markup|no-indent> primitive.

        <yes-indent>This is a long paragraph which demonstrates enabling
        indentation using the <markup|yes-indent> primitive.
      </with>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|no-indent*>

    <explain-macro|yes-indent*>
  <|explain>
    Disable or enable indentation for the next paragraph. For instance,

    <\tm-fragment>
      <\inactive*>
        A first paragraph.<yes-indent*>

        A second paragraph.
      </inactive*>
    </tm-fragment>

    typically produces

    <\tm-fragment>
      <\with|par-first|2fn>
        A first paragraph.<yes-indent*>

        A second paragraph.
      </with>
    </tm-fragment>

    Notice that <markup|no-indent> and <markup|yes-indent> override
    <markup|no-indent*> and <markup|yes-indent*> directives in the previous
    paragraph.

    Currently, the <markup|no-indent*> and <markup|yes-indent*> tags are
    mainly used in order to control the indentation after section titles or
    environments like <markup|equation> which usually correspond to paragraph
    units. In the future, when sectional tags will take the section bodies as
    arguments, and when the <markup|paragraph> tag will be correctly
    implemented, the <markup|no-indent*> and <markup|yes-indent*> will become
    deprecated.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>