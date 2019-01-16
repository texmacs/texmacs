<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Fundamental primitives>

  <\explain|<explain-macro|document|par-1|<math|\<cdots\>>|par-n><explain-synopsis|vertical
  sequence of paragraphs>>
    This primitive is used for sequences of logical paragraphs. A simple,
    plain text document is made of a sequence of paragraphs. For instance,

    <\tm-fragment>
      A simple document.

      Made of several paragraphs. The second paragraph is very long, so that
      it is hyphenated across several line.
    </tm-fragment>

    is internally represented as a <markup|document> with two subtrees:

    <\tm-fragment>
      <with|src-special|raw|<\inactive*>
        A simple document.

        Made of several paragraphs. The second paragraph is very long, so
        that it is hyphenated across several line.
      </inactive*>>
    </tm-fragment>

    From the visual point of view, different paragraphs are often separated
    by some vertical whitespace. Alternatively, new paragraphs are indicated
    through the use of an additional indentation. The root of a <TeXmacs>
    document is usually a <markup|document> node.

    The <markup|document> tag is also used for marking multi-paragraph
    content inside other tags, such lists or theorem-like environments.
    Environments which require the use of a <markup|document> tag for at
    least one argument are called ``block environments''.
  </explain>

  <\explain|<explain-macro|paragraph|unit-1|<math|\<cdots\>>|unit-n><explain-synopsis|vertical
  sequence of paragraph units>>
    This not yet implemented primitive is a variant of <markup|document>.
    While a document is made up of logical paragraphs, a paragraph is made up
    of ``paragraph units''. From a visual point of view, different paragraphs
    are singled out using some additional space or indentation. New paragraph
    units rather correspond to simple new lines. Typically, displayed
    equations are also paragraph units in a larger paragraph.
  </explain>

  <\explain|<explain-macro|concat|item-1|<math|\<cdots\>>|item-n><explain-synopsis|horizontal
  sequence of inline markup>>
    This primitive is used for sequences of line items, also called ``inline
    content''. For instance,

    <\tm-fragment>
      Some <em|emphasized> text.
    </tm-fragment>

    is internally represented as:

    <\tm-fragment>
      <with|src-special|raw|<inactive*|Some <em|emphasized> text.>>
    </tm-fragment>

    The <markup|concat> operator is essential to put compound structures in
    trees taking multiple parameters. For example, let us place the previous
    fragment in a multi-paragraph context:

    <\tm-fragment>
      Multiple paragraphs.

      Some <em|emphasized> text.
    </tm-fragment>

    In this example, we need the <markup|concat> tag in order to indicate
    that ``Some <em|emphasized> text.'' corresponds to a single paragraph:

    <\tm-fragment>
      <with|src-special|raw|<\inactive*>
        A simple document.

        Some <em|emphasized> text.
      </inactive*>>
    </tm-fragment>

    Notice that block tags like <markup|document> may contain inline tags
    such as <markup|concat> as its children, but not <em|vice versa>. In
    order to typeset line content before or after block content, one has to
    use the <markup|surround> tag below.
  </explain>

  <\explain>
    <explain-macro|surround|left|right|body><explain-synopsis|surround block
    content with inline content>
  <|explain>
    Although it is not possible in <TeXmacs> to use block content inside
    horizontal concatenations, it is sometimes useful to add some additional
    inline content before or after a block environment. The <markup|surround>
    primitive serves this purpose, by adding a <src-arg|left> and
    <src-arg|right> surrounding to some block content <src-arg|body>. For
    instance,

    <\tm-fragment>
      <\inactive*>
        <\surround|<active*|<math|<with|color|red|\<lightning\>>> >|>
          <\theorem>
            <active*|Given <math|P\<in\>\<bbb-T\><around|{|F|}>> and
            <math|f\<less\>g\<in\>\<bbb-T\>> with
            <math|P<around|(|f|)>*P<around|(|g|)>\<less\>0>, there exists an
            <math|h\<in\>\<bbb-T\>> with <math|P<around|(|h|)>=0>.>
          </theorem>
        </surround>
      </inactive*>
    </tm-fragment>

    produces

    <\tm-fragment>
      <\surround|<math|<with|color|red|\<lightning\>>> |>
        <\theorem>
          Given <math|P\<in\>\<bbb-T\><around|{|F|}>> and
          <math|f\<less\>g\<in\>\<bbb-T\>> with
          <math|P<around|(|f|)>*P<around|(|g|)>\<less\>0>, there exists an
          <math|h\<in\>\<bbb-T\>> with <math|P<around|(|h|)>=0>.
        </theorem>
      </surround>
    </tm-fragment>

    In general, the <markup|surround> is mainly used in style files, but it
    occasionally turns out to be useful in regular documents as well.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>