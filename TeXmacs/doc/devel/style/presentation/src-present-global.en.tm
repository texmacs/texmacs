<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Global presentation>

  In the <menu|Source tags> group of the <menu|Document|View> menu, you find
  several ways to customize the rendering of source trees in your document.
  We recommend you to play around with the different possibilities in a
  document of your own (after enabling <menu|Document|View|Source tree>) or a
  standard style package in <verbatim|$TEXMACS_PATH/packages>.

  First of all, you may choose between the different major styles
  ``angular'', ``scheme'', ``functional'' and ``<LaTeX>'' \ for rendering
  source trees, as illustrated in the figure below:

  <\big-figure>
    <\quote-env>
      <small|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|5|5|1|1|cell-hyphen|t>|<cwith|5|5|3|3|cell-hyphen|t>|<table|<row|<cell|<with|font-size|0.71|Angular>>|<cell|>|<cell|<with|font-size|0.71|Scheme>>>|<row|<\cell>
        <\with|src-style|angular>
          <\framed-fragment>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </framed-fragment>
        </with>
      </cell>|<cell|>|<\cell>
        <\with|src-style|scheme>
          <\framed-fragment>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </framed-fragment>
        </with>
      </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|Functional>>|<cell|>|<cell|<with|font-size|0.71|<LaTeX>>>>|<row|<\cell>
        <\with|src-style|functional>
          <\framed-fragment>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </framed-fragment>
        </with>
      </cell>|<cell|>|<\cell>
        <\with|src-style|latex>
          <\framed-fragment>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </framed-fragment>
        </with>
      </cell>>>>>>
    </quote-env>
  <|big-figure>
    Different styles for rendering the same source tree.
  </big-figure>

  Secondly, you may wish to reserve a special treatment to certain tags like
  <markup|concat> and <markup|document>. In the menu
  <menu|Document|View|Special> you may specify to which extent you want to
  treat such tags in a special way:

  <\description>
    <item*|None>No tags receive a special treatment.

    <item*|Formatting>Only the formatting tags <markup|concat> and
    <markup|document> are represented as usual.

    <item*|Normal>In addition to the formatting tags, a few other tags like
    <markup|compound>, <markup|value> and <markup|arg> are represented in a
    special way.

    <item*|Maximal>At the moment, this option is not yet implemented. The
    intention is to allow the user to write his own customizations and to
    allow for special rendering of basic operations like <markup|plus>.
  </description>

  These different options are illustrated below:

  <\big-figure>
    <\quote-env>
      <small|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|5|5|1|1|cell-hyphen|t>|<cwith|5|5|3|3|cell-hyphen|t>|<table|<row|<cell|<with|font-size|0.71|None>>|<cell|>|<cell|<with|font-size|0.71|Formatting>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-special|raw>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-special|format>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|Normal>>|<cell|>|<cell|<with|font-size|0.71|Maximal>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-special|normal>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-special|maximal>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>>>>>
    </quote-env>

    \;
  <|big-figure>
    Different ways to render special tags.
  </big-figure>

  Another thing which may be controlled by the user is whether the
  presentation of tags should be compact or stretched out across several
  lines. Several levels of compactification may be specified in the
  <menu|Document|View|Compactification> menu:

  <\description>
    <item*|Minimal>The tags are all stretched out across several lines.

    <item*|Only inline tags>All non-inline tags are stretched out across
    several lines.

    <item*|Normal>All inline arguments at the start of the tag are
    represented in a compact way. As soon as we encounter a block argument,
    the remainder of the arguments are stretched out across several lines.

    <item*|Inline arguments>All inline arguments are represented in a compact
    way and only block tags are stretched out across several lines.

    <item*|Maximal>All source code is represented in a compact way.
  </description>

  The ``normal'' and ``inline arguments'' options rarely differ. The visual
  effect of the different options is illustrated below:

  <\big-figure>
    <\quote-env>
      <small|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|5|5|1|1|cell-hyphen|t>|<cwith|5|5|3|3|cell-hyphen|t>|<table|<row|<cell|<with|font-size|0.71|Minimal>>|<cell|>|<cell|<with|font-size|0.71|Only
      inline tags>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-compact|none>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-compact|inline>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|Normal>>|<cell|>|<cell|<with|font-size|0.71|Maximal>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-compact|normal>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-compact|all>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>>>>>
    </quote-env>
  <|big-figure>
    Different levels of compactification.
  </big-figure>

  Finally, the user may specify the way closing tags should be rendered when
  the tag is stretched out across several lines. The rendering may either be
  minimalistic, compact, long, or recall the matching opening tag. The
  different options are illustrated below:

  <\big-figure>
    <\quote-env>
      <small|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|3|3|cell-hyphen|t>|<cwith|5|5|1|1|cell-hyphen|t>|<cwith|5|5|3|3|cell-hyphen|t>|<table|<row|<cell|<with|font-size|0.71|Minimal>>|<cell|>|<cell|<with|font-size|0.71|Compact>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-close|minimal>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-close|compact>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|Stretched>>|<cell|>|<cell|<with|font-size|0.71|Repeat>>>|<row|<\cell>
        <\framed-fragment>
          <\with|src-close|long>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>|<cell|>|<\cell>
        <\framed-fragment>
          <\with|src-close|repeat>
            <inactive*|<assign|quick-theorem|<\macro|body>
              <surround|<no-indent>Theorem. ||<arg|body>>
            </macro>>>
          </with>
        </framed-fragment>
      </cell>>>>>>
    </quote-env>
  <|big-figure>
    Different ways to render closing tags.
  </big-figure>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>