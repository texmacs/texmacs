<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Theorem-like environments>

  The <tmdtd|env-theorem> <abbr|d.t.d.> contains the default theorem-like and
  other textual environments, which are available through
  <menu|Text|Environment>. They are subdivided into three main categories:

  <\description>
    <item*|Variants of theorems>The bodies of theorem-like environments are
    usually emphasized. By default, the following such environments are
    available via <menu|Text|Environment>: <markup|theorem>,
    <markup|proposition>, <markup|lemma>, <markup|corollary>, <markup|axiom>,
    <markup|definition>, <markup|notation>, <markup|conjecture>.

    <item*|Variants of remarks>The following ones are available via
    <menu|Text|Environment>: <markup|remark>, <markup|example>,
    <markup|note>, <markup|warning>, <markup|convention>.

    <item*|Variants of exercises>Two such environments are provided by
    default and available via <menu|Text|Environment>: <markup|exercise> and
    <markup|problem>.
  </description>

  The environments are all available in unnumbered versions
  <markup|theorem*>, <markup|proposition*>, <abbr|etc.> as well. You may use
  <key|A-*> in order to switch between the unnumbered and numbered version.
  The following tags are also provided:

  <\explain|<explain-macro|proof|body>>
    For proofs of theorems.
  </explain>

  <\explain|<explain-macro|dueto|who>>
    An environment which can be used to specify the inventors of a theorem.
    It should be used at the start inside the body of a theorem, like in

    <\theorem*>
      <dueto|Pythagoras><with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
    </theorem*>
  </explain>

  The following customizable macros are used for the rendering of textual
  environments:

  <\explain|<explain-macro|render-theorem|name|body>>
    This macro is used for displaying a theorem-like environments. The first
    argument <src-arg|name> specifies the name of the theorem, like ``Theorem
    1.2'' and the second argument <src-arg|body> contains the body of the
    theorem. This environment is used for environments defined by
    <markup|new-theorem>.
  </explain>

  <\explain|<explain-macro|render-remark|name|body>>
    Similar to <markup|render-theorem>, but for remark-like environments.
  </explain>

  <\explain|<explain-macro|render-exercise|name|body>>
    Similar to <markup|render-theorem>, but for exercise-like environments.
  </explain>

  <\explain|<explain-macro|render-proof|name|body>>
    Similar to <markup|render-theorem>, but for proofs. This environment is
    mainly used for customizing the name of a proof, like in ``End of the
    proof of theorem 1.2''.
  </explain>

  Notice that you may also use these macros if you want an environment which
  is rendered in a similar way as a theorem, but with another name (like
  ``Corollary of Theorem 7'').

  The following tags can be used for further customization of the rendering:

  <\explain|<explain-macro|theorem-name|name>>
    This macro controls the appearance of the names of theorem-like <em|and>
    remark-like environments. Most styles use bold face or small capitals.
  </explain>

  <\explain|<explain-macro|exercise-name|name>>
    Similar to <markup|theorem-name>, but for exercises.
  </explain>

  <\explain|<explain-macro|theorem-sep>>
    The separator between the name of a theorem-like or remark-like
    environment and its main body. By default, this is a period followed by a
    space.
  </explain>

  <\explain|<explain-macro|exercise-sep>>
    Similar to <markup|theorem-sep>, but for exercises.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>