<TeXmacs|1.0.3.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Default environments>

  The <tmdtd|env-default> <abbr|d.t.d.> contains the default textual
  environments. They are subdivided into the following groups:

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

    <item*|Variants of figures>These environments always come by pairs: big
    and small ones. By default, we provide <markup|big-figure>,
    <markup|small-figure>, <markup|big-table> and <markup|small-table>. You
    can access them through <menu|Insert|Image> and <menu|Insert|Table>.
  </description>

  The above environments are all available in unnumbered versions
  <markup|theorem*>, <markup|proposition*>, <abbr|etc.> as well. We also
  recall that the above environments are based on the macros
  <markup|render-theorem>, <markup|render-remark>, <markup|render-exercise>,
  <markup|render-big-figure> and <markup|render-small-figure> for their
  rendering. Consequently, you may use <markup|render-theorem> if you want an
  environment which is rendered in a similar way as a theorem, but with
  another name (like ``Corollary of Theorem 7'').

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