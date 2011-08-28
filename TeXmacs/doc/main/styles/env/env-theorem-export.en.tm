<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Using the theorem-like environments>

  The <tmdtd|env-theorem> <abbr|d.t.d.> contains the default theorem-like and
  other textual environments, which are available through
  <menu|Insert|Environment>. They are subdivided into three main categories:

  <\description>
    <item*|Variants of theorems>The bodies of theorem-like environments are
    usually emphasized. By default, the following such environments are
    available via <menu|Insert|Environment>: <markup|theorem>,
    <markup|proposition>, <markup|lemma>, <markup|corollary>, <markup|axiom>,
    <markup|definition>, <markup|notation>, <markup|conjecture>.

    <item*|Variants of remarks>The following ones are available via
    <menu|Insert|Environment>: <markup|remark>, <markup|example>,
    <markup|note>, <markup|warning>, <markup|convention>.

    <item*|Variants of exercises>Two such environments are provided by
    default and available via <menu|Insert|Environment>: <markup|exercise> and
    <markup|problem>.
  </description>

  The environments are all available in unnumbered versions
  <markup|theorem*>, <markup|proposition*>, <abbr|etc.> as well. You may use
  <shortcut|(numbered-toggle (focus-tree))> in order to switch between the unnumbered and numbered version.
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