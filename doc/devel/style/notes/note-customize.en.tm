<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing arbitrary tags>

  Imagine that you want to change the rendering of a given tag, like
  <markup|lemma>. As a general rule, <TeXmacs> provides a set of well-chosen
  macros which can be customized by the user so as to obtain the desired
  effect. For instance, as we have seen <hyper-link|above|../customize/customize-theorems.en.tm>,
  you should use modify one of the macros <markup|render-theorem>,
  <markup|theorem-name> or <markup|theorem-sep> in order to customize the
  rendering of <markup|lemma> and all other theorem-like environments.

  However, in some cases, it may not be clear which ``well-chosen'' macro to
  customize. If we just wanted to change the presentation of lemmas and not
  of any other theorem-like environments, then we clearly cannot modify
  <markup|render-theorem>, <markup|theorem-name> or <markup|theorem-sep>. In
  other cases, the user may not want to invest his time in completely
  understanding the macro hierarchy of <TeXmacs>, and find out about the
  existence of <markup|render-theorem>, <markup|theorem-name> and
  <markup|theorem-sep>.

  So imagine that you want all lemmas to appear in red. One thing \ you can
  always do is copy the original definition of lemmas in a safe place and
  redefine the lemma macro on top of the original definition:

  <\tm-fragment>
    <\inactive*>
      <assign|orig-lemma|<value|lemma>>

      <assign|lemma|<macro|body|<with|color|red|<orig-lemma|<arg|body>>>>>
    </inactive*>
  </tm-fragment>

  Alternatively, if only the text inside the lemma should be rendered in red,
  then you may do:

  <\tm-fragment>
    <\inactive*>
      <assign|orig-lemma|<value|lemma>>

      <assign|lemma|<macro|body|<orig-lemma|<with|color|red|<arg|body>>>>>
    </inactive*>
  </tm-fragment>

  Of course, you have to be careful that the name <markup|orig-lemma> is not
  already in use.

  Another frequent situation is that you only want to modify the rendering of
  a tag when it is used inside another one. On the web, the <em|Cascading
  Style Sheet> language (<acronym|CSS>) provides a mechanism for doing this.
  In <TeXmacs>, you may simulate this behaviour by redefining macros inside a
  <markup|with>. For instance, imagine that we want the inter-paragraph space
  inside lists inside theorem-like environments to vanish. Then we may use:

  <\tm-fragment>
    <\inactive*>
      <assign|orig-render-theorem|<value|render-theorem>>

      <assign|render-theorem|<macro|name|body|<with|orig-render-list|<value|render-list>|<with|render-list|<macro|x|<with|par-par-sep|0fn|<orig-render-list|<arg|x>>>>|<style-with|src-compact|none|<orig-render-theorem|<arg|name>|<arg|body>>>>>>>
    </inactive*>
  </tm-fragment>

  On the one hand side, this mechanism is a bit more complex than
  <acronym|CSS>, where it suffices to respecify the <src-var|par-par-sep>
  attribute of lists inside theorems. On the other hand, it is also more
  powerful, since the <markup|render-theorem> macro applies to all
  theorem-like environments at once. Furthermore, if the above mechanism is
  to be used frequently, then real hackers may simplify the notations using
  further macro magic.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>