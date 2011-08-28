<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Keyboard (1.0.0.11 -- 1.0.1)>

  The <apply|TeXmacs> keybindings have been rationalized. Here follows a list
  of the major changes:

  <\itemize>
    <item>The <key|E-> prefix has been renamed to <prefix|M->.

    <item><key|escape> is equivalent to <prefix|M-> and
    <key|escape>-<key|escape> to <prefix|A->.

    <item>Mode dependent commands are now prefixed by <prefix|A->. In
    particular, accents are typed using <prefix|A-> instead of <key|E->.

    <item>Variants are now obtained using <key|tab> instead of
    <key|*> and you can circle back using
    <key|S-tab>.

    <item>Greek characters are now typed using <prefix|A-C->, <prefix|math:greek>, or the
    hyper modifier, which can be configured in
    <samp|Edit><with|mode|math|\<rightarrow\>><samp|Preferences>. You may
    also obtain Greek characters as variants of Latin characters. For
    instance, <key|p tab> yields <with|mode|math|\<pi\>>.

    <item>The signification of the cursor keys in combination with control,
    alt and meta has changed.
  </itemize>

  You may choose between several ``look and feels'' for the keyboard
  behaviour in <samp|Edit><with|mode|math|\<rightarrow\>><samp|Preferences><w\
  ith|mode|math|\<rightarrow\>><samp|Look and feel>. The default is
  <samp|Emacs>, but you may choose <samp|Old style> if you want to keep the
  behaviour to which you may be used now.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
