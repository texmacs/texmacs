<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Porting <TeXmacs> to other platforms>

  Having only access to PC/Linux and SUN systems, I am interested in people
  who want to port <apply|TeXmacs> to other Unix systems with X Window and to
  maintain the corresponding distributions. If you want to do this, you
  should take a look at the files\ 

  <\verbatim>
    \ \ \ \ configure.in<format|next line> \ \ \ src/Basic/fast_alloc.cpp
  </verbatim>

  Specialists on <verbatim|autoconf>, redhat and rpm packages are also
  welcome to communicate their suggestions, patches, etc.

  Besides porting <apply|TeXmacs> to other Unix-based systems, it would be
  nice to port <apply|TeXmacs> to Windows (and Mac OS). Please join the
  <verbatim|texmacs-dev@gnu.org> mailing list if you want to help.
  Discussions have been going on about how to do the porting and in
  particular about which portable graphical user interface (like Gtk, Qt,
  Wxwindows or GNUstep) we should use. Our strategy will be to first put all
  GUI dependent code in a cleanly specified TMGUI API and then do the actual
  porting. In fact, this will allow us to support multiple graphical
  toolkits. More details can be found in the archives of the
  <verbatim|texmacs-dev@gnu.org> mailing list.

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
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>
