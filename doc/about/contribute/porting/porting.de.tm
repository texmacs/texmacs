<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs> auf andere Plattformen portieren>

  Da wir nur Zugang zu PC/Linux und SUN-Systemen haben, sind wir daran
  interessiert, jemanden zu finden, der \ <TeXmacs> auf andere UNIX-Systeme
  mit X portiert und die resultierenden Distributionen pflegt. Wenn Sie das
  tun wollen, sollten Sie einen Blick auf

  <\verbatim>
    \ \ \ \ configure.in<next-line> \ \ \ src/System/Misc/fast_alloc.cpp
  </verbatim>

  werfen. Spezialisten für <verbatim|autoconf>, redhat und rpm Paketen sind
  auch herzlich willkommen, Anregungen, Patches usw. zu übermitteln.\ 

  Neben der Portierung von <TeXmacs> auf andere UNIX-Plattformen würde es
  schön sein, wenn \ <TeXmacs> auf Windows und Mac OS übertragen würde. Bitte
  treten Sie der <verbatim|texmacs-dev@gnu.org> Mailing-Liste bei, wenn Sie
  helfen wollen. Diskussionen laufen über das Vorgehen bei der Portierung und
  insbesondere darüber, welche graphische Oberfläche (wie z.B. Gtk, Qt,
  Wxwindows oder GNUstep) benutzt werden sollte. Unsere Strategie ist,
  zunächst einmal allen GUI-abhängigen Code ein ein spezielles TMGUI API
  einzubringen und dann die eigentliche Übertragung zu machen. Diese
  Vorgehensweise sollte es ermöglichen, unterschiedliche graphische
  \RToolkits`` zu benutzen. Mehr finden Sie in der Mailing-Liste:
  <verbatim|texmacs-dev@gnu.org>.

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
    <associate|language|german>
  </collection>
</initial>