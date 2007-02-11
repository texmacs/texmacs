<TeXmacs|1.0.5.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Type 1 fonts become the default (1.0.5.10)>

  From now on, <TeXmacs> uses Type 1 fonts by default, which enable you to
  generate higher quality <name|Pdf> files. The basic <TeXmacs> distribution
  (for <name|Unix>) comes with a minimal set of EC fonts for European
  languages, but an additional font package can be downloaded from our web
  site (the additional fonts are directly included in the <name|Windows>
  version). Whenever a given font is not available as a type 1 font, then
  <TeXmacs> falls back on <name|Metafont> in order to generate a Type<nbsp>3
  substitute. This behaviour can be further customized in
  <menu|Edit|Preferences|Printer|Font type>.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>