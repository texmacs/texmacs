<TeXmacs|2.1.5>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|The <name|LanguageTool> plug-in>

  <name|<hlink|LanguageTool|https://languagetool.org/>> is a grammar checker.
  <em|Its support within <TeXmacs> is still experimental.> You may enable it
  via <menu|Edit|Preferences|Other|Experimental features|grammar checking>.

  LanguageTool is configured via <menu|Insert|Session|Preferences|Languagetool>.
  The default server is <verbatim|http://localhost:8081>, which assumes that
  your computer runs its own instance of the LanguageTool server. This server
  may be installed for <name|GNU/Linux>, <name|Windows>, and <name|macOS>
  operating systems.

  The server can also be set to <slink|https://api.languagetool.org>, but
  then, beware that the documents you check are entirely sent to this server.
  Premium accounts are supported.

  Grammar can be checked for a whole document or a selected region via
  <menu|Edit|Check grammar>. If <menu|Insert|Session|Preferences|Languagetool|Use
  widgets> is selected, then a standalone or toolbar widget is created;
  Clicking on <icon|tm_close_tool.xpm> terminates corrections. Otherwise,
  clicking on an error creates a popup window that indicates possible
  corrections; To end corrections, you may use <menu|Edit|Terminate grammar>
  (this removes all the remaining error tags inserted in the document).

  <tmdoc-copyright|2026|Grégoire Lecerf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
  </collection>
</initial>