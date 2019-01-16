<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Notes for users of oriental languages>

  In order to type oriental languages, you first have to start a conversion
  server which can be used in combination with the X input method and set the
  environment variables accordingly. For instance, in the case of Japanese,
  one typically has to execute the following shell commands:

  <\shell-code>
    kinput2 &

    export LANG="ja_JP.eucJP"

    export LC_ALL="ja_JP.eucJP"

    export XMODIFIERS="@im=kinput2"
  </shell-code>

  You also have to install Japanese fonts. For instance, you may download the
  <name|Ipag> fonts <verbatim|ipam.ttf>, <verbatim|ipag.ttf>,
  <verbatim|ipamp.ttf>, <verbatim|ipagm.ttf> and <verbatim|ipagui.ttf> and
  copy them to

  <verbatim| \ \ \ ~/.TeXmacs/fonts/truetype>

  After doing this, you may launch <TeXmacs> using

  <\shell-code>
    texmacs --delete-font-cache
  </shell-code>

  and select <menu|Japanese> from the <icon|tm_language.xpm> icon on the
  first icon bar. If everything went allright, the menus should now show up
  in Japanese and the current document is also in Japanese. Notice that you
  may also select Japanese as your default language in
  <menu|Edit|Preferences|Language|Japanese>. It is also possible to select
  Japanese for a portion of text in a document using
  <menu|Format|Language|Japanese>.

  Inside a Japanese portion of text, and depending on your input method, you
  usually have to type<nbsp><key|S-space> in order to start Kana to Kanji
  conversion. A small window shows up where you can type phonetic characters
  and use <key|space> in order to start conversion to Kanji characters. When
  pressing <shortcut|(kbd-return)>, the text is inserted into the main
  <TeXmacs> window. Pressing <key|S-space> once again returns to the
  classical <TeXmacs> input method.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>