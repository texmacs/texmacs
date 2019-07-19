<TeXmacs|1.99.10>

<style|<tuple|tmdoc|chinese|old-spacing>>

<\body>
  <tmdoc-title|\<#5B9A\>\<#5236\>\<#52A8\>\<#6001\>\<#83DC\>\<#5355\>>

  \<#5B9A\>\<#4E49\>\<#4E00\>\<#4E2A\>\<#540D\>\<#79F0\>\<#4E3A\><scm-arg|name>\<#7684\>\<#83DC\>\<#5355\>\<#FF0C\>\<#53EF\>\<#7528\>

  <\scm-code>
    (menu-bind <scm-arg|name> . <scm-arg|def>)
  </scm-code>

  \<#6216\>\<#8005\>

  <\scm-code>
    (tm-menu (<scm-arg|name>) . <scm-arg|def>)
  </scm-code>

  \<#5176\>\<#4E2D\> <scm-arg|def> \<#662F\>\<#4E00\>\<#4E2A\>\<#5448\>\<#73B0\>\<#6240\>\<#6709\>\<#83DC\>\<#5355\>\<#6761\>\<#76EE\>\<#7684\>\<#7A0B\>\<#5E8F\>\<#3002\>\<#4E0B\>\<#9762\>\<#8FD9\>\<#4E2A\>\<#76EE\>\<#5F55\>\<#4E0B\>\<#7684\>\<#6587\>\<#4EF6\>\<#53EF\>\<#4F9B\>\<#53C2\>\<#8003\>\<#FF1A\>

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  \<#5176\>\<#4E2D\>\<#5305\>\<#542B\>\<#4E86\>\<#6807\>\<#51C6\><TeXmacs>\<#83DC\>\<#5355\>\<#7684\>\<#5B9A\>\<#4E49\>\<#65B9\>\<#5F0F\>\<#3002\>\<#5BF9\>\<#4E8E\><scm|tm-menu>\<#FF0C\>\<#4F60\>\<#8FD8\>\<#53EF\>\<#4EE5\>\<#6307\>\<#5B9A\>\<#989D\>\<#5916\>\<#7684\>\<#53C2\>\<#6570\>\<#FF0C\>\<#4F7F\>\<#4E4B\>\<#6839\>\<#636E\>\<#53C2\>\<#6570\>\<#52A8\>
  \<#6001\>\<#6784\>\<#5EFA\>\<#66F4\>\<#52A0\>\<#590D\>\<#6742\>\<#7684\>\<#83DC\>\<#5355\>\<#3002\>

  \<#66F4\>\<#7CBE\>\<#786E\>\<#5730\>\<#8BF4\>\<#FF0C\>\<#5728\><scm|menu-bind>\<#6216\>\<#8005\><scm|tm-menu>\<#4E2D\>\<#7684\><verbatim|<em|def>>\<#7A0B\>\<#5E8F\>\<#5B9E\>\<#9645\>\<#4E0A\>\<#662F\>\<#5982\>\<#4E0B\>\<#6240\>\<#793A\>\<#5F62\>\<#5F0F\>\<#7684\>\<#7A0B\>\<#5E8F\>\<#4EE3\>\<#7801\>\<#FF1A\>

  <\scm-code>
    (=\<gtr\> "pulldown menu name" <scm-arg|menu-definition>)

    (-\<gtr\> "pullright menu name" <scm-arg|menu-definition>)

    ("entry" <scm-arg|action>)

    ---

    (if <scm-arg|condition> <scm-arg|menu-definition>)

    (link <scm-arg|variable>)
  </scm-code>

  \<#51FD\>\<#6570\><scm|=\<gtr\>>\<#548C\><scm|-\<gtr\>>\<#7528\>\<#6765\>\<#521B\>\<#5EFA\>\<#4E0B\>\<#62C9\>\<#6216\>\<#8005\>\<#53F3\>\<#62C9\>\<#83DC\>\<#5355\>\<#FF0C\><scm-arg|menu-definition>\<#4E2D\>\<#8981\>\<#5305\>\<#542B\>\<#521B\>\<#5EFA\>\<#5B50\>\<#83DC\>\<#5355\>\<#7684\>\<#7A0B\>\<#5E8F\>\<#3002\>\<#4F7F\>\<#7528\>\<#51FD\>\<#6570\><scm|("entry"
  <scm-arg|action>)>\<#53EF\>\<#521B\>\<#5EFA\>\<#4E00\>\<#4E2A\>\<#83DC\>\<#5355\>\<#6761\>\<#76EE\>\<#FF0C\>\<#70B9\>\<#51FB\><scm|entry>\<#5C31\>\<#4F1A\>\<#6267\>\<#884C\><scm-arg|action>\<#3002\>\<#83DC\>\<#5355\>\<#9879\>\<#4E4B\>\<#95F4\>\<#4F7F\>\<#7528\><scm|--->\<#5206\>\<#5272\>\<#3002\>\<#51FD\>\<#6570\>
  <scm|if> \<#7528\>\<#4E8E\>\<#5728\>\<#6EE1\>\<#8DB3\>\<#7279\>\<#5B9A\>\<#7684\>
  <scm|condition> \<#65F6\>\<#63D2\>\<#5165\>\<#83DC\>\<#5355\>\<#9879\>\<#3002\>\<#FF08\>\<#6BD4\>\<#5982\>\<#8BF4\>\<#5728\>\<#6570\>\<#5B66\>\<#6A21\>\<#5F0F\>\<#4E2D\>\<#FF09\>

  \<#5982\>\<#679C\>\<#4F60\>\<#58F0\>\<#660E\>\<#4E86\>\<#4E00\>\<#4E2A\>\<#83DC\>\<#5355\>
  <scm|name> \<#FF0C\>\<#90A3\>\<#4E48\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>\<#51FD\>\<#6570\>
  <scm|link> \<#95F4\>\<#63A5\>\<#5F15\>\<#7528\>\<#8BE5\>\<#83DC\>\<#5355\>\<#3002\>\<#8FD9\>\<#79CD\>\<#95F4\>\<#63A5\>\<#58F0\>\<#660E\>\<#5B50\>\<#83DC\>\<#5355\>\<#7684\>\<#65B9\>\<#5F0F\>\<#6709\>\<#4E24\>\<#4E2A\>\<#4F18\>\<#52BF\>\<#FF1A\>

  <\itemize>
    <item>\P\<#95F4\>\<#63A5\>\Q\<#5B50\>\<#83DC\>\<#5355\>\<#53EF\>\<#4EE5\>\<#94FE\>\<#63A5\>\<#5230\>\<#6211\>\<#4EEC\>\<#6240\>\<#9700\>\<#7684\>\<#83DC\>\<#5355\>\<#FF0C\>\<#65E0\>\<#8BBA\>\<#591A\>\<#5C11\>

    <item>\<#4F7F\>\<#7528\><scm|menu-append>\<#53EF\>\<#4EE5\>\<#540E\>\<#7EED\>\<#6DFB\>\<#52A0\>\<#65B0\>\<#6761\>\<#76EE\>\<#5230\>\Q\<#95F4\>\<#63A5\>\Q\<#5B50\>\<#83DC\>\<#5355\>\<#4E2D\>
  </itemize>

  \<#4E3B\>\<#8981\>\<#7684\><TeXmacs>\<#83DC\>\<#5355\>\<#662F\><scm|texmacs-menu>\<#FF0C\><scm|texmacs-popup-menu>\<#FF0C\><scm|texmacs-main-icons>\<#FF0C\>
  <scm|texmacs-mode-icons>\<#FF0C\> <scm|texmacs-focus-icons>\<#548C\><scm|texmacs-extra-icons>\<#3002\>\<#5176\>\<#4ED6\>\<#4E00\>\<#4E9B\>\<#6807\>\<#51C6\>\<#7684\>\<#95F4\>\<#63A5\>\<#83DC\>\<#5355\>\<#662F\><scm|file-menu>\<#FF0C\>
  <scm|edit-menu>\<#FF0C\> <scm|insert-menu>\<#FF0C\>
  <scm|text-menu>\<#FF0C\> <scm|paragraph-menu>\<#FF0C\>
  <scm|document-menu>\<#FF0C\> <scm|options-menu>\<#548C\><scm|help-menu>\<#3002\>

  <tmdoc-copyright|2013--2019|Joris van der Hoeven|\<#6C88\>\<#8FBE\>>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>