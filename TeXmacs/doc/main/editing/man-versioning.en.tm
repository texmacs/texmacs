<TeXmacs|1.0.7.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Versioning tools>

  When writing documents in collaboration with other authors, it frequently
  arises that one wants to go through changes made by the other authors, and
  either accept, discard or further correct them. After enabling the
  versioning tool through <menu|Edit|Preferences|Utilities|Versioning tool>,
  a<nbsp>special menu <menu|Version> appears in the main menu bar, which
  makes this process automatic. Let us describe in more detail how this tool
  works.

  For the moment, automatic version control systems such as <name|Subversion>
  are not yet supported. In the future, we intend to include support for such
  systems and the operation of merging two different new versions.

  <paragraph*|Comparing two versions>

  Assume that we have two versions <verbatim|old.tm> and <verbatim|new.tm> of
  the same document. In order to see the changes, first load the newest
  version <verbatim|new.tm>, then click on <menu|Version|File|Compare> and
  select the oldest version <verbatim|old.tm>. The buffer will still be named
  <verbatim|new.tm>, and the changes between both versions will be indicated
  by special markup. If there are any changes, then the cursor will be
  positioned at the first difference.

  It is possible to go through all the differences between the old and new
  versions either from the items in the submenu <menu|Version|Move>, or using
  the keyboard shortcuts <shortcut|(version-previous-difference)> and <shortcut|(version-next-difference)>. One may also use the
  more general structured navigation shortcuts <shortcut|(traverse-first)>, <shortcut|(traverse-last)>,
  <shortcut|(traverse-previous)> and <shortcut|(traverse-next)>.

  <paragraph*|Visualization of the differences>

  Differences between the two versions can be displayed in three ways: by
  showing only the old version, only the new version, or both versions
  simultaneously. In all cases, the old version is displayed in dark red and
  the new version in dark green.

  The visualization style can be specified individually for each individual
  change, via <menu|Version|Show> or the keyboard shortcuts <shortcut|(version-show 'version-old)> (old
  version), <shortcut|(version-show 'version-new)> (new version) and <shortcut|(version-show 'version-both)> (both versions). One
  may also cycle through the different style using the structured variant key
  <shortcut|(variant-circulate (focus-tree) #t)>. If you selected some text, then the above actions will apply
  to the whole selection. The visualization style may also be specified
  globally, using <menu|Version|File|Show old version>,
  <menu|Version|File|Show new version> and <menu|Version|File|Show both
  versions>.

  <paragraph*|Retaining a specific version>

  It often occurs that we want to go through the changes between two versions
  and progressively retain either one or the other version for each
  individual difference. Assuming that the cursor is inside a given
  difference, this can be done from entries in the submenu
  <menu|Version|Retain>. Alternatively, one may use the shortcuts <shortcut|(version-retain 0)>,
  <shortcut|(version-retain 1)> and <shortcut|(version-retain 'current)> to retain the old, new and currently displayed
  version, respectively. If both versions are displayed, then <shortcut|(version-retain 'current)>
  retains the new version. After retaining one of the versions, we
  automatically jump to the next difference, which can then be processed. If
  you selected some text, then any of the above action will retain the
  appropriate version for each of the differences in the selection.

  It is also possible to globally select the old, new or current version
  using <menu|Version|File|Retain old version>, <menu|Version|File|Retain new
  version>, <abbr|resp.> <menu|Version|File|Retain current version>. A
  convenient alternative way to process all differences is to use <shortcut|(version-previous-difference)>
  and <shortcut|(version-next-difference)> to go through the differences, use <shortcut|(version-show 'version-old)> and
  <shortcut|(version-show 'version-new)> to select the preferred version, and then click on
  <menu|Version|File|Retain current version> as soon as all differences have
  been processed.

  <paragraph*|Grain control and reactualizing the differences>

  The entries in the submenu <menu|Version|Grain> allow you to control the
  grain with which differences between versions are computed. By default, we
  use the finest grain <menu|Detailed>. It is also possible to compute
  differences on a paragraph-based level, using <menu|Block>. In that case,
  the entire paragraphs in which a change occurs will be highlighted. The
  roughest grain <menu|Rough> will highlight the entire text, if a change
  occurs within it.

  The grain is used when comparing two documents using
  <menu|Version|File|Compare>, but it is also possible to recompare the
  differences in a selected portion of text using <menu|Version|Reactualize>.
  The latter possibility is especially useful after a change the grain.

  Similarly, the cursor is inside a difference, then you may recompare the
  two versions <menu|Version|Reactualize>. This may be useful, if you made
  some changes to one of the versions. For instance, assume that the old
  version contained a theorem and that we changed it into a lemma in the new
  version and also modified part of its inside text. When visualizing the
  changes, the whole theorem will be highlighted, since there is no
  appropriate markup to indicate that we just changed from a<nbsp>theorem to
  a lemma. Nevertheless, if we want to compare the inside texts, we may turn
  the old theorem into a lemma and then click on <menu|Version|Reactualize>.

  <tmdoc-copyright|2010|Joris van der Hoeven>

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
  </collection>
</initial>