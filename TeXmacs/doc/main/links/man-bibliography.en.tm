<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Compiling a bibliography>

  <subsubsection*|Editing files with bibliographic entries>

  <TeXmacs> uses the <BibTeX> model for its bibliographies. Manuals about
  <BibTeX> can easily be found at various places on the web. <BibTeX> files
  can either be entered and edited using <TeXmacs> itself or using an
  external tool. Some external tools offer possibilities to search and
  retrieve bibliographic entries on the web, which can be a reason to prefer
  such tools from time to time. <TeXmacs> implements good converters for
  <BibTeX> files, so several editors can easily be used in conjunction.

  The built-in editor for <BibTeX> files is automatically used for files with
  the <verbatim|.bib> extension. New items can easily be added using
  <menu|Insert|Database entry>. When creating a new entry, required fields
  appear in dark blue, alternative fields in dark green and optional fields
  in light blue. The special field inside the header of your entry is the
  name of your entry, which will be used later for references to this entry.
  When editing a field, you may use <shortcut|(kbd-return)> to confirm it and
  jump to the next one (blank optional fields will automatically be removed
  when doing this). When the cursor is inside a bibliographic entry,
  additional fields may also be added using <menu|Focus|Insert above> and
  <menu|Focus|Insert below>.

  <BibTeX> contains a few unnatural conventions for entering names of authors
  and managing capitalization inside titles. When editing <BibTeX> files
  using <TeXmacs>, these conventions are replaced by the following more user
  friendly conventions:

  <\itemize>
    <item>When entering authors (inside ``Author'' or ``Editor'' fields), use
    the <markup|name> tag for specifying last names (using <menu|Insert|Last
    name> or <shortcut|(make 'name)>) For instance, ``Albert Einstein''
    should be entered as ``Albert <name|Einstein>'' or as ``A.
    <name|Einstein>''. Special particles such as ``von'' can be entered using
    <menu|Insert|Particle>. Title suffices such as ``Jr.'' can be entered
    similarly using <menu|Insert|Title suffix>.

    <item>When entering titles, do not capitalize, except for the first
    character and names or concepts that always must be. For instance, use
    ``Riemannian geometry'' instead of ``Riemannian Geometry'' and
    ``Differential Galois theory'' instead of ``Differential Galois Theory''.
  </itemize>

  <subsubsection*|Inserting citations and compiling bibliographies>

  Assuming that you have created a <verbatim|.bib> file with your
  bibliographic references, the mechanism to automatically compile a
  bibliography is the following:

  <\itemize>
    <item>Use <menu|Insert|Link|Citation> and <menu|Insert|Link|Invisible
    citation> to insert citations, which correspond to entries in your
    <verbatim|.bib> file.

    <item>At the place where your bibliography should be compiled, click on
    <menu|Insert|Automatic|Bibliography>. At the prompt, you should enter a
    <verbatim|bibtex> style (such as <verbatim|plain>, <verbatim|alpha>,
    <verbatim|abbrv>, etc.) and your <verbatim|.bib> file.

    <item>Use <menu|Document|Update|Bibliography> in order to compile your
    bibliography.
  </itemize>

  Notice that additional BiB<TeX> styles should be put in the directory
  <verbatim|~/.TeXmacs/system/bib>.

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>