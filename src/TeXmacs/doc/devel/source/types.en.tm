<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Basic data types>

  In this chapter, we give a rough description of <apply|TeXmacs>'s basic
  data types in <verbatim|Basic>. The description of the exported functions
  is non exhaustive and we refer to the corresponding header files for more
  precision.

  <section|Memory allocation and data structures in TeXmacs>

  The file <verbatim|fast_alloc.hpp> declares the <apply|TeXmacs> memory
  allocation routines. These routines are very fast for small sizes, since
  for each such size, <apply|TeXmacs> maintains a linked list of freed
  objects of that size. No garbage collection has been implemented yet.

  Modulo a few exceptions, all <apply|TeXmacs> composite data structures are
  based on the modules <verbatim|concrete>, <verbatim|abstract>,
  <verbatim|concrete_null> and <verbatim|abstract_null>. Consequently, these
  data structures are pointers to representation classes, which may be
  abstract in the case of <verbatim|abstract> and <verbatim|abstract_null>,
  and which always contain a reference counter. Because of the reference
  counter, the C++ copy operator is very fast. Most of the implemented data
  structures also export a function <verbatim|copy>, which should be used if
  one really wants to physically duplicate an object,

  For classes constructed using <verbatim|concrete_null> or
  <verbatim|abstract_null>, the pointer to the representation class is
  allowed to be <verbatim|NULL> and we have a default constructor which
  initializes this pointer with <verbatim|NULL>. Instances of these classes
  are tested to be <verbatim|NULL> using the function <verbatim|nil>.
  Examples of such classes are lists, files and widgets.

  <section|Array-like structures>

  <apply|TeXmacs> implements three ``array-like'' structures:

  <\itemize>
    <item><verbatim|string> is the string type, which may contain '0'
    characters.

    <item><verbatim|tree> is the tree type with string labels.

    <item><verbatim|array\<less\>T\<gtr\>> is the generic array type with
    elements of type <verbatim|T>.
  </itemize>

  Array-like structures export the following operations:

  <\itemize>
    <item><verbatim|N> computes the length of an array.

    <item><verbatim|[]> accesses an element.

    <item><verbatim|\<less\>\<less\>> is used for appending elements or
    arrays.
  </itemize>

  For trees <verbatim|t>, we notice that <verbatim|t-\<gtr\>label> yields the
  label of the tree and <verbatim|t-\<gtr\>a> the array of its children. The
  second argument of <verbatim|\<less\>\<less\>> for trees is either a tree
  or an array of trees.

  The implementation has been made such that the <verbatim|\<less\>\<less\>>
  operation is fast, which is useful when considering arrays as buffers.
  Actually, the allocated space for arrays with more than five elements
  (words for strings) is always a power of two, so that new elements can be
  appended quickly. Notice that GNU malloc also always allocates blocks,
  whose sizes are powers of two. Therefore, we do not waste memory for small
  and large arrays.

  <section|Lists>

  Generic lists are implemented by the class <verbatim|list\<less\>T\<gtr\>>.
  The ``nil'' list is created using <verbatim|list\<less\>T\<gtr\>()>, an
  atom using <verbatim|list\<less\>T\<gtr\>(T x)> and a general list using
  <verbatim|list\<less\>T\<gtr\>(T x, list\<less\>T\<gtr\> next)>. If
  <verbatim|l> is a list, <verbatim|l-\<gtr\>item> and
  <verbatim|l-\<gtr\>next> correspond to its label and its successor
  respectively (<verbatim|car> and <verbatim|cdr> in lisp). The functions
  <verbatim|nil> and <verbatim|atom> tests whether a list is nil or an atom.
  The function <verbatim|N> computes the length of a list.

  The type <verbatim|list\<less\>T\<gtr\>> is also denoted by
  <verbatim|path>, because some additional functions are defined for it.
  Indeed, paths are used for accessing descendents in tree like structures.
  For instance, we implemented the function <verbatim|tree subtree (tree t,
  path p)>.

  <section|Hash tables>

  The <verbatim|hashmap\<less\>T,U\<gtr\>> class implements hash tables with
  entries in <verbatim|T> and values in <verbatim|U>. A function
  <verbatim|hash> should be implemented for <verbatim|T>. Given a hash table
  <verbatim|H>. We set elements through\ 

  <\verbatim>
    \ \ \ \ H(x)=y;
  </verbatim>

  and access to elements through\ 

  <\verbatim>
    \ \ \ \ H[x]
  </verbatim>

  We also implemented a variant <verbatim|rel_hashmap\<less\>T,U\<gtr\>> of
  hash tables, which also have a list-like structure, which makes them useful
  for implementing recursive environments.

  <section|Other data structures>

  <\itemize>
    <item><verbatim|command> implements abstract commands.

    <item><verbatim|file> implements files.

    <item><verbatim|iterator\<less\>T\<gtr\>> implements generic iterators.

    <item><verbatim|rectangles> implements rectangles and lists of
    rectangles.

    <item><verbatim|space> implements stretchable spaces.

    <item><verbatim|timer> implements timers.
  </itemize>

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
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      Memory allocation and data structures in
      TeXmacs<value|toc-dots><pageref|toc-1>

      Array-like structures<value|toc-dots><pageref|toc-2>

      Lists<value|toc-dots><pageref|toc-3>

      Hash tables<value|toc-dots><pageref|toc-4>

      Other data structures<value|toc-dots><pageref|toc-5>
    </associate>
  </collection>
</auxiliary>
