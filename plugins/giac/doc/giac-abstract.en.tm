<TeXmacs|1.99.12>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|The <name|Giac> system>

  <hlink|<name|Giac>|http://www-fourier.ujf-grenoble.fr/~parisse/giac.html>
  Is A Computer algebra system. The system has been designed by <name|Bernard
  Parisse> and is under active development. It is a general-purpose
  computation kernel written in <abbr|C++> with the following features.

  <\description-dash>
    <item*|Algebra>Arbitrary precision integers, integer and polynomial
    arithmetic, Gröbner bases, simplification, equation solver, partial
    fraction decomposition, linear algebra (vectors, matrices, row reduction
    to echelon form, eigenvalues and eigenvectors), permutations,
    combinatorial analysis, computing in <math|\<bbb-Z\>/p*\<bbb-Z\>> and
    <math|\<bbb-Z\>/p*\<bbb-Z\><around*|[|x|]>>

    <item*|Calculus>Derivatives, integration, limits, series expansion,
    vector calculus, calculus of variations, implicit differentiation, curve
    interpolation, differential equations, special functions

    <item*|<abbr|2D> and <abbr|3D> plotting>Function graphs, surfaces,
    plotting implicit curves, plotting the solution of a system of
    inequalities, polar and parametric plots, scatter and line plots, bar
    plots, pie charts, histograms

    <item*|<abbr|2D> and <abbr|3D> geometry>Points, segments, lines,
    triangles, polygons, circles, conics, parametric curves, curve
    intersection, tangents, planes, surfaces

    <item*|Probability and statistics>Probability distributions, random
    variables, efficient sampling, maximum-likelihood fitting, statistical
    tests, kernel density estimation

    <item*|Signal processing and audio tools>Convolution, (auto)correlation,
    continuous/fast Fourier transform, Fourier series, filtering, windowing,
    loading, creating and saving audio clips, audio playback, resampling,
    noise removal, waveform plotting, spectrum plotting

    <item*|Optimization>Mixed integer linear programming, finding local and
    global extrema, nonlinear programming, transportation problem

    <item*|Graph theory>Creating (random) graphs and digraphs, operations on
    graphs, modifying graphs, importing and exporting graphs to
    <with|font-family|ss|dot> file format, examining properties of graphs,
    traversing graphs, vertex and edge coloring, graph isomorphism, graph
    drawing

    <item*|Programming>Functions, local variables, conditionals, loops,
    choice of syntax (C-like, Maple, Mupad, <abbr|TI89>)

    <item*|Syntax compatibility with other systems>Maple compatibility, MuPAD
    compatibility, Python compatibility, MathML export (content and
    presentation), <LaTeX> export
  </description-dash>

  <name|Giac> output is rendered in <TeXmacs> from its Scheme tree
  representation, which results in semantically correct formulas which can be
  copied back to <name|Giac>. Interactive plotting and mathematical input are
  supported as well.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven, 2020 by Luka Marohni¢>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>