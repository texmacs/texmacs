<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversión de documentos <TeXmacs> a Html>

  Hemos empezado a implementar la convsrsión entre HTML y <TeXmacs>. En este
  momento, sólamente es posible importar documentos HTML utilizando
  <apply|menu|File|Import|Html>. La mayoría de HTML 2.0 y parte de HTML 3.0
  está actualmente soportado. Sin embargo, todavía no se han añadido
  capacidades navigacionales. En el futuro implementaremos Math-ML.

  Cuando se importan documentos HTML los ficheros cuyos nombres comiencen con
  <verbatim|http:> o <verbatim|ftp:> se descargan de la red utilizando
  <verbatim|wget>. Si compiló <TeXmacs> usted mismo, entonces puedes
  descargar <verbatim|wget> de

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  En las distribuciones binarias <verbatim|wget> está incluído.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

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
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Importar>|<with|font
      family|<quote|ss>|Html>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Importar>|<with|font
      family|<quote|ss>|Html>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
