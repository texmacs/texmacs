<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Compilar una bibliografía>

  De momento <TeXmacs> usa <verbatim|bibtex> para compilar bibliografías. El
  mecanismo para hacerlo automáticamente es el siguiente:

  <\itemize>
    <item>Escriba un archivo <verbatim|.bib> con todas sus referencias
    bibliográficas. El archivo debería tener el formato de un fichero
    bibliográfico estándar para <LaTeX>.

    <item>Use <apply|menu|Insert|Link|Citation> e
    <apply|menu|Insert|Link|Invisible citation> para insertar citas, que
    corresponden a entradas en su fichero <verbatim|.bib>.

    <item>En el lugar donde su bibliografía debería estar compilada, utilice
    <apply|menu|Insert|Automatic|Bibliography>. En el panel de información se
    le pedirá un estilo <verbatim|bibtex> (como <verbatim|plain>,
    <verbatim|alpha>, <verbatim|abbrv>, etc.) y su archivo <verbatim|.bib>.

    <item>Use <apply|menu|Document|Update|Bibliography> a fin de compilar su
    bibliografía.
  </itemize>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|El permiso está garantizado para copiar, distribuir
  y/o modificar este documento bajo los terminos de la GNU Free Documentation
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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font
      family|<quote|ss>|Cita>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Cita
      invisible>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Automático>|<with|font
      family|<quote|ss>|Bibliografía>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Actualizar>|<with|font
      family|<quote|ss>|Bibliografía>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
