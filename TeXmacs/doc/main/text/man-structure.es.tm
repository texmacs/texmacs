<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Tecleando texto estructurado>

  Usualmente, los documentos largos tienen estructura: están organizados en
  capítulos, secciones y subsecciones, contienen diferentes tipos de texto
  tales como, texto normal, citas, notas al pie, teoremas, etc. Después de
  seleccionar un <em|estilo de documento> en <apply|menu|Document|Style>,
  <TeXmacs> se encarga de los detalles esquemáticos, tales como, numeración
  de secciones, páginas, teoremas, composición tipográfica de citas y notas
  al pie en forma adecuada.

  Actualmente existen cuatro estilos estándar de documento: carta, artículo,
  libro y seminario. El estilo seminario se utiliza para hacer
  transparencias. Tan pronto como haya escogido uno de estos estilos podrá
  organizar su texto en secciones (vea <apply|menu|Insert|Section>) y usar
  <em|entornos> específicos. Teorema, proposición, comentario, etc. son
  ejemplos de diversos entornos disponibles (vea
  <apply|menu|Insert|Environment>). Otros ejemplos son listas de items (vea
  <apply|menu|Insert|Itemize>) y listas numeradas (vea
  <apply|menu|Insert|Enumerate>).

  Cuando esté más familiarizado con <apply|TeXmacs>, es posible adicionar sus
  propios entornos nuevos en su propio archivo de estilos. \ Imagine por
  ejemplo que suele hacer citas y quiere que éstas aparezcan en cursiva, con
  márgenes izquierdo y derecho de 1cm. En lugar de cambiar manualmente las
  propiedades del texto y del párrafo cada vez que hace una cita, es mejor
  crear un entorno de cita. No sólo será más rápido crear citas cuando se
  haga esto, sino que además es posible cambiar sistemáticamente el esquema
  de sus citas a lo largo del documento, sólo cambiando la definición del
  entorno de cita. Esta última situación ocurre por ejemplo si descubre
  <with|font shape|italic|a posteriori> que prefiere que las citas aparezcan
  en una fuente más pequeña.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas|Álvaro Cantero Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Sección>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Entorno>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Bolos>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Lista numerada>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
