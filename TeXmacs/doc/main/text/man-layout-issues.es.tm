<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Aspectos de presentación>

  Por regla general, <apply|TeXmacs> se ocupa de la presentación de su texto.
  Por lo tanto, aunque no queremos impedir esta posibilidad, no lo animamos a
  componer su documento visualmente. Por ejemplo, no debería insertar
  espacios o líneas en blanco como sustitutos del espacio vertical u
  horizontal entre palabras y líneas; en lugar de ello, el espacio adicional
  debería insertarse utilizando <apply|menu|Format|Space>. Esto hará su texto
  más robusto en el sentido de que no tendrá que reconsiderar la presentación
  cuando apliques pequeños cambios que afecten a los saltos de línea o
  página, o grandes cambios, como los de estilo del documento.

  Se han incorporado en <TeXmacs> diversos tipos de comandos explícitos de
  espaciado. Para empezar, puede insertar espacios rígidos de una anchura o
  altura dada. Los espacios horizontales no tienen altura y pueden ser
  elásticos o no. La longitud de un espacio elástico depende del modo de
  guionar el párrafo. Además se pueden insertar espacios tabulados. Los
  espacios verticales pueden insertarse o bien al comienzo o al final de un
  párrafo: el espacio vertical adicional entre dos párrafos es el máximo de
  entre el espacio vertical después del primero y el espacio vertical antes
  del segundo (en contraposición a <apply|TeX>, lo que impide que aparezca un
  espacio superfluo entre dos teoremas consecutivos).

  En lo referente a la presentación de los párrafos, puedes especificar el
  estilo de párrafo (justificado, alineado a la izquierda, centrado o
  alineado a la derecha), los márgenes del párrafo y el sangrado izquierdo
  (resp. derecho) de la primera (resp. última) línea de un párrafo. El
  usuario también controla los espacios entre párrafos y las líneas sucesivas
  en los párrafos.

  Puede especificar la presentación de la página en el menú
  <apply|menu|Document|Page>. En primer lugar, puede especificar el modo de
  mostrar páginas en la pantalla: al seleccionar ``papel'' como tipo de
  página en <apply|menu|Document|Page|Type>, puedes ver de modo explícito los
  saltos de página. Por defecto el tipo de página es ``papiro'', lo que evita
  los saltos de página en la presentación del documento. El tipo de página
  ``automático'' asume que tu tamaño de papel es exactamente del tamaño de tu
  ventana. Los márgenes de página y la anchura del texto se especifican en
  <apply|menu|Documento|Página|Presentación>. Habitualmente es conveniente
  reducir los márgenes de página para la edición en pantalla; esto se puede
  hacer en <apply|menu|Documento|Página|Presentación en pantalla>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Espacio>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font
      family|<quote|ss>|Tipo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font
      family|<quote|ss>|Presentación>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font family|<quote|ss>|Presentación en
      pantalla>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
