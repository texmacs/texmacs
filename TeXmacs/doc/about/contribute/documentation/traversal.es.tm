<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Recorriendo la documentación <TeXmacs>>

  Como una regla general, usted debe evitar el uso de comandos de sección
  dentro de la documentación de <TeXmacs> y tratar de escribir pequeñas
  páginas de ayuda sobre tópicos bien identificados. En una segunda etapa,
  usted debe escribir "meta archivos de ayuda" que indique como recorrer la
  documentación en una forma automática. Esto le permite el uso de una página
  de ayuda para diferentes propósitos (un manual impreso, un tutorial
  orientado a la Web, etc.).

  El estilo <tmstyle|tmdoc> provee tres macros de etiquetamiento para indicar
  cómo recorrer la documentación. El macro <markup|traverse> es usado para
  encapsular regiones con información transversal. El macro <markup|branch>
  indica una página de ayuda que debe ser considerada como una subsección y
  el macro <markup|continue> indica una página que sigue. Tanto el macro
  <markup|branch> como el <markup|continue> toman dos argumentos. El primer
  argumento describe el enlace y el segundo argumento da la dirección física
  relativa del archivo enlazado.

  Típicamente, al final de un meta archivo de ayuda usted encontrará varios
  macros <markup|branch> o <markup|continue>, dentro de un macro
  <markup|traverse>. En la parte superior del documento, usted debe
  espeficicar un título para su documento usando el macro
  <markup|tmdoc-title>. Cuanod se genera un manual impreso desde la
  documentación, una estructura capítulo-sección-subsección será
  automáticamente generada a partir de esta información y los títulos de los
  documentos. Alternaticamente, uno podría generar botones adicionales para
  la navegación dentro de la documentación usando un navegador.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir
  Luna Cárdenas>

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
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdoc-title>>|<pageref|idx-10>>
    </associate>
  </collection>
</auxiliary>
