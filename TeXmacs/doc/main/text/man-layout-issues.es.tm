<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Aspectos de presentaci�n>

  Por regla general, <apply|TeXmacs> se ocupa de la presentaci�n de su texto.
  Por lo tanto, aunque no queremos impedir esta posibilidad, no lo animamos a
  componer su documento visualmente. Por ejemplo, no deber�a insertar
  espacios o l�neas en blanco como sustitutos del espacio vertical u
  horizontal entre palabras y l�neas; en lugar de ello, el espacio adicional
  deber�a insertarse utilizando <apply|menu|Format|Space>. Esto har� su texto
  m�s robusto en el sentido de que no tendr� que reconsiderar la presentaci�n
  cuando apliques peque�os cambios que afecten a los saltos de l�nea o
  p�gina, o grandes cambios, como los de estilo del documento.

  Se han incorporado en <TeXmacs> diversos tipos de comandos expl�citos de
  espaciado. Para empezar, puede insertar espacios r�gidos de una anchura o
  altura dada. Los espacios horizontales no tienen altura y pueden ser
  el�sticos o no. La longitud de un espacio el�stico depende del modo de
  guionar el p�rrafo. Adem�s se pueden insertar espacios tabulados. Los
  espacios verticales pueden insertarse o bien al comienzo o al final de un
  p�rrafo: el espacio vertical adicional entre dos p�rrafos es el m�ximo de
  entre el espacio vertical despu�s del primero y el espacio vertical antes
  del segundo (en contraposici�n a <apply|TeX>, lo que impide que aparezca un
  espacio superfluo entre dos teoremas consecutivos).

  En lo referente a la presentaci�n de los p�rrafos, puedes especificar el
  estilo de p�rrafo (justificado, alineado a la izquierda, centrado o
  alineado a la derecha), los m�rgenes del p�rrafo y el sangrado izquierdo
  (resp. derecho) de la primera (resp. �ltima) l�nea de un p�rrafo. El
  usuario tambi�n controla los espacios entre p�rrafos y las l�neas sucesivas
  en los p�rrafos.

  Puede especificar la presentaci�n de la p�gina en el men�
  <apply|menu|Document|Page>. En primer lugar, puede especificar el modo de
  mostrar p�ginas en la pantalla: al seleccionar ``papel'' como tipo de
  p�gina en <apply|menu|Document|Page|Type>, puedes ver de modo expl�cito los
  saltos de p�gina. Por defecto el tipo de p�gina es ``papiro'', lo que evita
  los saltos de p�gina en la presentaci�n del documento. El tipo de p�gina
  ``autom�tico'' asume que tu tama�o de papel es exactamente del tama�o de tu
  ventana. Los m�rgenes de p�gina y la anchura del texto se especifican en
  <apply|menu|Documento|P�gina|Presentaci�n>. Habitualmente es conveniente
  reducir los m�rgenes de p�gina para la edici�n en pantalla; esto se puede
  hacer en <apply|menu|Documento|P�gina|Presentaci�n en pantalla>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|�lvaro Cantero
  Tejero|Pablo Ruiz M�zquiz|David Moriano Garcia|Offray Vladimir Luna
  C�rdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

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
      family|<quote|ss>|P�gina>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font
      family|<quote|ss>|Tipo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font
      family|<quote|ss>|Presentaci�n>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font family|<quote|ss>|Presentaci�n en
      pantalla>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
