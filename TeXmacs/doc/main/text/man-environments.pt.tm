<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambientes>

  De forma análoga às etiquetas de conteúdo, ambientes são usados para marcar
  partes do texto com um significado especial. Entretanto, enquanto
  <apply|hyper-link|etiquetas de conteúdo|man-content-tags.pt.tm> normalmente
  são usadas para marcar pequenos trechos to texto, ambientes freqüentemente
  extendem-se por vários parágrafos. Algums ambientes usados comumente em
  matemática são <markup|teorema> e <markup|prova>, como nos exemplos abaixo:

  <\theorem>
    Não existem inteiros positivos <with|mode|math|a>, <with|mode|math|b>,
    <with|mode|math|c> e <with|mode|math|n> com
    <with|mode|math|n\<geqslant\>3>, tal que
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    Não há espaço aqui para escrever a prova.
  </proof>

  Você pode iniciar ambiente usando <apply|menu|Insert|Environment>. Outros
  ambientes com exibição similar aos teoremas são <markup|proposição>,
  <markup|lema>, <markup|corolário>, <markup|axioma>, <markup|definição>.
  Você pode usar o macro <markup|dueto> (o macro é ativado digitando <key|d u
  e t o return>) para registrar as pessoas às quais se deve o
  teorema, como em:

  <\theorem>
    <dueto|Pitágoras>Sob circustâncias apropriadas, nós temos
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Outros ambientes com exibição semelhante aos teoremas, mas que não
  enfatizam o texto incluso, são <markup|observação>, <markup|nota>,
  <markup|examplo>, <markup|aviso>, <markup|exercício> e <markup|problema>.
  Os ambiente remanescentes, <markup|literal>, <markup|codigo>,
  <markup|citar>, <markup|citação> e <markup|verso> \ podem ser usado para
  digitar texto ou programas com vários parágrafos, citações ou poesia.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
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
    <associate|language|portuguese>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-20|<tuple|2|?>>
    <associate|idx-10|<tuple|2|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|2|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|2|?>>
    <associate|idx-15|<tuple|2|?>>
    <associate|idx-16|<tuple|2|?>>
    <associate|idx-17|<tuple|2|?>>
    <associate|idx-18|<tuple|2|?>>
    <associate|idx-19|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|teorema>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|prova>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Ambiente>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proposição>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|lema>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corolário>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|axioma>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|definição>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|observação>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nota>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|examplo>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|aviso>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercício>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|problema>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|literal>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|codigo>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|citar>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|citação>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verso>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
