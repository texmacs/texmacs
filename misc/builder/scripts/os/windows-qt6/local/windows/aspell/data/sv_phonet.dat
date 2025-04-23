# vim:ts=20:
# swedish_phonet.dat - Swedish phonetic transformation rules for aspell
# Copyright (C) 2000  Martin Norbäck  <d95mback@dtek.chalmers.se>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
# USA.
#
# $Id$
#
# ChangeLog:
# 2001-01-25: 0.2 total rewrite
# 2000-02-24: 0.1 initial version
#
# See end of file for description (in Swedish)

version 0.2

&	&
@	@
ANG(EIYÄÖ)-^	ANI	# förledet an- ska inte bli @-ljud
AGNO6	AKNO	# agnostiker osv.
AG(IE)-6	AK	# vokal+g(ie) ger ej j-ljud
A	A
BB-<	_
B	P
CCO-	K	# broccoli, piccolo
CC	KS	# successiv, access, succé
CH	&	# choklad osv.
CK	K	# ck -> k som vanligt
C(EIYÄÖ)-<	S	# c + mjuk vokal -> s
C	K	# c + annat -> k
DJ(UÄÖ)-	I	# djungel, djävel, adjö
DD-	_
D	T
EG(IE)-6	EK	# vokal+g(ie) ger ej j-ljud
E	E
É	E
FF	F
F	F
G(EIYÄÖ)-3	I	# g+mjuk vokal ger j-ljud
GG6	K
GN	@N	# ugn, lugn...
G	K
H(AOUÅEIYÄÖ)-^	H	# h i början av ord hörs
H(AUOÅEIYÄÖ)-	_	# annars stumt framför vokal
HJ	I	# hj->j (hjärta osv.)
H	H
IG(IE)-6	IK	# vokal+g(ie) ger ej j-ljud
I	I
J	I
K(EIYÄÖ)-^	&	# k+mjuk vokal ger sje-ljud
KJ	&	# kjol
K	K
LJU-	I	# ljuga, ljus
LL-	_
L	L
MM-	_
M	M
NG6	@
NN-	_
N	N
ORIGI8	ORKI	# specialfall
OG(IE)-6	OK	# vokal+g(ie) ger ej j-ljud
O	O
PROJEKT	PRO&EKT	# specialfall
PSALT<	SALT	# specialfall
PSALM<	SALM	# specialfall
PP-	_
P	P
Q	K
RGI$6	RGI	# inget j i slutet
RGA$6	RIA	# här är det däremot j (arga,färga)
RGE$6	RIE	# här också (Norge, överge)
RGS$	RIS	# rgs i slutet
RG$	RI	# rg i slutet
RD	T	# "blött" d
RN	N	# "blött" n
RT	T	# "blött" t
RLD	T	# värld
RL	L	# "blött" l
RS	&	# sje-ljud (fars, gärsgård)
RR-	_
R	R
SS-	_
SCHIZ6	SKITS	# specialfall
SCH6	&
SKJ	&	# skjorta, skjuta
SJ	&	# sje-ljud
S	S
TION9^	TION	# tionde
TION6	&ON	# station osv.
TT-	_
T	T
UG(IE)-6	UK	# vokal+g(ie) ger ej j-ljud
U	U
V	F
W	F
X9	KS
YG(IE)-6	YK	# vokal+g(ie) ger ej j-ljud
Y	I
ZZ	TS
Z	S
ÅTTIO9	OTIO	# specialfall (ej sje-ljud)
ÅRTION9	ORTION	# specialfall (r hörs)
ÅG(IE)-6	OK	# vokal+g(ie) ger ej j-ljud
Å	O
ÄG(IE)-6	EK	# vokal+g(ie) ger ej j-ljud
Ä	E
ÖG(IE)-6	ÖK	# vokal+g(ie) ger ej j-ljud
Ö	Ö

# Detta är en lista med transformationer för att bättre kunna föreslå
# ersättningsord till felstavade svenska ord.
#
# När man stavar fel på grund av att man inte vet stavningen (till
# skillnad från slarvfel), så skriver man ofta något som skulle uttalas
# på samma sätt.
#
# För att kunna föreslå bättre så gör aspell om ordet till en
# (ungefärlig) fonetisk beskrivning med hjälp av dessa regler.
#
# Det fungerar så här:
#
# Ord som finns i ordlistan transformeras till sin fonetiska form
# När ett ord som inte finns med i ordlistan påträffas transformeras det
# till sin fonetiska form och jämförs med de ord som finns i ordlistan.
# Det är därför viktigt att ord som finns i ordlistan transformeras
# rätt, annars kommer de aldrig att föreslås rätt.
#
# Följande fonem finns:
# A kort och långt a-ljud
# E kort och långt e- och ä-ljud
# F f- och v-ljud
# H tonande h (tonlöst h noteras inte)
# I kort och långt i- och y-ljud, j-ljud
# K g- och k-ljud
# L l-ljud (även "blött" som farlig och kärl)
# M m-ljud
# N n-ljud (även "blött" som barn och torn)
# O kort och långt o- och å-ljud
# P b- och p-ljud
# R r-ljud
# S s-ljud
# T d- och t-ljud (även "blött", som hård och hårt)
# U u-ljud
# V v-ljud
# @ ng-ljud
# & sje-ljud
#
# Så här funkar transformeringsreglerna i korthet:
# * Det som står till vänster transformeras till det till höger
# * Står det ett eller flera - betyder det att de sista bokstäverna inte
#   översätts, utan används nästa gång
# * Står det en siffra betyder det att den transformeringen har en viss
#   prioritet (normal prioritet är 5), ju högre desto starkare
# * Inom varje initialbokstavsgrupp tilldelas prioritet uppifrån och ner
#
# För mer information om processen och om aspell, se aspell.sourceforge.org
# Kapitel 5.3 i bruksanvisningen handlar om detta
