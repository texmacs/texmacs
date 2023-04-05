//*****************************************************************************
// MODULE     : scilab-demo.sce
// DESCRIPTION: Show a demo of some Scilab plugin features.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************

// print complex
(1 - 2*%i)^4

// print boolean matrix
rand(3,3) > 0.5

// print sparse matrix
sparse (rand(3,3) > 0.5)

// print matrix
A= [0,1;0,0], B= [1;1]; C= [1,1];

// print linear system
S1= syslin('c',A,B,C)  

// print rational
s1= ss2tf (S1)
h= s1/poly(1:4,'s','c')
f = (1 + %s) / (1 - %s)

// print (non Ascii) string
"degré"
M= ["une" "matrice"; "de" "caractère"]'
strcat(M, " ")

// print mlist
M=mlist(['V','name','value'],['a','b','c'],[1 2 3])

// overload V-list printing
function s= V2tmstree (arg)
  s= tmstree (arg.value) // generate stree
endfunction
// now, printing M print M.value
M

// print struct
s= struct ('month', 05, 'day', 31, 'year', 2013)
s.misc=s
s.misc=s

// print struct matrix
S= struct ('month', 05, 'day', 31, 'year', 2013)
S(2)= S
S(:,2)= S

S.month

// print cell
ce= cell ([2 2 2])
ce(2,1).entries= 0
ce(1,2).entries= S1
ce(2,2,2).entries= s
