/* classes: h_files */

#ifndef SCM_TAGS_H
#define SCM_TAGS_H

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2008
 * Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



/** This file defines the format of SCM values and cons pairs.
 ** It is here that tag bits are assigned for various purposes.
 **/

/* picks up scmconfig.h too */
#include "libguile/__scm.h"

#if HAVE_INTTYPES_H
# include <inttypes.h>  /* for INTPTR_MAX and friends */
#else
# if HAVE_STDINT_H
#  include <stdint.h>   /* for INTPTR_MAX and friends */
# endif
#endif



/* In the beginning was the Word:
 *
 * For the representation of scheme objects and their handling, Guile provides
 * two types:  scm_t_bits and SCM.
 *
 * - scm_t_bits values can hold bit patterns of non-objects and objects:
 *
 *   Non-objects -- in this case the value may not be changed into a SCM value
 *   in any way.
 *
 *   Objects -- in this case the value may be changed into a SCM value using
 *   the SCM_PACK macro.
 *
 * - SCM values can hold proper scheme objects only.  They can be changed into
 *   a scm_t_bits value using the SCM_UNPACK macro.
 *
 * When working in the domain of scm_t_bits values, programmers must keep
 * track of any scm_t_bits value they create that is not a proper scheme
 * object.  This makes sure that in the domain of SCM values developers can
 * rely on the fact that they are dealing with proper scheme objects only.
 * Thus, the distinction between scm_t_bits and SCM values helps to identify
 * those parts of the code where special care has to be taken not to create
 * bad SCM values.
 */

/* For dealing with the bit level representation of scheme objects we define
 * scm_t_bits:
 */
/* On Solaris 7 and 8, /usr/include/sys/int_limits.h defines
   INTPTR_MAX and UINTPTR_MAX to empty, INTPTR_MIN is not defined.
   To avoid uintptr_t and intptr_t in this case we require
   UINTPTR_MAX-0 != 0 etc.  */
#if SCM_SIZEOF_INTPTR_T != 0 && defined(INTPTR_MAX) && defined(INTPTR_MIN) \
  && INTPTR_MAX-0 != 0 && INTPTR_MIN-0 != 0 \
  && SCM_SIZEOF_UINTPTR_T != 0 && defined(UINTPTR_MAX) && UINTPTR_MAX-0 != 0

typedef intptr_t scm_t_signed_bits;
#define SCM_T_SIGNED_BITS_MAX INTPTR_MAX
#define SCM_T_SIGNED_BITS_MIN INTPTR_MIN
typedef uintptr_t scm_t_bits;
#define SIZEOF_SCM_T_BITS SCM_SIZEOF_UINTPTR_T
#define SCM_T_BITS_MAX UINTPTR_MAX

#else

typedef signed long scm_t_signed_bits;
#define SCM_T_SIGNED_BITS_MAX LONG_MAX
#define SCM_T_SIGNED_BITS_MIN LONG_MIN
typedef unsigned long scm_t_bits;
#define SIZEOF_SCM_T_BITS SCM_SIZEOF_UNSIGNED_LONG
#define SCM_T_BITS_MAX ULONG_MAX

#endif

/* But as external interface, we define SCM, which may, according to the
 * desired level of type checking, be defined in several ways:
 */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
    typedef union { struct { scm_t_bits n; } n; } SCM;
    static SCM scm_pack(scm_t_bits b) { SCM s; s.n.n = b; return s; }
#   define SCM_UNPACK(x) ((x).n.n)
#   define SCM_PACK(x) (scm_pack ((scm_t_bits) (x)))
#elif (SCM_DEBUG_TYPING_STRICTNESS == 1)
/* This is the default, which provides an intermediate level of compile time
 * type checking while still resulting in very efficient code.
 */
    typedef struct scm_unused_struct * SCM;

/*
  The 0?: constructions makes sure that the code is never executed,
  and that there is no performance hit.  However, the alternative is
  compiled, and does generate a warning when used with the wrong
  pointer type.

  The Tru64 and ia64-hp-hpux11.23 compilers fail on `case (0?0=0:x)'
  statements, so for them type-checking is disabled.  */
#if defined __DECC || defined __HP_cc
#   define SCM_UNPACK(x) ((scm_t_bits) (x))
#else
#   define SCM_UNPACK(x) ((scm_t_bits) (0? (*(SCM*)0=(x)): x))
#endif

/*
  There is no typechecking on SCM_PACK, since all kinds of types
  (unsigned long, void*) go in SCM_PACK
 */
#   define SCM_PACK(x) ((SCM) (x))

#else
/* This should be used as a fall back solution for machines on which casting
 * to a pointer may lead to loss of bit information, e. g. in the three least
 * significant bits.
 */
    typedef scm_t_bits SCM;
#   define SCM_UNPACK(x) (x)
#   define SCM_PACK(x) ((SCM) (x))
#endif


/* SCM values can not be compared by using the operator ==.  Use the following
 * macro instead, which is the equivalent of the scheme predicate 'eq?'.
 */
#define scm_is_eq(x, y) (SCM_UNPACK (x) == SCM_UNPACK (y))



/* Representation of scheme objects:
 *
 * Guile's type system is designed to work on systems where scm_t_bits and SCM
 * variables consist of at least 32 bits.  The objects that a SCM variable can
 * represent belong to one of the following two major categories:
 *
 * - Immediates -- meaning that the SCM variable contains an entire Scheme
 *   object.  That means, all the object's data (including the type tagging
 *   information that is required to identify the object's type) must fit into
 *   32 bits.
 *
 * - Non-immediates -- meaning that the SCM variable holds a pointer into the
 *   heap of cells (see below).  On systems where a pointer needs more than 32
 *   bits this means that scm_t_bits and SCM variables need to be large enough
 *   to hold such pointers.  In contrast to immediates, the object's data of
 *   a non-immediate can consume arbitrary amounts of memory: The heap cell
 *   being pointed to consists of at least two scm_t_bits variables and thus
 *   can be used to hold pointers to malloc'ed memory of any size.
 *
 * The 'heap' is the memory area that is under control of Guile's garbage
 * collector.  It holds 'single-cells' or 'double-cells', which consist of
 * either two or four scm_t_bits variables, respectively.  It is guaranteed
 * that the address of a cell on the heap is 8-byte aligned.  That is, since
 * non-immediates hold a cell address, the three least significant bits of a
 * non-immediate can be used to store additional information.  The bits are
 * used to store information about the object's type and thus are called
 * tc3-bits, where tc stands for type-code.  
 *
 * For a given SCM value, the distinction whether it holds an immediate or
 * non-immediate object is based on the tc3-bits (see above) of its scm_t_bits
 * equivalent: If the tc3-bits equal #b000, then the SCM value holds a
 * non-immediate, and the scm_t_bits variable's value is just the pointer to
 * the heap cell.
 *
 * Summarized, the data of a scheme object that is represented by a SCM
 * variable consists of a) the SCM variable itself, b) in case of
 * non-immediates the data of the single-cell or double-cell the SCM object
 * points to, c) in case of non-immediates potentially additional data outside
 * of the heap (like for example malloc'ed data), and d) in case of
 * non-immediates potentially additional data inside of the heap, since data
 * stored in b) and c) may hold references to other cells.
 *
 *
 * Immediates
 *
 * Operations on immediate objects can typically be processed faster than on
 * non-immediates.  The reason is that the object's data can be extracted
 * directly from the SCM variable (or rather a corresponding scm_t_bits
 * variable), instead of having to perform additional memory accesses to
 * obtain the object's data from the heap.  In order to get the best possible
 * performance frequently used data types should be realized as immediates.
 * This is, as has been mentioned above, only possible if the objects can be
 * represented with 32 bits (including type tagging).
 *
 * In Guile, the following data types and special objects are realized as
 * immediates: booleans, characters, small integers (see below), the empty
 * list, the end of file object, the 'unspecified' object (which is delivered
 * as a return value by functions for which the return value is unspecified),
 * a 'nil' object used in the elisp-compatibility mode and certain other
 * 'special' objects which are only used internally in Guile.
 *
 * Integers in Guile can be arbitrarily large.  On the other hand, integers
 * are one of the most frequently used data types.  Especially integers with
 * less than 32 bits are commonly used.  Thus, internally and transparently
 * for application code guile distinguishes between small and large integers.
 * Whether an integer is a large or a small integer depends on the number of
 * bits needed to represent its value.  Small integers are those which can be
 * represented as immediates.  Since they don't require more than a fixed
 * number of bits for their representation, they are also known as 'fixnums'.
 *
 * The tc3-combinations #b010 and #b110 are used to represent small integers,
 * which allows to use the most significant bit of the tc3-bits to be part of
 * the integer value being represented.  This means that all integers with up
 * to 30 bits (including one bit for the sign) can be represented as
 * immediates.  On systems where SCM and scm_t_bits variables hold more than
 * 32 bits, the amount of bits usable for small integers will even be larger.
 * The tc3-code #b100 is shared among booleans, characters and the other
 * special objects listed above.
 *
 *
 * Non-Immediates
 *
 * All object types not mentioned above in the list of immedate objects are
 * represented as non-immediates.  Whether a non-immediate scheme object is
 * represented by a single-cell or a double-cell depends on the object's type,
 * namely on the set of attributes that have to be stored with objects of that
 * type.  Every non-immediate type is allowed to define its own layout and
 * interpretation of the data stored in its cell (with some restrictions, see
 * below).
 *
 * One of the design goals of guile's type system is to make it possible to
 * store a scheme pair with as little memory usage as possible.  The minimum
 * amount of memory that is required to store two scheme objects (car and cdr
 * of a pair) is the amount of memory required by two scm_t_bits or SCM
 * variables.  Therefore pairs in guile are stored in single-cells.
 *
 * Another design goal for the type system is to store procedure objects
 * created by lambda expresssions (closures) and class instances (goops
 * objects) with as little memory usage as possible.  Closures are represented
 * by a reference to the function code and a reference to the closure's
 * environment.  Class instances are represented by a reference to the
 * instance's class definition and a reference to the instance's data.  Thus,
 * closures as well as class instances also can be stored in single-cells.
 *
 * Certain other non-immediate types also store their data in single-cells.
 * By design decision, the heap is split into areas for single-cells and
 * double-cells, but not into areas for single-cells-holding-pairs and areas
 * for single-cells-holding-non-pairs.  Any single-cell on the heap therefore
 * can hold pairs (consisting of two scm_t_bits variables representing two
 * scheme objects - the car and cdr of the pair) and non-pairs (consisting of
 * two scm_t_bits variables that hold bit patterns as defined by the layout of
 * the corresponding object's type).
 *
 *
 * Garbage collection
 *
 * During garbage collection, unreachable cells on the heap will be freed.
 * That is, the garbage collector will detect cells which have no SCM variable
 * pointing towards them.  In order to properly release all memory belonging
 * to the object to which a cell belongs, the gc needs to be able to interpret
 * the cell contents in the correct way.  That means that the gc needs to be
 * able to determine the object type associated with a cell only from the cell
 * itself.
 *
 * Consequently, if the gc detects an unreachable single-cell, those two
 * scm_t_bits variables must provide enough information to determine whether
 * they belong to a pair (i. e. both scm_t_bits variables represent valid
 * scheme objects), to a closure, a class instance or if they belong to any
 * other non-immediate.  Guile's type system is designed to make it possible
 * to determine a the type to which a cell belongs in the majority of cases
 * from the cell's first scm_t_bits variable.  (Given a SCM variable X holding
 * a non-immediate object, the macro SCM_CELL_TYPE(X) will deliver the
 * corresponding cell's first scm_t_bits variable.)
 *
 * If the cell holds a scheme pair, then we already know that the first
 * scm_t_bits variable of the cell will hold a scheme object with one of the
 * following tc3-codes: #b000 (non-immediate), #b010 (small integer), #b100
 * (small integer), #b110 (non-integer immediate).  All these tc3-codes have
 * in common, that their least significant bit is #b0.  This fact is used by
 * the garbage collector to identify cells that hold pairs.  The remaining
 * tc3-codes are assigned as follows: #b001 (class instance or, more
 * precisely, a struct, of which a class instance is a special case), #b011
 * (closure), #b101/#b111 (all remaining non-immediate types).
 *
 *
 * Summary of type codes of scheme objects (SCM variables)
 *
 * Here is a summary of tagging bits as they might occur in a scheme object.
 * The notation is as follows: tc stands for type code as before, tc<n> with n
 * being a number indicates a type code formed by the n least significant bits
 * of the SCM variables corresponding scm_t_bits value.
 *
 * Note that (as has been explained above) tc1==1 can only occur in the first
 * scm_t_bits variable of a cell belonging to a non-immediate object that is
 * not a pair.  For an explanation of the tc tags with tc1==1, see the next
 * section with the summary of the type codes on the heap.
 *
 * tc1:
 *   0:  For scheme objects, tc1==0 must be fulfilled.
 *  (1:  This can never be the case for a scheme object.)
 *
 * tc2:
 *   00:  Either a non-immediate or some non-integer immediate
 *  (01:  This can never be the case for a scheme object.)
 *   10:  Small integer
 *  (11:  This can never be the case for a scheme object.)
 *
 * tc3:
 *   000:  a non-immediate object (pair, closure, class instance etc.)
 *  (001:  This can never be the case for a scheme object.)
 *   010:  an even small integer (least significant bit is 0).
 *  (011:  This can never be the case for a scheme object.)
 *   100:  Non-integer immediate
 *  (101:  This can never be the case for a scheme object.)
 *   110:  an odd small integer (least significant bit is 1).
 *  (111:  This can never be the case for a scheme object.)
 *
 * The remaining bits of the non-immediate objects form the pointer to the
 * heap cell.  The remaining bits of the small integers form the integer's
 * value and sign.  Thus, the only scheme objects for which a further
 * subdivision is of interest are the ones with tc3==100.
 *
 * tc8 (for objects with tc3==100):
 *   00000-100:  special objects ('flags')
 *   00001-100:  characters
 *   00010-100:  evaluator byte codes ('isyms')
 *   00011-100:  evaluator byte codes ('ilocs')
 *
 *
 * Summary of type codes on the heap
 *
 * Here is a summary of tagging in scm_t_bits values as they might occur in
 * the first scm_t_bits variable of a heap cell.
 *
 * tc1:
 *   0:  the cell belongs to a pair.
 *   1:  the cell belongs to a non-pair.
 *
 * tc2:
 *   00:  the cell belongs to a pair with no short integer in its car.
 *   01:  the cell belongs to a non-pair (struct or some other non-immediate).
 *   10:  the cell belongs to a pair with a short integer in its car.
 *   11:  the cell belongs to a non-pair (closure or some other non-immediate).
 *
 * tc3:
 *   000:  the cell belongs to a pair with a non-immediate in its car.
 *   001:  the cell belongs to a struct
 *   010:  the cell belongs to a pair with an even short integer in its car.
 *   011:  the cell belongs to a closure
 *   100:  the cell belongs to a pair with a non-integer immediate in its car.
 *   101:  the cell belongs to some other non-immediate.
 *   110:  the cell belongs to a pair with an odd short integer in its car.
 *   111:  the cell belongs to some other non-immediate.
 *
 * tc7 (for tc3==1x1):
 *   See below for the list of types.  Note the special case of scm_tc7_vector
 *   and scm_tc7_wvect:  vectors and weak vectors are treated the same in many
 *   cases.  Thus, their tc7-codes are chosen to only differ in one bit.  This
 *   makes it possible to check an object at the same time for being a vector
 *   or a weak vector by comparing its tc7 code with that bit masked (using
 *   the TYP7S macro).  Three more special tc7-codes are of interest:
 *   numbers, ports and smobs in fact each represent collections of types,
 *   which are subdivided using tc16-codes.
 *
 * tc16 (for tc7==scm_tc7_smob):
 *   The largest part of the space of smob types is not subdivided in a
 *   predefined way, since smobs can be added arbitrarily by user C code.
 *   However, while Guile also defines a number of smob types throughout,
 *   there is one smob type, namely scm_tc_free_cell, for which Guile assumes
 *   that it is declared first and thus gets a known-in-advance tc16-code.
 *   The reason of requiring a fixed tc16-code for this type is performance.
 */



/* Checking if a SCM variable holds an immediate or a non-immediate object:
 * This check can either be performed by checking for tc3==000 or tc3==00x,
 * since for a SCM variable it is known that tc1==0.  */
#define SCM_IMP(x) 		(6 & SCM_UNPACK (x))
#define SCM_NIMP(x) 		(!SCM_IMP (x))

/* Checking if a SCM variable holds an immediate integer: See numbers.h for
 * the definition of the following macros: SCM_I_FIXNUM_BIT,
 * SCM_MOST_POSITIVE_FIXNUM, SCM_I_INUMP, SCM_I_MAKINUM, SCM_I_INUM.  */

/* Checking if a SCM variable holds a pair (for historical reasons, in Guile
 * also known as a cons-cell): This is done by first checking that the SCM
 * variable holds a non-immediate, and second, by checking that tc1==0 holds
 * for the SCM_CELL_TYPE of the SCM variable.  
*/

#define SCM_I_CONSP(x)  (!SCM_IMP (x) && ((1 & SCM_CELL_TYPE (x)) == 0))



/* Definitions for tc2: */

#define scm_tc2_int              2


/* Definitions for tc3: */

#define SCM_ITAG3(x) 		 (7 & SCM_UNPACK (x))
#define SCM_TYP3(x) 		 (7 & SCM_CELL_TYPE (x))

#define scm_tc3_cons	 	 0
#define scm_tc3_struct    	 1
#define scm_tc3_int_1		 (scm_tc2_int + 0)
#define scm_tc3_closure		 3
#define scm_tc3_imm24		 4
#define scm_tc3_tc7_1		 5
#define scm_tc3_int_2		 (scm_tc2_int + 4)
#define scm_tc3_tc7_2		 7


/* Definitions for tc7: */

#define SCM_ITAG7(x) 		(127 & SCM_UNPACK (x))
#define SCM_TYP7(x) 		(0x7f &        SCM_CELL_TYPE (x))
#define SCM_TYP7S(x) 		((0x7f & ~2) & SCM_CELL_TYPE (x))

#define scm_tc7_symbol		5
#define scm_tc7_variable        7

/* couple */
#define scm_tc7_vector		13
#define scm_tc7_wvect		15

#define scm_tc7_string		21
#define scm_tc7_number		23
#define scm_tc7_stringbuf       39

/* Many of the following should be turned
 * into structs or smobs.  We need back some
 * of these 7 bit tags!  */

#define scm_tc7_pws		31

#define scm_tc7_unused_1        29
#define scm_tc7_unused_2	37
#define scm_tc7_unused_3	45
#define scm_tc7_unused_4	47
#define scm_tc7_unused_5	53
#define scm_tc7_unused_6	55
#define scm_tc7_unused_7	71
#define scm_tc7_unused_8	77
#define scm_tc7_unused_9	79

#define scm_tc7_dsubr		61
#define scm_tc7_cclo		63
#define scm_tc7_rpsubr		69
#define scm_tc7_subr_0		85
#define scm_tc7_subr_1		87
#define scm_tc7_cxr		93
#define scm_tc7_subr_3		95
#define scm_tc7_subr_2		101
#define scm_tc7_asubr		103
#define scm_tc7_subr_1o		109
#define scm_tc7_subr_2o		111
#define scm_tc7_lsubr_2		117
#define scm_tc7_lsubr		119

/* There are 256 port subtypes.  */
#define scm_tc7_port		125

/* There are 256 smob subtypes.  [**] If you change scm_tc7_smob, you must
 * also change the places it is hard coded in this file and possibly others.
 * Dirk:FIXME:: Any hard coded reference to scm_tc7_smob must be replaced by a
 * symbolic reference.  */
#define scm_tc7_smob		127 /* DO NOT CHANGE [**] */


/* Definitions for tc16: */
#define SCM_TYP16(x) 		(0xffff & SCM_CELL_TYPE (x))
#define SCM_TYP16_PREDICATE(tag, x) (!SCM_IMP (x) && SCM_TYP16 (x) == (tag))


/* Here is the first smob subtype.  */

/* scm_tc_free_cell is the 0th smob type.  We place this in free cells to tell
 * the conservative marker not to trace it.  */
#define scm_tc_free_cell	(scm_tc7_smob + 0 * 256L)



/* {Immediate Values}
 */

enum scm_tc8_tags
{
  scm_tc8_flag = scm_tc3_imm24 + 0x00,  /* special objects ('flags') */
  scm_tc8_char = scm_tc3_imm24 + 0x08,  /* characters */
  scm_tc8_isym = scm_tc3_imm24 + 0x10,  /* evaluator byte codes ('isyms') */
  scm_tc8_iloc = scm_tc3_imm24 + 0x18   /* evaluator byte codes ('ilocs') */
};

#define SCM_ITAG8(X)		(SCM_UNPACK (X) & 0xff)
#define SCM_MAKE_ITAG8(X, TAG)	SCM_PACK (((X) << 8) + TAG)
#define SCM_ITAG8_DATA(X)	(SCM_UNPACK (X) >> 8)



/* Flags (special objects).  The indices of the flags must agree with the
 * declarations in print.c: iflagnames.  */

#define SCM_IFLAGP(n)    (SCM_ITAG8 (n) == scm_tc8_flag)
#define SCM_MAKIFLAG(n)  SCM_MAKE_ITAG8 ((n), scm_tc8_flag)
#define SCM_IFLAGNUM(n)  (SCM_ITAG8_DATA (n))

#define SCM_BOOL_F		SCM_MAKIFLAG (0)
#define SCM_BOOL_T 		SCM_MAKIFLAG (1)
#define SCM_UNDEFINED	 	SCM_MAKIFLAG (2)
#define SCM_EOF_VAL 		SCM_MAKIFLAG (3)
#define SCM_EOL			SCM_MAKIFLAG (4)
#define SCM_UNSPECIFIED		SCM_MAKIFLAG (5)

/* When a variable is unbound this is marked by the SCM_UNDEFINED
 * value.  The following is an unbound value which can be handled on
 * the Scheme level, i.e., it can be stored in and retrieved from a
 * Scheme variable.  This value is only intended to mark an unbound
 * slot in GOOPS.  It is needed now, but we should probably rewrite
 * the code which handles this value in C so that SCM_UNDEFINED can be
 * used instead.  It is not ideal to let this kind of unique and
 * strange values loose on the Scheme level.  */
#define SCM_UNBOUND		SCM_MAKIFLAG (6)

/* The Elisp nil value.  */
#define SCM_ELISP_NIL		SCM_MAKIFLAG (7)


#define SCM_UNBNDP(x)		(scm_is_eq ((x), SCM_UNDEFINED))



/* Evaluator byte codes ('immediate symbols').  These constants are used only
 * in eval but their values have to be allocated here.  The indices of the
 * SCM_IM_ symbols must agree with the declarations in print.c:
 * scm_isymnames.  */

#define SCM_ISYMP(n) 		(SCM_ITAG8 (n) == scm_tc8_isym)
#define SCM_MAKISYM(n) 		SCM_MAKE_ITAG8 ((n), scm_tc8_isym)

#define SCM_IM_AND              SCM_MAKISYM (0)
#define SCM_IM_BEGIN            SCM_MAKISYM (1)
#define SCM_IM_CASE             SCM_MAKISYM (2)
#define SCM_IM_COND             SCM_MAKISYM (3)
#define SCM_IM_DO               SCM_MAKISYM (4)
#define SCM_IM_IF               SCM_MAKISYM (5)
#define SCM_IM_LAMBDA           SCM_MAKISYM (6)
#define SCM_IM_LET              SCM_MAKISYM (7)
#define SCM_IM_LETSTAR          SCM_MAKISYM (8)
#define SCM_IM_LETREC           SCM_MAKISYM (9)
#define SCM_IM_OR               SCM_MAKISYM (10)
#define SCM_IM_QUOTE            SCM_MAKISYM (11)
#define SCM_IM_SET_X            SCM_MAKISYM (12)
#define SCM_IM_DEFINE           SCM_MAKISYM (13)
#define SCM_IM_APPLY		SCM_MAKISYM (14)
#define SCM_IM_CONT		SCM_MAKISYM (15)
#define SCM_IM_DISPATCH		SCM_MAKISYM (16)
#define SCM_IM_SLOT_REF		SCM_MAKISYM (17)
#define SCM_IM_SLOT_SET_X	SCM_MAKISYM (18)
#define SCM_IM_DELAY		SCM_MAKISYM (19)
#define SCM_IM_FUTURE		SCM_MAKISYM (20)
#define SCM_IM_CALL_WITH_VALUES SCM_MAKISYM (21)
#define SCM_IM_ELSE             SCM_MAKISYM (22)
#define SCM_IM_ARROW            SCM_MAKISYM (23)
#define SCM_IM_NIL_COND         SCM_MAKISYM (24)  /* Multi-language support */
#define SCM_IM_BIND             SCM_MAKISYM (25)  /* Multi-language support */



/* Dispatching aids:

   When switching on SCM_TYP7 of a SCM value, use these fake case
   labels to catch types that use fewer than 7 bits for tagging.  */

/* For cons pairs with immediate values in the CAR
 */

#define scm_tcs_cons_imcar \
       scm_tc2_int + 0:   case scm_tc2_int + 4:   case scm_tc3_imm24 + 0:\
  case scm_tc2_int + 8:   case scm_tc2_int + 12:  case scm_tc3_imm24 + 8:\
  case scm_tc2_int + 16:  case scm_tc2_int + 20:  case scm_tc3_imm24 + 16:\
  case scm_tc2_int + 24:  case scm_tc2_int + 28:  case scm_tc3_imm24 + 24:\
  case scm_tc2_int + 32:  case scm_tc2_int + 36:  case scm_tc3_imm24 + 32:\
  case scm_tc2_int + 40:  case scm_tc2_int + 44:  case scm_tc3_imm24 + 40:\
  case scm_tc2_int + 48:  case scm_tc2_int + 52:  case scm_tc3_imm24 + 48:\
  case scm_tc2_int + 56:  case scm_tc2_int + 60:  case scm_tc3_imm24 + 56:\
  case scm_tc2_int + 64:  case scm_tc2_int + 68:  case scm_tc3_imm24 + 64:\
  case scm_tc2_int + 72:  case scm_tc2_int + 76:  case scm_tc3_imm24 + 72:\
  case scm_tc2_int + 80:  case scm_tc2_int + 84:  case scm_tc3_imm24 + 80:\
  case scm_tc2_int + 88:  case scm_tc2_int + 92:  case scm_tc3_imm24 + 88:\
  case scm_tc2_int + 96:  case scm_tc2_int + 100: case scm_tc3_imm24 + 96:\
  case scm_tc2_int + 104: case scm_tc2_int + 108: case scm_tc3_imm24 + 104:\
  case scm_tc2_int + 112: case scm_tc2_int + 116: case scm_tc3_imm24 + 112:\
  case scm_tc2_int + 120: case scm_tc2_int + 124: case scm_tc3_imm24 + 120

/* For cons pairs with non-immediate values in the SCM_CAR
 */
#define scm_tcs_cons_nimcar \
       scm_tc3_cons + 0:\
  case scm_tc3_cons + 8:\
  case scm_tc3_cons + 16:\
  case scm_tc3_cons + 24:\
  case scm_tc3_cons + 32:\
  case scm_tc3_cons + 40:\
  case scm_tc3_cons + 48:\
  case scm_tc3_cons + 56:\
  case scm_tc3_cons + 64:\
  case scm_tc3_cons + 72:\
  case scm_tc3_cons + 80:\
  case scm_tc3_cons + 88:\
  case scm_tc3_cons + 96:\
  case scm_tc3_cons + 104:\
  case scm_tc3_cons + 112:\
  case scm_tc3_cons + 120

/* For structs
 */
#define scm_tcs_struct \
       scm_tc3_struct + 0:\
  case scm_tc3_struct + 8:\
  case scm_tc3_struct + 16:\
  case scm_tc3_struct + 24:\
  case scm_tc3_struct + 32:\
  case scm_tc3_struct + 40:\
  case scm_tc3_struct + 48:\
  case scm_tc3_struct + 56:\
  case scm_tc3_struct + 64:\
  case scm_tc3_struct + 72:\
  case scm_tc3_struct + 80:\
  case scm_tc3_struct + 88:\
  case scm_tc3_struct + 96:\
  case scm_tc3_struct + 104:\
  case scm_tc3_struct + 112:\
  case scm_tc3_struct + 120

/* For closures
 */
#define scm_tcs_closures \
       scm_tc3_closure + 0:\
  case scm_tc3_closure + 8:\
  case scm_tc3_closure + 16:\
  case scm_tc3_closure + 24:\
  case scm_tc3_closure + 32:\
  case scm_tc3_closure + 40:\
  case scm_tc3_closure + 48:\
  case scm_tc3_closure + 56:\
  case scm_tc3_closure + 64:\
  case scm_tc3_closure + 72:\
  case scm_tc3_closure + 80:\
  case scm_tc3_closure + 88:\
  case scm_tc3_closure + 96:\
  case scm_tc3_closure + 104:\
  case scm_tc3_closure + 112:\
  case scm_tc3_closure + 120

/* For subrs
 */
#define scm_tcs_subrs \
       scm_tc7_asubr:\
  case scm_tc7_subr_0:\
  case scm_tc7_subr_1:\
  case scm_tc7_dsubr:\
  case scm_tc7_cxr:\
  case scm_tc7_subr_3:\
  case scm_tc7_subr_2:\
  case scm_tc7_rpsubr:\
  case scm_tc7_subr_1o:\
  case scm_tc7_subr_2o:\
  case scm_tc7_lsubr_2:\
  case scm_tc7_lsubr



#if (SCM_ENABLE_DEPRECATED == 1)

#define SCM_CELLP(x) 	(((sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)
#define SCM_NCELLP(x) 	(!SCM_CELLP (x))

#endif

#endif  /* SCM_TAGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
