/******************************************************************************
* MODULE     : XKeyCodes.h
* DESCRIPTION: Windows version of X11 KeyCodes
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef XKEYCODES_H
#define XKEYCODES_H

#define XK_space               0x020
#define XK_exclam              0x021
#define XK_quotedbl            0x022
#define XK_numbersign          0x023
#define XK_dollar              0x024
#define XK_percent             0x025
#define XK_ampersand           0x026
#define XK_apostrophe          0x027
#define XK_quoteright          0x027	/* deprecated */
#define XK_parenleft           0x028
#define XK_parenright          0x029
#define XK_asterisk            0x02a
#define XK_plus                0x02b
#define XK_comma               0x02c
#define XK_minus               0x02d
#define XK_period              0x02e
#define XK_slash               0x02f
#define XK_0                   0x030
#define XK_1                   0x031
#define XK_2                   0x032
#define XK_3                   0x033
#define XK_4                   0x034
#define XK_5                   0x035
#define XK_6                   0x036
#define XK_7                   0x037
#define XK_8                   0x038
#define XK_9                   0x039
#define XK_colon               0x03a
#define XK_semicolon           0x03b
#define XK_less                0x03c
#define XK_equal               0x03d
#define XK_greater             0x03e
#define XK_question            0x03f
#define XK_at                  0x040
#define XK_A                   0x041
#define XK_B                   0x042
#define XK_C                   0x043
#define XK_D                   0x044
#define XK_E                   0x045
#define XK_F                   0x046
#define XK_G                   0x047
#define XK_H                   0x048
#define XK_I                   0x049
#define XK_J                   0x04a
#define XK_K                   0x04b
#define XK_L                   0x04c
#define XK_M                   0x04d
#define XK_N                   0x04e
#define XK_O                   0x04f
#define XK_P                   0x050
#define XK_Q                   0x051
#define XK_R                   0x052
#define XK_S                   0x053
#define XK_T                   0x054
#define XK_U                   0x055
#define XK_V                   0x056
#define XK_W                   0x057
#define XK_X                   0x058
#define XK_Y                   0x059
#define XK_Z                   0x05a
#define XK_bracketleft         0x05b
#define XK_backslash           0x05c
#define XK_bracketright        0x05d
#define XK_asciicircum         0x05e
#define XK_underscore          0x05f
#define XK_grave               0x060
#define XK_quoteleft           0x060	/* deprecated */
#define XK_a                   0x061
#define XK_b                   0x062
#define XK_c                   0x063
#define XK_d                   0x064
#define XK_e                   0x065
#define XK_f                   0x066
#define XK_g                   0x067
#define XK_h                   0x068
#define XK_i                   0x069
#define XK_j                   0x06a
#define XK_k                   0x06b
#define XK_l                   0x06c
#define XK_m                   0x06d
#define XK_n                   0x06e
#define XK_o                   0x06f
#define XK_p                   0x070
#define XK_q                   0x071
#define XK_r                   0x072
#define XK_s                   0x073
#define XK_t                   0x074
#define XK_u                   0x075
#define XK_v                   0x076
#define XK_w                   0x077
#define XK_x                   0x078
#define XK_y                   0x079
#define XK_z                   0x07a
#define XK_braceleft           0x07b
#define XK_bar                 0x07c
#define XK_braceright          0x07d
#define XK_asciitilde          0x07e

#define XK_nobreakspace        0x0a0
#define XK_exclamdown          0x0a1
#define XK_cent        	       0x0a2
#define XK_sterling            0x0a3
#define XK_currency            0x0a4
#define XK_yen                 0x0a5
#define XK_brokenbar           0x0a6
#define XK_section             0x0a7
#define XK_diaeresis           0x0a8
#define XK_copyright           0x0a9
#define XK_ordfeminine         0x0aa
#define XK_guillemotleft       0x0ab	/* left angle quotation mark */
#define XK_notsign             0x0ac
#define XK_hyphen              0x0ad
#define XK_registered          0x0ae
#define XK_macron              0x0af
#define XK_degree              0x0b0
#define XK_plusminus           0x0b1
#define XK_twosuperior         0x0b2
#define XK_threesuperior       0x0b3
#define XK_acute               0x0b4
#define XK_mu                  0x0b5
#define XK_paragraph           0x0b6
#define XK_periodcentered      0x0b7
#define XK_cedilla             0x0b8
#define XK_onesuperior         0x0b9
#define XK_masculine           0x0ba
#define XK_guillemotright      0x0bb	/* right angle quotation mark */
#define XK_onequarter          0x0bc
#define XK_onehalf             0x0bd
#define XK_threequarters       0x0be
#define XK_questiondown        0x0bf
#define XK_Agrave              0x0c0
#define XK_Aacute              0x0c1
#define XK_Acircumflex         0x0c2
#define XK_Atilde              0x0c3
#define XK_Adiaeresis          0x0c4
#define XK_Aring               0x0c5
#define XK_AE                  0x0c6
#define XK_Ccedilla            0x0c7
#define XK_Egrave              0x0c8
#define XK_Eacute              0x0c9
#define XK_Ecircumflex         0x0ca
#define XK_Ediaeresis          0x0cb
#define XK_Igrave              0x0cc
#define XK_Iacute              0x0cd
#define XK_Icircumflex         0x0ce
#define XK_Idiaeresis          0x0cf
#define XK_ETH                 0x0d0
#define XK_Eth                 0x0d0	/* deprecated */
#define XK_Ntilde              0x0d1
#define XK_Ograve              0x0d2
#define XK_Oacute              0x0d3
#define XK_Ocircumflex         0x0d4
#define XK_Otilde              0x0d5
#define XK_Odiaeresis          0x0d6
#define XK_multiply            0x0d7
#define XK_Ooblique            0x0d8
#define XK_Oslash              XK_Ooblique
#define XK_Ugrave              0x0d9
#define XK_Uacute              0x0da
#define XK_Ucircumflex         0x0db
#define XK_Udiaeresis          0x0dc
#define XK_Yacute              0x0dd
#define XK_THORN               0x0de
#define XK_Thorn               0x0de	/* deprecated */
#define XK_ssharp              0x0df
#define XK_agrave              0x0e0
#define XK_aacute              0x0e1
#define XK_acircumflex         0x0e2
#define XK_atilde              0x0e3
#define XK_adiaeresis          0x0e4
#define XK_aring               0x0e5
#define XK_ae                  0x0e6
#define XK_ccedilla            0x0e7
#define XK_egrave              0x0e8
#define XK_eacute              0x0e9
#define XK_ecircumflex         0x0ea
#define XK_ediaeresis          0x0eb
#define XK_igrave              0x0ec
#define XK_iacute              0x0ed
#define XK_icircumflex         0x0ee
#define XK_idiaeresis          0x0ef
#define XK_eth                 0x0f0
#define XK_ntilde              0x0f1
#define XK_ograve              0x0f2
#define XK_oacute              0x0f3
#define XK_ocircumflex         0x0f4
#define XK_otilde              0x0f5
#define XK_odiaeresis          0x0f6
#define XK_division            0x0f7
#define XK_oslash              0x0f8
#define XK_ooblique            XK_oslash
#define XK_ugrave              0x0f9
#define XK_uacute              0x0fa
#define XK_ucircumflex         0x0fb
#define XK_udiaeresis          0x0fc
#define XK_yacute              0x0fd
#define XK_thorn               0x0fe
#define XK_ydiaeresis          0x0ff

/*#define XK_Cyrillic_GHE_bar	                           0x680
#define XK_Cyrillic_ghe_bar	                           0x690
#define XK_Cyrillic_ZHE_descender	                   0x681
#define XK_Cyrillic_zhe_descender	                   0x691
#define XK_Cyrillic_KA_descender	                   0x682
#define XK_Cyrillic_ka_descender	                   0x692
#define XK_Cyrillic_KA_vertstroke	                   0x683
#define XK_Cyrillic_ka_vertstroke	                   0x693
#define XK_Cyrillic_EN_descender	                   0x684
#define XK_Cyrillic_en_descender	                   0x694
#define XK_Cyrillic_U_straight	                       0x685
#define XK_Cyrillic_u_straight	                       0x695
#define XK_Cyrillic_U_straight_bar	                   0x686
#define XK_Cyrillic_u_straight_bar	                   0x696
#define XK_Cyrillic_HA_descender	                   0x687
#define XK_Cyrillic_ha_descender	                   0x697
#define XK_Cyrillic_CHE_descender	                   0x688
#define XK_Cyrillic_che_descender	                   0x698
#define XK_Cyrillic_CHE_vertstroke	                   0x689
#define XK_Cyrillic_che_vertstroke	                   0x699
#define XK_Cyrillic_SHHA	                           0x68a
#define XK_Cyrillic_shha	                           0x69a

#define XK_Cyrillic_SCHWA	                           0x68c
#define XK_Cyrillic_schwa	                           0x69c
#define XK_Cyrillic_I_macron		                   0x68d
#define XK_Cyrillic_i_macron		                   0x69d
#define XK_Cyrillic_O_bar	                           0x68e
#define XK_Cyrillic_o_bar	                           0x69e
#define XK_Cyrillic_U_macron		                   0x68f
#define XK_Cyrillic_u_macron		                   0x69f
*/
//#define XK_Serbian_dje                                 0x6a1
//#define XK_Macedonia_gje                               0x6a2
#define XK_Cyrillic_io                                 0x451
#define XK_Ukrainian_ie                                0x435
#define XK_Ukranian_je                                 0x458  /* deprecated */
//#define XK_Macedonia_dse                               0x6a5
#define XK_Ukrainian_i                                 0x456
#define XK_Ukranian_i                                  0x456  /* deprecated */
#define XK_Ukrainian_yi                                0x457
#define XK_Ukranian_yi                                 0x457  /* deprecated */
#define XK_Cyrillic_je                                 0x458
//#define XK_Serbian_je                                  0x6a8  /* deprecated */
#define XK_Cyrillic_lje                                0x459
//#define XK_Serbian_lje                                 0x6a9  /* deprecated */
#define XK_Cyrillic_nje                                0x45A
//#define XK_Serbian_nje                                 0x6aa  /* deprecated */
//#define XK_Serbian_tshe                                0x6ab
//#define XK_Macedonia_kje                               0x6ac
#define XK_Ukrainian_ghe_with_upturn                   0x491
//#define XK_Byelorussian_shortu                         0x6ae
#define XK_Cyrillic_dzhe                               0x45F
//#define XK_Serbian_dze                                 0x6af  /* deprecated */
//#define XK_numerosign                                  0x6b0
//#define XK_Serbian_DJE                                 0x6b1
//#define XK_Macedonia_GJE                               0x6b2
#define XK_Cyrillic_IO                                 0x401
#define XK_Ukrainian_IE                                0x404
//#define XK_Ukranian_JE                                 0x6b4  /* deprecated */
//#define XK_Macedonia_DSE                               0x6b5
#define XK_Ukrainian_I                                 0x406
//#define XK_Ukranian_I                                  0x6b6  /* deprecated */
#define XK_Ukrainian_YI                                0x407
//#define XK_Ukranian_YI                                 0x6b7  /* deprecated */
#define XK_Cyrillic_JE                                 0x408
//#define XK_Serbian_JE                                  0x6b8  /* deprecated */
#define XK_Cyrillic_LJE                                0x409
//#define XK_Serbian_LJE                                 0x6b9  /* deprecated */
#define XK_Cyrillic_NJE                                0x40A
//#define XK_Serbian_NJE                                 0x6ba  /* deprecated */
//#define XK_Serbian_TSHE                                0x6bb
//#define XK_Macedonia_KJE                               0x6bc
#define XK_Ukrainian_GHE_WITH_UPTURN                   0x490
//#define XK_Byelorussian_SHORTU                         0x6be
#define XK_Cyrillic_DZHE                               0x40F
//#define XK_Serbian_DZE                                 0x6bf  /* deprecated */
#define XK_Cyrillic_yu                                 0x44E
#define XK_Cyrillic_a                                  0x430
#define XK_Cyrillic_be                                 0x431
#define XK_Cyrillic_tse                                0x446
#define XK_Cyrillic_de                                 0x434
#define XK_Cyrillic_ie                                 0x435
#define XK_Cyrillic_ef                                 0x444
#define XK_Cyrillic_ghe                                0x433
#define XK_Cyrillic_ha                                 0x445
#define XK_Cyrillic_i                                  0x438
#define XK_Cyrillic_shorti                             0x439
#define XK_Cyrillic_ka                                 0x43A
#define XK_Cyrillic_el                                 0x43B
#define XK_Cyrillic_em                                 0x43C
#define XK_Cyrillic_en                                 0x43D
#define XK_Cyrillic_o                                  0x43E
#define XK_Cyrillic_pe                                 0x43F
#define XK_Cyrillic_ya                                 0x44F
#define XK_Cyrillic_er                                 0x440
#define XK_Cyrillic_es                                 0x441
#define XK_Cyrillic_te                                 0x442
#define XK_Cyrillic_u                                  0x443
#define XK_Cyrillic_zhe                                0x436
#define XK_Cyrillic_ve                                 0x432
#define XK_Cyrillic_softsign                           0x44C
#define XK_Cyrillic_yeru                               0x44B
#define XK_Cyrillic_ze                                 0x437
#define XK_Cyrillic_sha                                0x448
#define XK_Cyrillic_e                                  0x44D
#define XK_Cyrillic_shcha                              0x449
#define XK_Cyrillic_che                                0x447
#define XK_Cyrillic_hardsign                           0x44A
#define XK_Cyrillic_YU                                 0x42E
#define XK_Cyrillic_A                                  0x410
#define XK_Cyrillic_BE                                 0x411
#define XK_Cyrillic_TSE                                0x426
#define XK_Cyrillic_DE                                 0x414
#define XK_Cyrillic_IE                                 0x415
#define XK_Cyrillic_EF                                 0x424
#define XK_Cyrillic_GHE                                0x413
#define XK_Cyrillic_HA                                 0x425
#define XK_Cyrillic_I                                  0x418
#define XK_Cyrillic_SHORTI                             0x419
#define XK_Cyrillic_KA                                 0x41A
#define XK_Cyrillic_EL                                 0x41B
#define XK_Cyrillic_EM                                 0x41C
#define XK_Cyrillic_EN                                 0x41D
#define XK_Cyrillic_O                                  0x41E
#define XK_Cyrillic_PE                                 0x41F
#define XK_Cyrillic_YA                                 0x42F
#define XK_Cyrillic_ER                                 0x420
#define XK_Cyrillic_ES                                 0x421
#define XK_Cyrillic_TE                                 0x422
#define XK_Cyrillic_U                                  0x423
#define XK_Cyrillic_ZHE                                0x416
#define XK_Cyrillic_VE                                 0x412
#define XK_Cyrillic_SOFTSIGN                           0x42C
#define XK_Cyrillic_YERU                               0x42B
#define XK_Cyrillic_ZE                                 0x417
#define XK_Cyrillic_SHA                                0x428
#define XK_Cyrillic_E                                  0x42D
#define XK_Cyrillic_SHCHA                              0x429
#define XK_Cyrillic_CHE                                0x427
#define XK_Cyrillic_HARDSIGN                           0x42A

#define XK_Aogonek             0x1a1
#define XK_breve               0x1a2
#define XK_Lstroke             0x1a3
#define XK_Lcaron              0x1a5
#define XK_Sacute              0x1a6
#define XK_Scaron              0x1a9
#define XK_Scedilla            0x1aa
#define XK_Tcaron              0x1ab
#define XK_Zacute              0x1ac
#define XK_Zcaron              0x1ae
#define XK_Zabovedot           0x1af
#define XK_aogonek             0x1b1
#define XK_ogonek              0x1b2
#define XK_lstroke             0x1b3
#define XK_lcaron              0x1b5
#define XK_sacute              0x1b6
#define XK_caron               0x1b7
#define XK_scaron              0x1b9
#define XK_scedilla            0x1ba
#define XK_tcaron              0x1bb
#define XK_zacute              0x1bc
#define XK_doubleacute         0x1bd
#define XK_zcaron              0x1be
#define XK_zabovedot           0x1bf
#define XK_Racute              0x1c0
#define XK_Abreve              0x1c3
#define XK_Lacute              0x1c5
#define XK_Cacute              0x1c6
#define XK_Ccaron              0x1c8
#define XK_Eogonek             0x1ca
#define XK_Ecaron              0x1cc
#define XK_Dcaron              0x1cf
#define XK_Dstroke             0x1d0
#define XK_Nacute              0x1d1
#define XK_Ncaron              0x1d2
#define XK_Odoubleacute        0x1d5
#define XK_Rcaron              0x1d8
#define XK_Uring               0x1d9
#define XK_Udoubleacute        0x1db
#define XK_Tcedilla            0x1de
#define XK_racute              0x1e0
#define XK_abreve              0x1e3
#define XK_lacute              0x1e5
#define XK_cacute              0x1e6
#define XK_ccaron              0x1e8
#define XK_eogonek             0x1ea
#define XK_ecaron              0x1ec
#define XK_dcaron              0x1ef
#define XK_dstroke             0x1f0
#define XK_nacute              0x1f1
#define XK_ncaron              0x1f2
#define XK_odoubleacute        0x1f5
#define XK_udoubleacute        0x1fb
#define XK_rcaron              0x1f8
#define XK_uring               0x1f9
#define XK_tcedilla            0x1fe
#define XK_abovedot            0x1ff


/* Cursor control & motion */

#define XK_Home			0xFF50
#define XK_Left			0xFF51	/* Move left, left arrow */
#define XK_Up			0xFF52	/* Move up, up arrow */
#define XK_Right		0xFF53	/* Move right, right arrow */
#define XK_Down			0xFF54	/* Move down, down arrow */
#define XK_Prior		0xFF55	/* Prior, previous */
#define XK_Page_Up		0xFF55
#define XK_Next			0xFF56	/* Next */
#define XK_Page_Down		0xFF56
#define XK_End			0xFF57	/* EOL */
#define XK_Begin		0xFF58	/* BOL */


/* Misc Functions */

#define XK_Select		0xFF60	/* Select, mark */
#define XK_Print		0xFF61
#define XK_Execute		0xFF62	/* Execute, run, do */
#define XK_Insert		0xFF63	/* Insert, insert here */
#define XK_Undo			0xFF65	/* Undo, oops */
#define XK_Redo			0xFF66	/* redo, again */
#define XK_Menu			0xFF67
#define XK_Find			0xFF68	/* Find, search */
#define XK_Cancel		0xFF69	/* Cancel, stop, abort, exit */
#define XK_Help			0xFF6A	/* Help */
#define XK_Break		0xFF6B
#define XK_Mode_switch		0xFF7E	/* Character set switch */
#define XK_script_switch        0xFF7E  /* Alias for mode_switch */
#define XK_Num_Lock		0xFF7F

/* Keypad Functions, keypad numbers cleverly chosen to map to ascii */

#define XK_KP_Space		0xFF80	/* space */
#define XK_KP_Tab		0xFF89
#define XK_KP_Enter		0xFF8D	/* enter */
#define XK_KP_F1		0xFF91	/* PF1, KP_A, ... */
#define XK_KP_F2		0xFF92
#define XK_KP_F3		0xFF93
#define XK_KP_F4		0xFF94
#define XK_KP_Home		0xFF95
#define XK_KP_Left		0xFF96
#define XK_KP_Up		0xFF97
#define XK_KP_Right		0xFF98
#define XK_KP_Down		0xFF99
#define XK_KP_Prior		0xFF9A
#define XK_KP_Page_Up		0xFF9A
#define XK_KP_Next		0xFF9B
#define XK_KP_Page_Down		0xFF9B
#define XK_KP_End		0xFF9C
#define XK_KP_Begin		0xFF9D
#define XK_KP_Insert		0xFF9E
#define XK_KP_Delete		0xFF9F
#define XK_KP_Equal		0xFFBD	/* equals */
#define XK_KP_Multiply		0xFFAA
#define XK_KP_Add		0xFFAB
#define XK_KP_Separator		0xFFAC	/* separator, often comma */
#define XK_KP_Subtract		0xFFAD
#define XK_KP_Decimal		0xFFAE
#define XK_KP_Divide		0xFFAF

#define XK_KP_0			0xFFB0
#define XK_KP_1			0xFFB1
#define XK_KP_2			0xFFB2
#define XK_KP_3			0xFFB3
#define XK_KP_4			0xFFB4
#define XK_KP_5			0xFFB5
#define XK_KP_6			0xFFB6
#define XK_KP_7			0xFFB7
#define XK_KP_8			0xFFB8
#define XK_KP_9			0xFFB9



/*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 */

#define XK_F1			0xFFBE
#define XK_F2			0xFFBF
#define XK_F3			0xFFC0
#define XK_F4			0xFFC1
#define XK_F5			0xFFC2
#define XK_F6			0xFFC3
#define XK_F7			0xFFC4
#define XK_F8			0xFFC5
#define XK_F9			0xFFC6
#define XK_F10			0xFFC7
#define XK_F11			0xFFC8
#define XK_L1			0xFFC8
#define XK_F12			0xFFC9
#define XK_L2			0xFFC9
#define XK_F13			0xFFCA
#define XK_L3			0xFFCA
#define XK_F14			0xFFCB
#define XK_L4			0xFFCB
#define XK_F15			0xFFCC
#define XK_L5			0xFFCC
#define XK_F16			0xFFCD
#define XK_L6			0xFFCD
#define XK_F17			0xFFCE
#define XK_L7			0xFFCE
#define XK_F18			0xFFCF
#define XK_L8			0xFFCF
#define XK_F19			0xFFD0
#define XK_L9			0xFFD0
#define XK_F20			0xFFD1
#define XK_L10			0xFFD1
#define XK_F21			0xFFD2
#define XK_R1			0xFFD2
#define XK_F22			0xFFD3
#define XK_R2			0xFFD3
#define XK_F23			0xFFD4
#define XK_R3			0xFFD4
#define XK_F24			0xFFD5
#define XK_R4			0xFFD5
#define XK_F25			0xFFD6
#define XK_R5			0xFFD6
#define XK_F26			0xFFD7
#define XK_R6			0xFFD7
#define XK_F27			0xFFD8
#define XK_R7			0xFFD8
#define XK_F28			0xFFD9
#define XK_R8			0xFFD9
#define XK_F29			0xFFDA
#define XK_R9			0xFFDA
#define XK_F30			0xFFDB
#define XK_R10			0xFFDB
#define XK_F31			0xFFDC
#define XK_R11			0xFFDC
#define XK_F32			0xFFDD
#define XK_R12			0xFFDD
#define XK_F33			0xFFDE
#define XK_R13			0xFFDE
#define XK_F34			0xFFDF
#define XK_R14			0xFFDF
#define XK_F35			0xFFE0
#define XK_R15			0xFFE0

/* Modifiers */

#define XK_Shift_L		0xFFE1	/* Left shift */
#define XK_Shift_R		0xFFE2	/* Right shift */
#define XK_Control_L		0xFFE3	/* Left control */
#define XK_Control_R		0xFFE4	/* Right control */
#define XK_Caps_Lock		0xFFE5	/* Caps lock */
#define XK_Shift_Lock		0xFFE6	/* Shift lock */

#define XK_Meta_L		0xFFE7	/* Left meta */
#define XK_Meta_R		0xFFE8	/* Right meta */
#define XK_Alt_L		0xFFE9	/* Left alt */
#define XK_Alt_R		0xFFEA	/* Right alt */
#define XK_Super_L		0xFFEB	/* Left super */
#define XK_Super_R		0xFFEC	/* Right super */
#define XK_Hyper_L		0xFFED	/* Left hyper */
#define XK_Hyper_R		0xFFEE	/* Right hyper */

#define XK_BackSpace		0xFF08	/* back space, back char */
#define XK_Tab			0xFF09
#define XK_Linefeed		0xFF0A	/* Linefeed, LF */
#define XK_Clear		0xFF0B
#define XK_Return		0xFF0D	/* Return, enter */
#define XK_Pause		0xFF13	/* Pause, hold */
#define XK_Scroll_Lock		0xFF14
#define XK_Sys_Req		0xFF15
#define XK_Escape		0xFF1B
#define XK_Delete		0xFFFF	/* Delete, rubout */

#define	XK_ISO_Left_Tab					0xFE20

#define	SunXK_FA_Grave		0x1005FF00
#define	SunXK_FA_Circum		0x1005FF01
#define	SunXK_FA_Tilde		0x1005FF02
#define	SunXK_FA_Acute		0x1005FF03
#define	SunXK_FA_Diaeresis	0x1005FF04
#define	SunXK_FA_Cedilla	0x1005FF05

/*
 * Miscellaneous Functions
 */

#define	SunXK_F36		0x1005FF10	/* Labeled F11 */
#define	SunXK_F37		0x1005FF11	/* Labeled F12 */

#define SunXK_Sys_Req   	0x1005FF60
#define SunXK_Print_Screen	0x0000FF61	/* Same as XK_Print */

/*
 * International & Multi-Key Character Composition
 */

#define SunXK_Compose		0x0000FF20	/* Same as XK_Multi_key */
#define SunXK_AltGraph		0x0000FF7E	/* Same as XK_Mode_switch */

/*
 * Cursor Control
 */

#define SunXK_PageUp		0x0000FF55 	/* Same as XK_Prior */
#define SunXK_PageDown		0x0000FF56	/* Same as XK_Next */

/*
 * Open Look Functions
 */

#define SunXK_Undo		0x0000FF65	/* Same as XK_Undo */
#define SunXK_Again		0x0000FF66	/* Same as XK_Redo */
#define SunXK_Find		0x0000FF68	/* Same as XK_Find */
#define SunXK_Stop		0x0000FF69	/* Same as XK_Cancel */
#define SunXK_Props		0x1005FF70
#define SunXK_Front		0x1005FF71
#define SunXK_Copy		0x1005FF72
#define SunXK_Open		0x1005FF73
#define SunXK_Paste		0x1005FF74
#define SunXK_Cut		0x1005FF75

#define SunXK_PowerSwitch		0x1005FF76
#define SunXK_AudioLowerVolume		0x1005FF77
#define SunXK_AudioMute			0x1005FF78
#define SunXK_AudioRaiseVolume		0x1005FF79
#define SunXK_VideoDegauss		0x1005FF7A
#define SunXK_VideoLowerBrightness	0x1005FF7B
#define SunXK_VideoRaiseBrightness	0x1005FF7C
#define SunXK_PowerSwitchShift		0x1005FF7D

#endif