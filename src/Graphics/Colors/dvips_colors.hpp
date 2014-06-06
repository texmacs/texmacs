  
/******************************************************************************
* MODULE     : dvips_colors.h
* DESCRIPTION: CMYK colors following dvips naming conventions
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
* NOTE       : Inspired from Dr. Uwe Kern <xcolor at ukern dot de> xcolor
*              LaTeX package
*              http://www.ctan.org/tex-archive/macros/latex/contrib/xcolor/
*              See also:
*              https://en.wikibooks.org/wiki/LaTeX/Colors
*              #The_68_standard_colors_known_to_dvips
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#define CMYK(c, m, y, k)  c, m, y, k

static cmyk_record DVIPSColors[] = {
  {"Apricot",         CMYK (  0,  81, 132,   0)},
  {"Aquamarine",      CMYK (209,   0,  76,   0)},
  {"Bittersweet",     CMYK (  0, 191, 255,  61)},
  {"Black",           CMYK (  0,   0,   0, 255)},
  {"Blue",            CMYK (255, 255,   0,   0)},
  {"BlueGreen",       CMYK (216,   0,  84,   0)},
  {"BlueViolet",      CMYK (219, 232,   0,  10)},
  {"BrickRed",        CMYK (  0, 226, 239,  71)},
  {"Brown",           CMYK (  0, 206, 255, 153)},
  {"BurntOrange",     CMYK (  0, 130, 255,   0)},
  {"CadetBlue",       CMYK (158, 145,  58,   0)},
  {"CarnationPink",   CMYK (  0, 160,   0,   0)},
  {"Cerulean",        CMYK (239,  28,   0,   0)},
  {"CornflowerBlue",  CMYK (165,  33,   0,   0)},
  {"Cyan",            CMYK (255,   0,   0,   0)},
  {"Dandelion",       CMYK (  0,  73, 214,   0)},
  {"DarkOrchid",      CMYK (102, 204,  51,   0)},
  {"Emerald",         CMYK (255,   0, 127,   0)},
  {"ForestGreen",     CMYK (232,   0, 224,  30)},
  {"Fuchsia",         CMYK (119, 232,   0,  20)},
  {"Goldenrod",       CMYK (  0,  25, 214,   0)},
  {"Gray",            CMYK (  0,   0,   0, 127)},
  {"Green",           CMYK (255,   0, 255,   0)},
  {"GreenYellow",     CMYK ( 38,   0, 175,   0)},
  {"JungleGreen",     CMYK (252,   0, 132,   0)},
  {"Lavender",        CMYK (  0, 122,   0,   0)},
  {"LimeGreen",       CMYK (127,   0, 255,   0)},
  {"Magenta",         CMYK (  0, 255,   0,   0)},
  {"Mahogany",        CMYK (  0, 216, 221,  89)},
  {"Maroon",          CMYK (  0, 221, 173,  81)},
  {"Melon",           CMYK (  0, 117, 127,   0)},
  {"MidnightBlue",    CMYK (249,  33,   0, 109)},
  {"Mulberry",        CMYK ( 86, 229,   0,   5)},
  {"NavyBlue",        CMYK (239, 137,   0,   0)},
  {"OliveGreen",      CMYK (163,   0, 242, 102)},
  {"Orange",          CMYK (  0, 155, 221,   0)},
  {"OrangeRed",       CMYK (  0, 255, 127,   0)},
  {"Orchid",          CMYK ( 81, 163,   0,   0)},
  {"Peach",           CMYK (  0, 127, 178,   0)},
  {"Periwinkle",      CMYK (145, 140,   0,   0)},
  {"PineGreen",       CMYK (234,   0, 150,  63)},
  {"Plum",            CMYK (127, 255,   0,   0)},
  {"ProcessBlue",     CMYK (244,   0,   0,   0)},
  {"Purple",          CMYK (114, 219,   0,   0)},
  {"RawSienna",       CMYK (  0, 183, 255, 114)},
  {"Red",             CMYK (  0, 255, 255,   0)},
  {"RedOrange",       CMYK (  0, 196, 221,   0)},
  {"RedViolet",       CMYK ( 17, 229,   0,  86)},
  {"Rhodamine",       CMYK (  0, 209,   0,   0)},
  {"RoyalBlue",       CMYK (255, 127,   0,   0)},
  {"RoyalPurple",     CMYK (191, 229,   0,   0)},
  {"RubineRed",       CMYK (  0, 255,  33,   0)},
  {"Salmon",          CMYK (  0, 135,  96,   0)},
  {"SeaGreen",        CMYK (175,   0, 127,   0)},
  {"Sepia",           CMYK (  0, 211, 255, 178)},
  {"SkyBlue",         CMYK (158,   0,  30,   0)},
  {"SpringGreen",     CMYK ( 66,   0, 193,   0)},
  {"Tan",             CMYK ( 35, 107, 142,   0)},
  {"TealBlue",        CMYK (219,   0,  86,   5)},
  {"Thistle",         CMYK ( 30, 150,   0,   0)},
  {"Turquoise",       CMYK (216,   0,  51,   0)},
  {"Violet",          CMYK (201, 224,   0,   0)},
  {"VioletRed",       CMYK (  0, 206,   0,   0)},
  {"White",           CMYK (  0,   0,   0,   0)},
  {"WildStrawberry",  CMYK (  0, 244,  99,   0)},
  {"Yellow",          CMYK (  0,   0, 255,   0)},
  {"YellowGreen",     CMYK (112,   0, 188,   0)},
  {"YellowOrange",    CMYK (  0, 107, 255,   0)},
  {"", 0, 0, 0, 0}
};

#undef CMYK
