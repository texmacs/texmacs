
/******************************************************************************
* MODULE     : page_type.cpp
* DESCRIPTION: Data base for page sizes and default settings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "vars.hpp"
#include "hashmap.hpp"

static bool page_data_base_initizalized= false;
static hashmap<string,string> page_data_base ("");

static void
INIT (string type, string width, string height, string standard_format,
      string por_parw, string por_hmargin, string por_vmargin,
      string lan_parw, string lan_hmargin, string lan_vmargin)
{
  page_data_base (type * "-P-standard")= standard_format;
  page_data_base (type * "-P-" * PAGE_WIDTH) = width;
  page_data_base (type * "-P-" * PAGE_HEIGHT)= height;
  page_data_base (type * "-P-" * PAR_WIDTH)= por_parw;
  page_data_base (type * "-P-" * PAGE_ODD)= por_hmargin;
  page_data_base (type * "-P-" * PAGE_EVEN)= por_hmargin;
  page_data_base (type * "-P-" * PAGE_RIGHT)= por_hmargin;
  page_data_base (type * "-P-" * PAGE_TOP)= por_vmargin;
  page_data_base (type * "-P-" * PAGE_BOT)= por_vmargin;

  page_data_base (type * "-L-standard")= standard_format;
  page_data_base (type * "-L-" * PAGE_WIDTH) = height;
  page_data_base (type * "-L-" * PAGE_HEIGHT)= width;
  page_data_base (type * "-L-" * PAR_WIDTH)= lan_parw;
  page_data_base (type * "-L-" * PAGE_ODD)= lan_hmargin;
  page_data_base (type * "-L-" * PAGE_EVEN)= lan_hmargin;
  page_data_base (type * "-L-" * PAGE_RIGHT)= lan_hmargin;
  page_data_base (type * "-L-" * PAGE_TOP)= lan_vmargin;
  page_data_base (type * "-L-" * PAGE_BOT)= lan_vmargin;
}

/* this list should contain at least all the paper sizes known to
   the libpaperg library
*/
static void
INIT_ALL () {
  INIT ("10x14", "254mm", "356mm", "yes",
        "180mm", "37mm", "37mm",
        "285mm", "37mm", "37mm");
  INIT ("11x17", "279mm", "432mm", "yes",
        "190mm", "40mm", "40mm",
        "300mm", "40mm", "40mm");
  INIT ("a0", "840mm", "1188mm", "yes",
        "600mm", "120mm", "120mm",
        "948mm", "120mm", "120mm");
  INIT ("a1", "594mm", "840mm", "yes",
        "414mm", "90mm", "90mm",
        "660mm", "90mm", "90mm");
  INIT ("a2", "420mm", "594mm", "yes",
        "300mm",  "60mm",  "60mm",
        "474mm",  "60mm",  "60mm");
  INIT ("a3", "297mm", "420mm", "yes",
        "207mm",  "45mm",  "45mm",
        "330mm",  "45mm",  "45mm");
  INIT ("a4", "210mm", "297mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("a5", "148mm", "210mm", "yes",
        "108mm",  "20mm",  "20mm",
        "170mm",  "20mm",  "20mm");
  INIT ("a6", "105mm", "149mm", "yes",
        "75mm", "15mm", "15mm",
        "118mm", "15mm", "15mm");
  INIT ("a7", "74mm",  "105mm", "yes",
        "54mm", "10mm", "10mm",
        "85mm", "10mm", "10mm");
  INIT ("a8", "52mm", "74mm", "yes",
        "38mm", "8mm", "8mm",
        "59mm", "8mm", "8mm");
  INIT ("a9", "37mm", "52mm", "yes",
        "27mm", "5mm", "5mm",
        "42mm", "5mm", "5mm");
  INIT ("a10", "26mm", "37mm", "yes",
        "19mm", "4mm", "4mm",
        "30mm", "4mm", "4mm");
  INIT ("archA", "229mm", "305mm", "yes",
        "170mm",  "30mm",  "30mm",
        "260mm",  "30mm",  "30mm");
  INIT ("archB", "305mm", "457mm", "yes",
        "210mm",  "45mm",  "45mm",
        "340mm",  "45mm",  "45mm");
  INIT ("archC",  "457mm", "610mm", "yes",
        "340mm", "60mm", "60mm",
        "520mm", "60mm", "60mm");
  INIT ("archD", "610mm", "914mm", "yes",
        "420mm", "90mm", "90mm",
        "680mm", "90mm", "90mm");
  INIT ("archE", "914mm", "1219mm", "yes",
        "680mm", "120mm", "120mm",
        "1040mm", "120mm", "120mm");
  INIT ("b0", "1000mm", "1414mm", "yes",
        "720mm", "140mm", "140mm",
        "1130mm", "140mm", "140mm");
  INIT ("b1", "707mm", "1000mm", "yes",
        "505mm", "100mm", "100mm",
        "800mm", "100mm", "100mm");
  INIT ("b2", "500mm", "707mm", "yes",
        "360mm", "70mm", "70mm",
        "565mm", "70mm", "70mm");
  INIT ("b3", "352mm", "500mm", "yes",
        "252mm",  "50mm",  "50mm",
        "400mm",  "50mm",  "50mm");
  INIT ("b4", "250mm", "352mm", "yes",
        "180mm",  "35mm",  "35mm",
        "282mm",  "35mm",  "35mm");
  INIT ("b5", "176mm", "250mm", "yes",
        "126mm",  "25mm",  "25mm",
        "200mm",  "25mm",  "25mm");
  INIT ("b6", "125mm", "176mm", "yes",
        "95mm",   "15mm",  "15mm",
        "146mm",  "15mm",  "15mm");
  INIT ("c5", "162mm",  "232mm", "yes",
        "115mm",  "25mm",  "25mm",
        "185mm",  "25mm",  "25mm");
  INIT ("Comm10", "105mm", "241mm", "yes",
        "75mm", "15mm", "15mm",
        "118mm", "15mm", "15mm");
  INIT ("csheet", "432mm", "559mm", "yes",
        "330mm", "60mm", "60mm",
        "510mm", "60mm", "60mm");
  INIT ("DL", "110mm", "220mm", "yes",
        "75mm", "15mm", "15mm",
        "118mm", "15mm", "15mm");
  INIT ("dsheet", "559mm", "864mm", "yes",
        "384mm", "90mm", "90mm",
        "620mm", "90mm", "90mm");
  INIT ("esheet", "864mm", "1118mm", "yes",
        "620mm", "120mm", "120mm",
        "970mm", "120mm", "120mm");
  INIT ("executive", "7.25in", "10.5in", "yes",
        "5.25in", "1in",   "1in",
        "8.5in",  "1in",   "1in");
  INIT ("flsa", "216mm", "330mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("flse", "216mm", "330mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("folio", "216mm", "330mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("halfexecutive", "133mm", "184mm", "yes",
        "95mm",   "15mm",  "15mm",
        "146mm",  "15mm",  "15mm");
  INIT ("halfletter", "140mm", "216mm", "yes",
        "100mm",   "15mm",  "15mm",
        "155mm",  "15mm",  "15mm");
  INIT ("ledger", "432mm", "279mm", "yes",
        "330mm", "60mm", "60mm",
        "510mm", "60mm", "60mm");
  INIT ("legal", "8.5in", "14in", "yes",
        "6.5in",  "1in",   "1in",
        "12in",   "1in",   "1in");
  INIT ("letter", "8.5in", "11in", "yes",
        "6.5in",  "1in",   "1in",
        "9in",    "1in",   "1in");
  INIT ("lecture note", "15.5cm", "23.5cm", "no",
        "121mm",  "17mm",  "17mm",
        "201mm",  "17mm",  "17mm");
  INIT ("Monarch", "98mm",  "190mm", "yes",
        "75mm", "15mm", "15mm",
        "118mm", "15mm", "15mm");
  INIT ("note",  "216mm", "279mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("quarto", "215mm", "275mm", "yes",
        "150mm",  "30mm",  "30mm",
        "237mm",  "30mm",  "30mm");
  INIT ("statement", "140mm", "216mm", "yes",
        "108mm",  "20mm",  "20mm",
        "170mm",  "20mm",  "20mm");
  INIT ("tabloid", "279mm", "432mm", "yes",
        "200mm",  "45mm",  "45mm",
        "315mm",  "45mm",  "45mm");
  page_data_base_initizalized= true;
}

string
page_get_feature (string type, string feature, bool landscape) {
  if (!page_data_base_initizalized) INIT_ALL ();
  string s= type * (landscape? string ("-L-"): string ("-P-")) * feature;
  if (page_data_base->contains (s)) return page_data_base [s];
  if (type == "a4") return "3cm";
  return page_get_feature ("a4", feature, landscape);
}
