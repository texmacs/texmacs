/******************************************************************************
* MODULE     : XColorTable.cpp
* DESCRIPTION: Windows version of XWindows color handling
* COPYRIGHT  : (C) 2003 Dan Martens dan_martens@lycos.com
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#pragma warning(disable: 4786);

#include <windows.h>
#include <X11/XColorTable.h>
#include <map>
#include <string>
using namespace std;

typedef struct XCOLOR_STRUCT{
	char* name;
	unsigned int rgbValue;
}XCOLOR_STRUCT;

void HashColors();
void MakeLowerCase(string &toConvert);
BOOL XGetHPCEColor(string &name, unsigned int *theValue);
BOOL GetHexValue(char hexChar, BYTE *temp);

extern BYTE XApproximateByte4ToByte(BYTE);
extern BYTE XApproximateByte12ToByte(USHORT);
extern BYTE XApproximateShortToByte(USHORT);

bool isHashed = false;
map<string, XCOLOR_STRUCT*> lookupMap;

XCOLOR_STRUCT allColors[] = {
	//Shades of grey
	{"black", 0x00000000},
	{"grey", 0x00BEBEBE},
	{"dimgrey", 0x00696969},
	{"lightgrey", 0x00D3D3D3},
	{"lightslategrey" , 0x00778899},
	{"slategrey", 0x00708090},
	{"slategrey1", 0x00C6E2FF},
	{"slategrey2", 0x00B9D3EE},
	{"slategrey3", 0x009FB6CD},
	{"slategrey4", 0x006C7B8B},
	{"slategrey", 0x00708090},
	{"darkgrey", 0x00707070},
	{"grey0", 0x00000000},
	{"grey1", 0x00030303},
	{"grey2", 0x00050505},
	{"grey3", 0x00080808},
	{"grey4", 0x000A0A0A},
	{"grey5", 0x000D0D0D},
	{"grey6", 0x000F0F0F},
	{"grey7", 0x00121212},
	{"grey8", 0x00141414},
	{"grey9", 0x00171717},
	{"grey10", 0x001A1A1A},
	{"grey11", 0x001C1C1C},
	{"grey12", 0x001F1F1F},
	{"grey13", 0x00212121},
	{"grey14", 0x00242424},
	{"grey15", 0x00262626},
	{"grey16", 0x00292929},
	{"grey17", 0x002B2B2B},
	{"grey18", 0x002E2E2E},
	{"grey19", 0x00303030},
	{"grey20", 0x00333333},
	{"grey21", 0x00363636},
	{"grey22", 0x00383838},
	{"grey23", 0x003B3B3B},
	{"grey24", 0x003D3D3D},
	{"grey25", 0x00404040},
	{"grey26", 0x00424242},
	{"grey27", 0x00454545},
	{"grey28", 0x00474747},
	{"grey29", 0x004A4A4A},
	{"grey30", 0x004D4D4D},
	{"grey31", 0x004F4F4F},
	{"grey32", 0x00525252},
	{"grey33", 0x00545454},
	{"grey34", 0x00575757},
	{"grey35", 0x00595959},
	{"grey36", 0x005C5C5C},
	{"grey37", 0x005E5E5E},
	{"grey38", 0x00616161},
	{"grey39", 0x00636363},
	{"grey40", 0x00666666},
	{"grey41", 0x00696969},
	{"grey42", 0x006B6B6B},
	{"grey43", 0x006E6E6E},
	{"grey44", 0x00707070},
	{"grey45", 0x00737373},
	{"grey46", 0x00757575},
	{"grey47", 0x00787878},
	{"grey48", 0x007A7A7A},
	{"grey49", 0x007D7D7D},
	{"grey50", 0x007F7F7F},
	{"grey51", 0x00828282},
	{"grey52", 0x00858585},
	{"grey53", 0x00878787},
	{"grey54", 0x008A8A8A},
	{"grey55", 0x008C8C8C},
	{"grey56", 0x008F8F8F},
	{"grey57", 0x00919191},
	{"grey58", 0x00949494},
	{"grey59", 0x00969696},
	{"grey60", 0x00999999},
	{"grey61", 0x009C9C9C},
	{"grey62", 0x009E9E9E},
	{"grey63", 0x00A1A1A1},
	{"grey64", 0x00A3A3A3},
	{"grey65", 0x00A6A6A6},
	{"grey66", 0x00A8A8A8},
	{"grey67", 0x00ABABAB},
	{"grey68", 0x00ADADAD},
	{"grey69", 0x00B0B0B0},
	{"grey70", 0x00B3B3B3},
	{"grey71", 0x00B5B5B5},
	{"grey72", 0x00B8B8B8},
	{"grey73", 0x00BABABA},
	{"grey74", 0x00BDBDBD},
	{"grey75", 0x00BFBFBF},
	{"grey76", 0x00C2C2C2},
	{"grey77", 0x00C4C4C4},
	{"grey78", 0x00C7C7C7},
	{"grey79", 0x00C9C9C9},
	{"grey80", 0x00CCCCCC},
	{"grey81", 0x00CFCFCF},
	{"grey82", 0x00D1D1D1},
	{"grey83", 0x00D4D4D4},
	{"grey84", 0x00D6D6D6},
	{"grey85", 0x00D9D9D9},
	{"grey86", 0x00DBDBDB},
	{"grey87", 0x00DEDEDE},
	{"grey88", 0x00E0E0E0},
	{"grey89", 0x00E3E3E3},
	{"grey90", 0x00E5E5E5},
	{"grey91", 0x00E8E8E8},
	{"grey92", 0x00EBEBEB},
	{"grey93", 0x00EDEDED},
	{"grey94", 0x00F0F0F0},
	{"grey95", 0x00F2F2F2},
	{"grey96", 0x00F5F5F5},
	{"grey97", 0x00F7F7F7},
	{"grey98", 0x00FAFAFA}, 
	{"grey99", 0x00FCFCFC},
	{"grey100", 0x00FFFFFF},

	//Shades of Blue
	{"AliceBlue", 0x00F0F8FF},
	{"BlueViolet", 0x008A2BE2},
	{"CadetBlue", 0x005F9EA0},
	{"CadetBlue1", 0x0098F5FF},
	{"CadetBlue2", 0x008EE5EE},
	{"CadetBlue3", 0x007AC5CD},
	{"CadetBlue4", 0x0053868B}, 
	{"CornflowerBlue", 0x006495ED},
	{"DarkSlateBlue", 0x00483D8B},
	{"DarkTurquoise", 0x0000CED1},
	{"DeepSkyBlue", 0x0000BFFF},
	{"DeepSkyBlue1", 0x0000BFFF},
	{"DeepSkyBlue2", 0x0000B2EE},
	{"DeepSkyBlue3", 0x00009ACD},
	{"DeepSkyBlue4", 0x0000688B},
	{"DodgerBlue", 0x001E90FF},
	{"DodgerBlue1", 0x001E90FF},
	{"DodgerBlue2", 0x001C86EE},
	{"DodgerBlue3", 0x001874CD},
	{"DodgerBlue4", 0x00104E8B},
	{"LightBlue", 0x00ADD8E6},
	{"LightBlue1", 0x00BFEFFF},
	{"LightBlue2", 0x00B2DFEE},
	{"LightBlue3", 0x009AC0CD},
	{"LightBlue4", 0x0068838B},
	{"LightCyan", 0x00E0FFFF},
	{"LightCyan1", 0x00E0FFFF},
	{"LightCyan2", 0x00D1EEEE},
	{"LightCyan3", 0x00B4CDCD},
	{"LightCyan4", 0x007A8B8B},
	{"LightSkyBlue", 0x0087CEFA},
	{"LightSkyBlue1", 0x00B0E2FF},
	{"LightSkyBlue2", 0x00A4D3EE},
	{"LightSkyBlue3", 0x008DB6CD},
	{"LightSkyBlue4", 0x00607B8B},
	{"LightSlateBlue", 0x008470FF},
	{"LightSteelBlue", 0x00B0C4DE},
	{"LightSteelBlue1", 0x00CAE1FF},
	{"LightSteelBlue2", 0x00BCD2EE},
	{"LightSteelBlue3", 0x00A2B5CD},
	{"LightSteelBlue4", 0x006E7B8B},
	{"MediumAquamarine", 0x0066CDAA},
	{"MediumBlue", 0x000000CD},
	{"MediumSlateBlue", 0x007B68EE},
	{"MediumTurquoise", 0x0048D1CC},
	{"MidnightBlue", 0x00191970},
	{"NavyBlue", 0x00000080},
	{"PaleTurquoise", 0x00AFEEEE},
	{"PaleTurquoise1", 0x00BBFFFF},
	{"PaleTurquoise2", 0x00AEEEEE},
	{"PaleTurquoise3", 0x0096CDCD},
	{"PaleTurquoise4", 0x00668B8B},
	{"PowderBlue", 0x00B0E0E6},
	{"RoyalBlue", 0x004169E1},
	{"RoyalBlue1", 0x004876FF},
	{"RoyalBlue2", 0x00436EEE},
	{"RoyalBlue3", 0x003A5FCD},
	{"RoyalBlue4", 0x007408B},
	{"RoyalBlue5", 0x00002266},
	{"SkyBlue", 0x0087CEEB},
	{"SkyBlue1", 0x0087CEFF},
	{"SkyBlue2", 0x007EC0EE},
	{"SkyBlue3", 0x006CA6CD},
	{"SkyBlue4", 0x004A708B},
	{"SlateBlue", 0x006A5ACD},
	{"SlateBlue1", 0x00836FFF},
	{"SlateBlue2", 0x007A67EE},
	{"SlateBlue3", 0x006959CD},
	{"SlateBlue4", 0x00473C8B},
	{"SteelBlue", 0x004682B4},
	{"SteelBlue1", 0x0063B8FF},
	{"SteelBlue2", 0x005CACEE},
	{"SteelBlue3", 0x004F94CD},
	{"SteelBlue4", 0x0036648B},
	{"aquamarine", 0x007FFFD4},
	{"aquamarine1", 0x007FFFD4},
	{"aquamarine2", 0x0076EEC6},
	{"aquamarine3", 0x0066CDAA},
	{"aquamarine4", 0x00458B74},
	{"azure", 0x00F0FFFF},
	{"azure1", 0x00F0FFFF},
	{"azure2", 0x00E0EEEE},
	{"azure3", 0x00C1CDCD},
	{"azure4", 0x00838B8B},
	{"blue", 0x000000FF},
	{"blue1", 0x000000FF},
	{"blue2", 0x000000EE},
	{"blue3", 0x000000CD},
	{"blue4", 0x0000008B},
	{"cyan", 0x0000FFFF},
	{"cyan1", 0x0000FFFF},
	{"cyan2", 0x0000EEEE},
	{"cyan3", 0x0000CDCD},
	{"cyan4", 0x00008B8B},
	{"navy", 0x00000080},
	{"turquoise", 0x0040E0D0},
	{"turquoise1", 0x0000F5FF},
	{"turquoise2", 0x0000E5EE},
	{"turquoise3", 0x0000C5CD},
	{"turquoise4", 0x0000868B},
	{"DarkSlateGray", 0x002F4F4F},
	{"DarkSlateGray1", 0x0097FFFF},
	{"DarkSlateGray2", 0x008DEEEE},
	{"DarkSlateGray3", 0x0079CDCD},
	{"DarkSlateGray4", 0x00528B8B},
	
	//Shades of Brown
	{"RosyBrown", 0x00BC8F8F},
	{"RosyBrown1", 0x00FFC1C1},
	{"RosyBrown2", 0x00EEB4B4},
	{"RosyBrown3", 0x00CD9B9B},
	{"RosyBrown4", 0x008B6969},
	{"SaddleBrown", 0x008B4513},
	{"SandyBrown", 0x00F4A460},
	{"beige", 0x00F5F5DC},
	{"brown", 0x00A52A2A},
	{"brown1", 0x00FF4040},
	{"brown2", 0x00EE3B3B},
	{"brown3", 0x00CD3333},
	{"brown4", 0x008B2323},
	{"burlywood", 0x00DEB887},
	{"burlywood1", 0x00FFD39B},
	{"burlywood2", 0x00EEC591},
	{"burlywood3", 0x00CDAA7D},
	{"burlywood4", 0x008B7355},
	{"chocolate", 0x00D2691E},
	{"chocolate1", 0x00FF7F24},
	{"chocolate2", 0x00EE7621},
	{"chocolate3", 0x00CD661D},
	{"chocolate4", 0x008B4513},
	{"peru", 0x00CD853F},
	{"tan", 0x00D2B48C},
	{"tan1", 0x00FFA54F},
	{"tan2", 0x00EE9A49},
	{"tan3", 0x00CD853F},
	{"tan4", 0x008B5A2B},

	//Shades of Green
	{"DarkGreen", 0x00006400},
	{"DarkKhaki", 0x00BDB76B},
	{"DarkOliveGreen", 0x00556B2F},
	{"DarkOliveGreen1", 0x00CAFF70},
	{"DarkOliveGreen2", 0x00BCEE68},
	{"DarkOliveGreen3", 0x00A2CD5A},
	{"DarkOliveGreen4", 0x006E8B3D},
	{"DarkSeaGreen", 0x008FBC8F},
	{"DarkSeaGreen1", 0x00C1FFC1},
	{"DarkSeaGreen2", 0x00B4EEB4},
	{"DarkSeaGreen3", 0x009BCD9B},
	{"DarkSeaGreen4", 0x00698B69},
	{"ForestGreen", 0x00228B22},
	{"GreenYellow", 0x00ADFF2F},
	{"LawnGreen", 0x007CFC00},
	{"LightSeaGreen", 0x0020B2AA},
	{"LimeGreen", 0x0032CD32},
	{"MediumSeaGreen", 0x003CB371},
	{"MediumSpringGreen", 0x0000FA9A},
	{"MintCream", 0x00F5FFFA},
	{"OliveDrab", 0x006B8E23},
	{"OliveDrab1", 0x00C0FF3E},
	{"OliveDrab2", 0x00B3EE3A},
	{"OliveDrab3", 0x009ACD32},
	{"OliveDrab4", 0x00698B22},
	{"PaleGreen", 0x0098FB98},
	{"PaleGreen1", 0x009AFF9A},
	{"PaleGreen2", 0x0090EE90},
	{"PaleGreen3", 0x007CCD7C},
	{"PaleGreen4", 0x00548B54},
	{"SeaGreen", 0x002E8B57},
	{"SeaGreen1", 0x0054FF9F},
	{"SeaGreen2", 0x004EEE94},
	{"SeaGreen3", 0x0043CD80},
	{"SeaGreen4", 0x002E8B57},
	{"SpringGreen", 0x0000FF7F},
	{"SpringGreen1", 0x0000FF7F},
	{"SpringGreen2", 0x0000EE76},
	{"SpringGreen3", 0x0000CD66},
	{"SpringGreen4", 0x00008B45},
	{"YellowGreen", 0x009ACD32},
	{"chartreuse", 0x007FFF00},
	{"chartreuse1", 0x007FFF00},
	{"chartreuse2", 0x0076EE00},
	{"chartreuse3", 0x0066CD00},
	{"chartreuse4", 0x00458B00},
	{"green", 0x0000FF00},
	{"green1", 0x0000FF00},
	{"green2", 0x0000EE00},
	{"green3", 0x0000CD00},
	{"green4", 0x00008B00},
	{"khaki", 0x00F0E68C},
	{"khaki1", 0x00FFF68F},
	{"khaki2", 0x00EEE685},
	{"khaki3", 0x00CDC673},
	{"khaki4", 0x008B864E},

	//Shades of Orange
	{"DarkOrange", 0x00FF8C00},
	{"DarkOrange1", 0x00FF7F00},
	{"DarkOrange2", 0x00EE7600},
	{"DarkOrange3", 0x00CD6600},
	{"DarkOrange4", 0x008B4500},
	{"DarkSalmon", 0x00E9967A},
	{"LightCoral", 0x00F08080},
	{"LightSalmon", 0x00FFA07A},
	{"LightSalmon1", 0x00FFA07A},
	{"LightSalmon2", 0x00EE9572},
	{"LightSalmon3", 0x00CD8162},
	{"LightSalmon4", 0x008B5742},
	{"PeachPuff", 0x00FFDAB9},
	{"PeachPuff1", 0x00FFDAB9},
	{"PeachPuff2", 0x00EECBAD},
	{"PeachPuff3", 0x00CDAF95},
	{"PeachPuff4", 0x008B7765},
	{"bisque", 0x00FFE4C4},
	{"bisque1", 0x00FFE4C4},
	{"bisque2", 0x00EED5B7},
	{"bisque3", 0x00CDB79E},
	{"bisque4", 0x008B7D6B},
	{"coral", 0x00FF7F50},
	{"coral1", 0x00FF7256},
	{"coral2", 0x00EE6A50},
	{"coral3", 0x00CD5B45},
	{"coral4", 0x008B3E2F},
	{"honeydew", 0x00F0FFF0},
	{"honeydew1", 0x00F0FFF0},
	{"honeydew2", 0x00E0EEE0},
	{"honeydew3", 0x00C1CDC1},
	{"honeydew4", 0x00838B83},
	{"orange", 0x00FFA500},
	{"orange1", 0x00FFA500},
	{"orange2", 0x00EE9A00},
	{"orange3", 0x00CD8500},
	{"orange4", 0x008B5A00},
	{"salmon", 0x00FA8072},
	{"salmon1", 0x00FF8C69},
	{"salmon2", 0x00EE8262},
	{"salmon3", 0x00CD7054},
	{"salmon4", 0x008B4C39},
	{"sienna", 0x00A0522D},
	{"sienna1", 0x00FF8247},
	{"sienna2", 0x00EE7942},
	{"sienna3", 0x00CD6839},
	{"sienna4", 0x008B4726},

	//Shades of Red
	{"DeepPink", 0x00FF1493},
	{"darkred", 0x00800000},
	{"DeepPink1", 0x00FF1493},
	{"DeepPink2", 0x00EE1289},
	{"DeepPink3", 0x00CD1076}, 
	{"DeepPink4", 0x008B0A50}, 
	{"HotPink", 0x00FF69B4},
	{"HotPink1", 0x00FF6EB4},
	{"HotPink2", 0x00EE6AA7},
	{"HotPink3", 0x00CD6090},
	{"HotPink4", 0x008B3A62},
	{"IndianRed", 0x00CD5C5C},
	{"IndianRed1", 0x00FF6A6A},
	{"IndianRed2", 0x00EE6363},
	{"IndianRed3", 0x00CD5555},
	{"IndianRed4", 0x008B3A3A},
	{"LightPink", 0x00FFB6C1},
	{"LightPink1", 0x00FFAEB9},
	{"LightPink2", 0x00EEA2AD},
	{"LightPink3", 0x00CD8C95},
	{"LightPink4", 0x008B5F65},
	{"MediumVioletRed", 0x00C71585},
	{"MistyRose", 0x00FFE4E1},
	{"MistyRose1", 0x00FFE4E1}, 
	{"MistyRose2", 0x00EED5D2},
	{"MistyRose3", 0x00CDB7B5},
	{"MistyRose4", 0x008B7D7B},
	{"OrangeRed", 0x00FF4500},
	{"OrangeRed1", 0x00FF4500},
	{"OrangeRed2", 0x00EE4000},
	{"OrangeRed3", 0x00CD3700},
	{"OrangeRed4", 0x008B2500},
	{"PaleVioletRed", 0x00DB7093},
	{"PaleVioletRed1", 0x00FF82AB},
	{"PaleVioletRed2", 0x00EE799F},
	{"PaleVioletRed3", 0x00CD6889},
	{"PaleVioletRed4", 0x008B475D},
	{"VioletRed", 0x00D02090},
	{"VioletRed1", 0x00FF3E96},
	{"VioletRed2", 0x00EE3A8C},
	{"VioletRed3", 0x00CD3278},
	{"VioletRed4", 0x008B2252},
	{"firebrick", 0x00B22222},
	{"firebrick1", 0x00FF3030},
	{"firebrick2", 0x00EE2C2C},
	{"firebrick3", 0x00CD2626},
	{"firebrick4", 0x008B1A1A},
	{"pink", 0x00FFC0CB},
	{"pink1", 0x00FFB5C5},
	{"pink2", 0x00EEA9B8},
	{"pink3", 0x00CD919E},
	{"pink4", 0x008B636C},
	{"red", 0x00FF0000},
	{"red1", 0x00FF0000},
	{"red2", 0x00EE0000},
	{"red3", 0x00CD0000},
	{"red4", 0x008B0000},
	{"tomato", 0x00FF6347},
	{"tomato1", 0x00FF6347},
	{"tomato2", 0x00EE5C42},
	{"tomato3", 0x00CD4F39},
	{"tomato4", 0x008B3626},

	//Shades of Violet
	{"DarkOrchid", 0x009932CC},
	{"DarkOrchid1", 0x00BF3EFF},
	{"DarkOrchid2", 0x00B23AEE},
	{"DarkOrchid3", 0x009A32CD},
	{"DarkOrchid4", 0x0068228B},
	{"DarkViolet", 0x009400D3},
	{"LavenderBlush", 0x00FFF0F5},
	{"LavenderBlush1", 0x00FFF0F5},
	{"LavenderBlush2", 0x00EEE0E5},
	{"LavenderBlush3", 0x00CDC1C5},
	{"LavenderBlush4", 0x008B8386},
	{"MediumOrchid", 0x00BA55D3},
	{"MediumOrchid1", 0x00E066FF},
	{"MediumOrchid2", 0x00D15FEE}, 
	{"MediumOrchid3", 0x00B452CD}, 
	{"MediumOrchid4", 0x007A378B},
	{"MediumPurple", 0x009370DB}, 
	{"MediumPurple1", 0x00AB82FF},
	{"MediumPurple2", 0x009F79EE},
	{"MediumPurple3", 0x008968CD},
	{"MediumPurple4", 0x005D478B},
	{"lavender", 0x00E6E6FA},
	{"magenta", 0x00FF00FF},
	{"magenta1", 0x00FF00FF},
	{"magenta2", 0x00EE00EE},
	{"magenta3", 0x00CD00CD},
	{"magenta4", 0x008B008B},
	{"maroon", 0x00B03060},
	{"maroon1", 0x00FF34B3},
	{"maroon2", 0x00EE30A7},
	{"maroon3", 0x00CD2990},
	{"maroon4", 0x008B1C62},
	{"orchid", 0x00DA70D6},
	{"orchid1", 0x00FF83FA},
	{"orchid2", 0x00EE7AE9},
	{"orchid3", 0x00CD69C9},
	{"orchid4", 0x008B4789},
	{"plum", 0x00DDA0DD},
	{"plum1", 0x00FFBBFF},
	{"plum2", 0x00EEAEEE},
	{"plum3", 0x00CD96CD},
	{"plum4", 0x008B668B},
	{"purple", 0x00A020F0},
	{"purple1", 0x009B30FF},
	{"purple2", 0x00912CEE},
	{"purple3", 0x007D26CD},
	{"purple4", 0x00551A8B},
	{"thistle", 0x00D8BFD8},
	{"thistle1", 0x00FFE1FF},
	{"thistle2", 0x00EED2EE},
	{"thistle3", 0x00CDB5CD},
	{"thistle4", 0x008B7B8B},
	{"violet", 0x00EE82EE},

	//Shades of White
	{"AntiqueWhite", 0x00FAEBD7},
	{"AntiqueWhite1", 0x00FFEFDB},
	{"AntiqueWhite2", 0x00EEDFCC},
	{"AntiqueWhite3", 0x00CDC0B0},
	{"AntiqueWhite4", 0x008B8378},
	{"FloralWhite", 0x00FFFAF0},
	{"GhostWhite", 0x00F8F8FF},
	{"NavajoWhite", 0x00FFDEAD},
	{"NavajoWhite1", 0x00FFDEAD},
	{"NavajoWhite2", 0x00EECFA1},
	{"NavajoWhite3", 0x00CDB38B},
	{"NavajoWhite4", 0x008B795E},
	{"OldLace", 0x00FDF5E6},
	{"WhiteSmoke", 0x00F5F5F5},
	{"gainsboro", 0x00DCDCDC},
	{"ivory", 0x00FFFFF0},
	{"ivory1", 0x00FFFFF0},
	{"ivory2", 0x00EEEEE0},
	{"ivory3", 0x00CDCDC1},
	{"ivory4", 0x008B8B83},
	{"linen", 0x00FAF0E6},
	{"seashell", 0x00FFF5EE},
	{"seashell1", 0x00FFF5EE},
	{"seashell2", 0x00EEE5DE},
	{"seashell3", 0x00CDC5BF},
	{"seashell4", 0x008B8682},
	{"snow", 0x00FFFAFA},
	{"snow1", 0x00FFFAFA},
	{"snow2", 0x00EEE9E9},
	{"snow3", 0x00CDC9C9},
	{"snow4", 0x008B8989},
	{"wheat", 0x00F5DEB3},
	{"wheat1", 0x00FFE7BA},
	{"wheat2", 0x00EED8AE},
	{"wheat3", 0x00CDBA96},
	{"wheat4", 0x008B7E66},
	{"white", 0x00FFFFFF},

	//Shades of yellow
	{"BlanchedAlmond", 0x00FFEBCD},
	{"DarkGoldenrod", 0x00B8860B},
	{"DarkGoldenrod1", 0x00FFB90F},
	{"DarkGoldenrod2", 0x00EEAD0E},
	{"DarkGoldenrod3", 0x00CD950C},
	{"DarkGoldenrod4", 0x008B6508},
	{"LemonChiffon", 0x00FFFACD},
	{"LemonChiffon1", 0x00FFFACD},
	{"LemonChiffon2", 0x00EEE9BF},
	{"LemonChiffon3", 0x00CDC9A5},
	{"LemonChiffon4", 0x008B8970},
	{"LightGoldenrod", 0x00EEDD82},
	{"LightGoldenrod1", 0x00FFEC8B},
	{"LightGoldenrod2", 0x00EEDC82},
	{"LightGoldenrod3", 0x00CDBE70},
	{"LightGoldenrod4", 0x008B814C},
	{"LightGoldenrodYellow", 0x00FAFAD2},
	{"LightYellow", 0x00FFFFE0},
	{"LightYellow1", 0x00FFFFE0},
	{"LightYellow2", 0x00EEEED1},
	{"LightYellow3", 0x00CDCDB4},
	{"LightYellow4", 0x008B8B7A},
	{"PaleGoldenrod", 0x00EEE8AA},
	{"PapayaWhip", 0x00FFEFD5},
	{"cornsilk", 0x00FFF8DC},
	{"cornsilk1", 0x00FFF8DC},
	{"cornsilk2", 0x00EEE8CD},
	{"cornsilk3", 0x00CDC8B1},
	{"cornsilk4", 0x008B8878},
	{"gold", 0x00FFD700},
	{"gold1", 0x00FFD700},
	{"gold2", 0x00EEC900},
	{"gold3", 0x00CDAD00},
	{"gold4", 0x008B7500},
	{"goldenrod", 0x00DAA520},
	{"goldenrod1", 0x00FFC125},
	{"goldenrod2", 0x00EEB422},
	{"goldenrod3", 0x00CD9B1D},
	{"goldenrod4", 0x008B6914},
	{"moccasin", 0x00FFE4B5},
	{"yellow", 0x00FFFF00},
	{"yellow1", 0x00FFFF00},
	{"yellow2", 0x00EEEE00},
	{"yellow3", 0x00CDCD00},
	{"yellow4", 0x008B8B00},

	//end of list
	{"endoflist", INVALID_COLOR_VALUE}
};

void MakeLowerCase(string &toConvert){

	int len;
	int index;

	len = toConvert.size();

	for(int i = 0; i < len; i++){
		if(isalpha(toConvert[i]))
			toConvert[i] = tolower(toConvert[i]);
	}

	if((index = toConvert.find("gray")) != -1){
		toConvert[index + 2] = 'e';
	}
}

unsigned int XGetColorValue(const char *name){

	string temp;
	unsigned int colorValue;
	int len;
	map<string, XCOLOR_STRUCT*>::iterator iter;

	if(!isHashed)
		HashColors();

	temp = name;
	MakeLowerCase(temp);
	
	if(XGetHPCEColor(temp, &colorValue))
		return colorValue;

	iter = lookupMap.find(temp);

	if(iter != lookupMap.end())
		return iter->second->rgbValue;
	else
		return INVALID_COLOR_VALUE;
}

BOOL XGetHPCEColor(string &name, unsigned int *theValue){

	int nameIndex;
	int length;
	int numBits;
	USHORT rgb[3];
	BYTE temp;
	int index;

	nameIndex = name.find("#");

	if(nameIndex == -1)
		return FALSE;

	length = name.size() - (nameIndex + 1);

	memset(rgb, 0, sizeof(USHORT) * 3);
	index = -1;

	switch(length){
		case 3: numBits = 4; break;
		case 6: numBits = 8; break;
		case 9: numBits = 12; break;
		case 12: numBits = 16; break;
		default: return FALSE;
	}

	for(int i = 0; i < length; i++){

		if(!(i % (length / 3)))
			index++;

		if(!GetHexValue(name[nameIndex + 1 + i], &temp))
			return FALSE;

		rgb[index] = (rgb[index] << 4) | temp;

	}

	switch(numBits){
		case 4: rgb[0] = XApproximateByte4ToByte((BYTE)rgb[0]);
				rgb[1] = XApproximateByte4ToByte((BYTE)rgb[1]);
				rgb[2] = XApproximateByte4ToByte((BYTE)rgb[2]);
				break;
		case 8: break;
		case 12: rgb[0] = XApproximateByte12ToByte((BYTE)rgb[0]);
				rgb[1] = XApproximateByte12ToByte((BYTE)rgb[1]);
				rgb[2] = XApproximateByte12ToByte((BYTE)rgb[2]);
				break;
		case 16: rgb[0] = XApproximateShortToByte((USHORT)rgb[0]);
				rgb[1] = XApproximateShortToByte((USHORT)rgb[1]);
				rgb[2] = XApproximateShortToByte((USHORT)rgb[2]);
				break;
	}

	*theValue = rgb[0];
	*theValue = (*theValue << 8) | rgb[1];
	*theValue = (*theValue << 8) | rgb[2];

	return TRUE;
}

BOOL GetHexValue(char hexChar, BYTE *temp){

	switch(hexChar){
		case '0': *temp = 0x00; break;
		case '1': *temp = 0x01; break;
		case '2': *temp = 0x02; break;
		case '3': *temp = 0x03; break;
		case '4': *temp = 0x04; break;
		case '5': *temp = 0x05; break;
		case '6': *temp = 0x06; break;
		case '7': *temp = 0x07; break;
		case '8': *temp = 0x08; break;
		case '9': *temp = 0x09; break;
		case 'A': *temp = 0x0A; break;
		case 'B': *temp = 0x0B; break;
		case 'C': *temp = 0x0C; break;
		case 'D': *temp = 0x0D; break;
		case 'E': *temp = 0x0E; break;
		case 'F': *temp = 0x0F; break;
		case 'a': *temp = 0x0A; break;
		case 'b': *temp = 0x0B; break;
		case 'c': *temp = 0x0C; break;
		case 'd': *temp = 0x0D; break;
		case 'e': *temp = 0x0E; break;
		case 'f': *temp = 0x0F; break;
		default: return FALSE;
	}

	return TRUE;

}

const char* XGetColorName(unsigned int value){

	if(!isHashed)
		HashColors();

	for(int i = 0; allColors[i].rgbValue != INVALID_COLOR_VALUE; i++){
		if(allColors[i].rgbValue == value)
			return allColors[i].name;
	}

	return NULL;
}

void HashColors(){

	string temp;

	if(isHashed)
		return;

	for(int i = 0; allColors[i].rgbValue != INVALID_COLOR_VALUE; i++){

		int len;

		temp = allColors[i].name;
		
		MakeLowerCase(temp);

		lookupMap[temp] = &allColors[i];
	}

	isHashed = true;
}