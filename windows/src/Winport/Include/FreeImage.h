// ==========================================================
// FreeImage 2
//
// Design and implementation by
// - Floris van den Berg (flvdberg@wxs.nl)
//
// Contributors:
// - Adam Gates (radad@xoasis.com)
// - Alex Kwak
// - Alexander Dymerets (sashad@te.net.ua)
// - Hervé Drolon (drolon@iut.univ-lehavre.fr)
// - Jan L. Nauta (jln@magentammt.com)
// - Jani Kajala (janik@remedy.fi)
// - Juergen Riecker (j.riecker@gmx.de)
// - Luca Piergentili (l.pierge@terra.es)
// - Machiel ten Brinke (brinkem@uni-one.nl)
// - Markus Loibl (markus.loibl@epost.de)
// - Martin Weber (martweb@gmx.net)
// - Matthias Wandel (mwandel@rim.net)
//
// This file is part of FreeImage 2
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.
//
// Use at your own risk!
// ==========================================================

#ifndef FREEIMAGE_H
#define FREEIMAGE_H

#if defined(_LIB) || defined(FREEIMAGE_LIB) || !defined(WIN32)
#define DLL_API
#define DLL_CALLCONV
#else
#define WIN32_LEAN_AND_MEAN
#define DLL_CALLCONV __stdcall
#ifdef FREEIMAGE_EXPORTS
#define DLL_API __declspec(dllexport)
#else
#define DLL_API __declspec(dllimport)
#endif
#endif

// For C compatility --------------------------------------------------------

#ifdef __cplusplus
#define FI_DEFAULT(x)	= x
#define FI_ENUM(x)      enum x
#define FI_STRUCT(x)	struct x
#else
#define FI_DEFAULT(x)
#define FI_ENUM(x)      typedef int x; enum x
#define FI_STRUCT(x)	typedef struct x x; struct x
#endif

// Bitmap types -------------------------------------------------------------

FI_STRUCT (FIBITMAP) { void *data; };
FI_STRUCT (FIMULTIBITMAP) { void *data; };

// Types used in the library (directly copied from Windows) -----------------

#ifndef _WINDOWS_
#define _WINDOWS_

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef NULL
#define NULL 0
#endif

typedef long BOOL;
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef long LONG;

#ifndef SEEK_SET
#define SEEK_SET  0
#define SEEK_CUR  1
#define SEEK_END  2
#endif

typedef struct tagRGBQUAD {
  BYTE rgbBlue; 
  BYTE rgbGreen; 
  BYTE rgbRed; 
  BYTE rgbReserved; 
} RGBQUAD; 

typedef struct tagBITMAPINFOHEADER{
  DWORD biSize;
  LONG  biWidth; 
  LONG  biHeight; 
  WORD  biPlanes; 
  WORD  biBitCount;
  DWORD biCompression; 
  DWORD biSizeImage; 
  LONG  biXPelsPerMeter; 
  LONG  biYPelsPerMeter; 
  DWORD biClrUsed; 
  DWORD biClrImportant;
} BITMAPINFOHEADER, *PBITMAPINFOHEADER; 

typedef struct tagBITMAPINFO { 
  BITMAPINFOHEADER bmiHeader; 
  RGBQUAD          bmiColors[1];
} BITMAPINFO, *PBITMAPINFO;

#endif

// Important enums ----------------------------------------------------------

FI_ENUM(FREE_IMAGE_FORMAT) {
	FIF_UNKNOWN = -1,
	FIF_BMP = 0,
	FIF_ICO,
	FIF_JPEG,
	FIF_JNG,
	FIF_KOALA,
	FIF_LBM,
	FIF_MNG,
	FIF_PBM,
	FIF_PBMRAW,
	FIF_PCD,
	FIF_PCX,
	FIF_PGM,
	FIF_PGMRAW,
	FIF_PNG,
	FIF_PPM,
	FIF_PPMRAW,
	FIF_RAS,
	FIF_TARGA,
	FIF_TIFF,
	FIF_WBMP,
	FIF_PSD,
	FIF_CUT,
	FIF_IFF = FIF_LBM,
};

FI_ENUM(FREE_IMAGE_COLOR_TYPE) {
	FIC_MINISWHITE = 0,             // min value is white
    FIC_MINISBLACK = 1,             // min value is black
    FIC_RGB        = 2,             // RGB color model
    FIC_PALETTE    = 3,             // color map indexed
	FIC_RGBALPHA   = 4,             // RGB color model with alpha channel
};

FI_ENUM(FREE_IMAGE_QUANTIZE) {
    FIQ_WUQUANT = 0,                // Xiaolin Wu color quantization algorithm
    FIQ_NNQUANT = 1                 // NeuQuant neural-net quantization algorithm by Anthony Dekker
};

// File IO routines ---------------------------------------------------------

#ifndef FREEIMAGE_IO
#define FREEIMAGE_IO

typedef void* fi_handle;
typedef unsigned (DLL_CALLCONV *FI_ReadProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef unsigned (DLL_CALLCONV *FI_WriteProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef int (DLL_CALLCONV *FI_SeekProc) (fi_handle handle, long offset, int origin);
typedef long (DLL_CALLCONV *FI_TellProc) (fi_handle handle);

#ifdef WIN32
#pragma pack(push, 1)
#else
#pragma pack(1)
#endif

FI_STRUCT(FreeImageIO) {
	FI_ReadProc  read_proc;     // pointer to the function used to read data
    FI_WriteProc write_proc;    // pointer to the function used to write data
    FI_SeekProc  seek_proc;     // pointer to the function used to seek
    FI_TellProc  tell_proc;     // pointer to the function used to aquire the current position
};

#ifdef WIN32
#pragma pack(pop)
#else
#pragma pack(4)
#endif

// Plugin routines ----------------------------------------------------------

#ifndef PLUGINS
#define PLUGINS

FI_STRUCT (Plugin);
FI_STRUCT (FreeImage);

typedef FIBITMAP *(DLL_CALLCONV *FI_AllocateProc)(int width, int height, int bpp, unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0));
typedef void (DLL_CALLCONV *FI_FreeProc)(FIBITMAP *dib);
typedef void (DLL_CALLCONV *FI_UnloadProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetColorsUsedProc)(FIBITMAP *dib);
typedef BYTE *(DLL_CALLCONV *FI_GetBitsProc)(FIBITMAP *dib);
typedef BYTE *(DLL_CALLCONV *FI_GetBitsRowColProc)(FIBITMAP *dib, int col, int row);
typedef BYTE *(DLL_CALLCONV *FI_GetScanLineProc)(FIBITMAP *dib, int scanline);
typedef unsigned (DLL_CALLCONV *FI_GetBPPProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetWidthProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetHeightProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetLineProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetPitchProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetDIBSizeProc)(FIBITMAP *dib);
typedef RGBQUAD *(DLL_CALLCONV *FI_GetPaletteProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetDotsPerMeterXProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetDotsPerMeterYProc)(FIBITMAP *dib);
typedef BITMAPINFOHEADER *(DLL_CALLCONV *FI_GetInfoHeaderProc)(FIBITMAP *dib);
typedef BITMAPINFO *(DLL_CALLCONV *FI_GetInfoProc)(FIBITMAP *dib);
typedef FREE_IMAGE_COLOR_TYPE (DLL_CALLCONV *FI_GetColorTypeProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetRedMaskProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetGreenMaskProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetBlueMaskProc)(FIBITMAP *dib);
typedef unsigned (DLL_CALLCONV *FI_GetTransparencyCountProc)(FIBITMAP *dib);
typedef BYTE * (DLL_CALLCONV *FI_GetTransparencyTableProc)(FIBITMAP *dib);
typedef void (DLL_CALLCONV *FI_SetTransparencyTableProc)(FIBITMAP *dib, BYTE *table, int count);
typedef BOOL (DLL_CALLCONV *FI_IsTransparentProc)(FIBITMAP *dib);
typedef void (DLL_CALLCONV *FI_SetTransparentProc)(FIBITMAP *dib, BOOL enabled);
typedef void (DLL_CALLCONV *FI_OutputMessageProc)(int fif, const char *fmt, ...);
typedef void (DLL_CALLCONV *FI_ConvertLine1To8Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine4To8Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine16To8_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine16To8_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine24To8Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine32To8Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine1To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine4To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine8To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine16_565_To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine24To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine32To16_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine1To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine4To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine8To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine16_555_To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine24To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine32To16_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine1To24Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine4To24Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine8To24Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine16To24_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine16To24_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine32To24Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine1To32Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine4To32Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine8To32Proc)(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
typedef void (DLL_CALLCONV *FI_ConvertLine16To32_555Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine16To32_565Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_ConvertLine24To32Proc)(BYTE *target, BYTE *source, int width_in_pixels);
typedef void (DLL_CALLCONV *FI_InitProc)(Plugin *plugin, int format_id);

typedef unsigned (DLL_CALLCONV *FI_ReadProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef unsigned (DLL_CALLCONV *FI_WriteProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef int (DLL_CALLCONV *FI_SeekProc) (fi_handle handle, long offset, int origin);
typedef long (DLL_CALLCONV *FI_TellProc) (fi_handle handle);

typedef const char *(DLL_CALLCONV *FI_FormatProc) ();
typedef const char *(DLL_CALLCONV *FI_DescriptionProc) ();
typedef const char *(DLL_CALLCONV *FI_ExtensionListProc) ();
typedef const char *(DLL_CALLCONV *FI_RegExprProc) ();
typedef void *(DLL_CALLCONV *FI_OpenProc)(FreeImageIO *io, fi_handle handle, BOOL read);
typedef void (DLL_CALLCONV *FI_CloseProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef int (DLL_CALLCONV *FI_PageCountProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef int (DLL_CALLCONV *FI_PageCapabilityProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef FIBITMAP *(DLL_CALLCONV *FI_LoadProc)(FreeImage *freeimage, FreeImageIO *io, fi_handle handle, int page, int flags, void *data);
typedef BOOL (DLL_CALLCONV *FI_SaveProc)(FreeImage *freeimage, FreeImageIO *io, FIBITMAP *dib, fi_handle handle, int page, int flags, void *data);
typedef BOOL (DLL_CALLCONV *FI_ValidateProc)(FreeImageIO *io, fi_handle handle);
typedef const char *(DLL_CALLCONV *FI_MimeProc) ();
typedef BOOL (DLL_CALLCONV *FI_SupportsExportBPPProc)(int bpp);

FI_STRUCT(FreeImage) {
	FI_AllocateProc allocate_proc;
	FI_UnloadProc unload_proc;
	FI_FreeProc free_proc;
	FI_GetColorsUsedProc get_colors_used_proc;
	FI_GetBitsProc get_bits_proc;
	FI_GetBitsRowColProc get_bits_row_col_proc;
	FI_GetScanLineProc get_scanline_proc;
	FI_GetBPPProc get_bpp_proc;
	FI_GetWidthProc get_width_proc;
	FI_GetHeightProc get_height_proc;
	FI_GetLineProc get_line_proc;
	FI_GetPitchProc get_pitch_proc;
	FI_GetDIBSizeProc get_dib_size_proc;
	FI_GetPaletteProc get_palette_proc;
	FI_GetDotsPerMeterXProc get_dots_per_meter_x_proc;
	FI_GetDotsPerMeterYProc get_dots_per_meter_y_proc;
	FI_GetInfoHeaderProc get_info_header_proc;
	FI_GetInfoProc get_info_proc;
	FI_GetColorTypeProc get_color_type_proc;
	FI_GetRedMaskProc get_red_mask_proc;
	FI_GetGreenMaskProc get_green_mask_proc;
	FI_GetBlueMaskProc get_blue_mask_proc;
	FI_GetTransparencyCountProc get_transparency_count_proc;
	FI_GetTransparencyTableProc get_transparency_table_proc;
	FI_SetTransparencyTableProc set_transparency_table_proc;
	FI_IsTransparentProc is_transparent_proc;
	FI_SetTransparentProc set_transparent_proc;
	FI_OutputMessageProc output_message_proc;
	FI_ConvertLine1To8Proc convert_line1to8_proc;
	FI_ConvertLine4To8Proc convert_line_4to8_proc;
	FI_ConvertLine16To8_555Proc convert_line_16to8_555_proc;
	FI_ConvertLine16To8_565Proc convert_line_16to8_565_proc;
	FI_ConvertLine24To8Proc convert_line_24to8_proc;
	FI_ConvertLine32To8Proc convert_line_32to8_proc;
	FI_ConvertLine1To16_555Proc convert_line_1to16_555_proc;
	FI_ConvertLine4To16_555Proc convert_line_4to16_555_proc;
	FI_ConvertLine8To16_555Proc convert_line_8to16_555_proc;
	FI_ConvertLine16_565_To16_555Proc convert_line_16_565_to_16_555_proc;
	FI_ConvertLine24To16_555Proc convert_line_24to16_555_proc;
	FI_ConvertLine32To16_555Proc convert_line_32to16_555_proc;
	FI_ConvertLine1To16_565Proc convert_line_1to16_565_proc;
	FI_ConvertLine4To16_565Proc convert_line_4to16_565_proc;
	FI_ConvertLine8To16_565Proc convert_line_8to16_565_proc;
	FI_ConvertLine16_555_To16_565Proc convert_line_16_555_to_16_565_proc;
	FI_ConvertLine24To16_565Proc convert_line_24to16_565_proc;
	FI_ConvertLine32To16_565Proc convert_line_32to16_565_proc;
	FI_ConvertLine1To24Proc convert_line_1to24_proc;
	FI_ConvertLine4To24Proc convert_line_4to24_proc;
	FI_ConvertLine8To24Proc convert_line_8to24_proc;
	FI_ConvertLine16To24_555Proc convert_line_16to24_555_proc;
	FI_ConvertLine16To24_565Proc convert_line_16to24_565_proc;
	FI_ConvertLine32To24Proc convert_line_32to24_proc;
	FI_ConvertLine1To32Proc convert_line_1to32_proc;
	FI_ConvertLine4To32Proc convert_line_4to32_proc;
	FI_ConvertLine8To32Proc convert_line_8to32_proc;
	FI_ConvertLine16To32_555Proc convert_line_16to32_555_proc;
	FI_ConvertLine16To32_565Proc convert_line_16to32_565_proc;
	FI_ConvertLine24To32Proc convert_line_24to32_proc;	
};

FI_STRUCT (Plugin) {
	FI_FormatProc format_proc;
	FI_DescriptionProc description_proc;
	FI_ExtensionListProc extension_proc;
	FI_RegExprProc regexpr_proc;
	FI_OpenProc open_proc;
	FI_CloseProc close_proc;
	FI_PageCountProc pagecount_proc;
	FI_PageCapabilityProc pagecapability_proc;
	FI_LoadProc load_proc;
	FI_SaveProc save_proc;
	FI_ValidateProc validate_proc;
	FI_MimeProc mime_proc;
	FI_SupportsExportBPPProc supports_export_bpp_proc;
};

#endif
#endif

// Load/Save flag constants -----------------------------------------------------

#define BMP_DEFAULT         0
#define CUT_DEFAULT         0
#define ICO_DEFAULT         0
#define ICO_FIRST           0
#define ICO_SECOND          0
#define ICO_THIRD           0
#define IFF_DEFAULT         0
#define JPEG_DEFAULT        0
#define JPEG_FAST           1
#define JPEG_ACCURATE       2
#define JPEG_QUALITYSUPERB  0x80
#define JPEG_QUALITYGOOD    0x100
#define JPEG_QUALITYNORMAL  0x200
#define JPEG_QUALITYAVERAGE 0x400
#define JPEG_QUALITYBAD     0x800
#define KOALA_DEFAULT       0
#define LBM_DEFAULT         0
#define MNG_DEFAULT         0
#define PCD_DEFAULT         0
#define PCD_BASE            1
#define PCD_BASEDIV4        2
#define PCD_BASEDIV16       3
#define PCX_DEFAULT         0
#define PNG_DEFAULT         0
#define PNG_IGNOREGAMMA		1		// avoid gamma correction
#define PNM_DEFAULT         0
#define PNM_SAVE_RAW        0       // If set the writer saves in RAW format (i.e. P4, P5 or P6)
#define PNM_SAVE_ASCII      1       // If set the writer saves in ASCII format (i.e. P1, P2 or P3)
#define RAS_DEFAULT         0
#define TARGA_DEFAULT       0
#define TARGA_LOAD_RGB888   1       // If set the loader converts RGB555 and ARGB8888 -> RGB888.
#define TARGA_LOAD_RGB555   2       // This flag is obsolete
#define TIFF_DEFAULT        0
#define WBMP_DEFAULT        0
#define PSD_DEFAULT         0

#ifdef __cplusplus
extern "C" {
#endif

// Init/Error routines ------------------------------------------------------

DLL_API void DLL_CALLCONV FreeImage_Initialise(BOOL load_local_plugins_only FI_DEFAULT(FALSE));
DLL_API void DLL_CALLCONV FreeImage_DeInitialise();

// Version routines ---------------------------------------------------------

DLL_API const char *DLL_CALLCONV FreeImage_GetVersion();
DLL_API const char *DLL_CALLCONV FreeImage_GetCopyrightMessage();

// Message output functions -------------------------------------------------

typedef void (*FreeImage_OutputMessageFunction)(FREE_IMAGE_FORMAT fif, const char *msg);
DLL_API void DLL_CALLCONV FreeImage_SetOutputMessage(FreeImage_OutputMessageFunction omf);

// Allocate/Unload routines ------------------------------------------------

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Allocate(int width, int height, int bpp, unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Load(FREE_IMAGE_FORMAT fif, const char *filename, int flags FI_DEFAULT(0));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromHandle(FREE_IMAGE_FORMAT fif, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0));
DLL_API BOOL DLL_CALLCONV FreeImage_Save(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(0));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveToHandle(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0));
DLL_API FIBITMAP * DLL_CALLCONV FreeImage_Clone(FIBITMAP *dib);
DLL_API void DLL_CALLCONV FreeImage_Free(FIBITMAP *dib);
DLL_API void DLL_CALLCONV FreeImage_Unload(FIBITMAP *dib);

// Plugin Interface --------------------------------------------------------

DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterLocalPlugin(FI_InitProc proc_address, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterExternalPlugin(const char *path, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
DLL_API int DLL_CALLCONV FreeImage_GetFIFCount();
DLL_API int DLL_CALLCONV FreeImage_SetPluginEnabled(FREE_IMAGE_FORMAT fif, BOOL enable);
DLL_API int DLL_CALLCONV FreeImage_IsPluginEnabled(FREE_IMAGE_FORMAT fif);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFormat(const char *format);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromMime(const char *mime);
DLL_API const char *DLL_CALLCONV FreeImage_GetFormatFromFIF(FREE_IMAGE_FORMAT fif);
DLL_API const char *DLL_CALLCONV FreeImage_GetFIFExtensionList(FREE_IMAGE_FORMAT fif);
DLL_API const char *DLL_CALLCONV FreeImage_GetFIFDescription(FREE_IMAGE_FORMAT fif);
DLL_API const char * DLL_CALLCONV FreeImage_GetFIFRegExpr(FREE_IMAGE_FORMAT fif);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilename(const char *filename);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsReading(FREE_IMAGE_FORMAT fif);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsWriting(FREE_IMAGE_FORMAT fif);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportBPP(FREE_IMAGE_FORMAT fif, int bpp);

// Multipaging interface ------------------------------------------------------

DLL_API FIMULTIBITMAP * DLL_CALLCONV FreeImage_OpenMultiBitmap(FREE_IMAGE_FORMAT fif, const char *filename, BOOL create_new, BOOL read_only, BOOL keep_cache_in_memory FI_DEFAULT(FALSE));
DLL_API BOOL DLL_CALLCONV FreeImage_CloseMultiBitmap(FIMULTIBITMAP *bitmap);
DLL_API int DLL_CALLCONV FreeImage_GetPageCount(FIMULTIBITMAP *bitmap);
DLL_API void DLL_CALLCONV FreeImage_AppendPage(FIMULTIBITMAP *bitmap, FIBITMAP *data);
DLL_API void DLL_CALLCONV FreeImage_InsertPage(FIMULTIBITMAP *bitmap, int page, FIBITMAP *data);
DLL_API void DLL_CALLCONV FreeImage_DeletePage(FIMULTIBITMAP *bitmap, int page);
DLL_API FIBITMAP * DLL_CALLCONV FreeImage_LockPage(FIMULTIBITMAP *bitmap, int page);
DLL_API void DLL_CALLCONV FreeImage_UnlockPage(FIMULTIBITMAP *bitmap, FIBITMAP *page, BOOL changed);
DLL_API BOOL DLL_CALLCONV FreeImage_MovePage(FIMULTIBITMAP *bitmap, int target, int source);
DLL_API BOOL DLL_CALLCONV FreeImage_GetLockedPageNumbers(FIMULTIBITMAP *bitmap, int *pages, int *count);

// Old style bitmap load routines (deprecated) --------------------------------

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadBMP(const char *filename, int flags FI_DEFAULT(BMP_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadBMPFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(BMP_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadCUT(const char *filename, int flags FI_DEFAULT(CUT_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadCUTFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(CUT_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadICO(const char *filename, int flags FI_DEFAULT(ICO_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadICOFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(ICO_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadIFF(const char *filename, int flags FI_DEFAULT(IFF_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadIFFFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(IFF_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadJPEG(const char *filename, int flags FI_DEFAULT(JPEG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadJPEGFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(JPEG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadKOALA(const char *filename, int flags FI_DEFAULT(KOALA_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadKOALAFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(KOALA_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadLBM(const char *filename, int flags FI_DEFAULT(LBM_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadLBMFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(LBM_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadMNG(const char *filename, int flags FI_DEFAULT(MNG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadMNGFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(MNG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPCD(const char *filename, int flags FI_DEFAULT(PCD_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPCDFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PCD_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPCX(const char *filename, int flags FI_DEFAULT(PCX_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPCXFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PCX_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPNG(const char *filename, int flags FI_DEFAULT(PNG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPNGFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PNG_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPNM(const char *filename, int flags FI_DEFAULT(PNM_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPNMFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PNM_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPSD(const char *filename, int flags FI_DEFAULT(PSD_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadPSDFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PSD_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadRAS(const char *filename, int flags FI_DEFAULT(RAS_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadRASFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(RAS_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadTARGA(const char *filename, int flags FI_DEFAULT(TARGA_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadTARGAFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(TARGA_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadTIFF(const char *filename, int flags FI_DEFAULT(TIFF_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadTIFFFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(TIFF_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadWBMP(const char *filename, int flags FI_DEFAULT(WBMP_DEFAULT));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadWBMPFromHandle(FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(WBMP_DEFAULT));

// Bitmap save routines -----------------------------------------------------

DLL_API BOOL DLL_CALLCONV FreeImage_SaveBMP(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(BMP_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveBMPToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(BMP_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveJPEG(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(JPEG_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveJPEGToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(JPEG_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SavePNG(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT( PNG_DEFAULT ) );
DLL_API BOOL DLL_CALLCONV FreeImage_SavePNGToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PNG_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SavePNM(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(PNM_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SavePNMToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(PNM_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveTIFF(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(TIFF_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveTIFFToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(TIFF_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveWBMP(FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(WBMP_DEFAULT));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveWBMPToHandle(FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(WBMP_DEFAULT));

// Filetype request routines -----------------------------------------------

DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileType(const char *filename, int size);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromHandle(FreeImageIO *io, fi_handle handle, int size);
DLL_API const char * DLL_CALLCONV FreeImage_GetFileTypeFromFormat(FREE_IMAGE_FORMAT fif); // this function is deprecated
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromExt(const char *filename); // this function is deprecated

// FreeImage info routines -------------------------------------------------

DLL_API unsigned DLL_CALLCONV FreeImage_GetRedMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetGreenMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetBlueMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetTransparencyCount(FIBITMAP *dib);
DLL_API BYTE * DLL_CALLCONV FreeImage_GetTransparencyTable(FIBITMAP *dib);
DLL_API void DLL_CALLCONV FreeImage_SetTransparent(FIBITMAP *dib, BOOL enabled);
DLL_API void DLL_CALLCONV FreeImage_SetTransparencyTable(FIBITMAP *dib, BYTE *table, int count);
DLL_API BOOL DLL_CALLCONV FreeImage_IsTransparent(FIBITMAP *dib);

// DIB info routines -------------------------------------------------------

DLL_API unsigned DLL_CALLCONV FreeImage_GetColorsUsed(FIBITMAP *dib);
DLL_API BYTE *DLL_CALLCONV FreeImage_GetBits(FIBITMAP *dib);
DLL_API BYTE *DLL_CALLCONV FreeImage_GetBitsRowCol(FIBITMAP *dib, int col, int row);
DLL_API BYTE *DLL_CALLCONV FreeImage_GetScanLine(FIBITMAP *dib, int scanline);
DLL_API unsigned DLL_CALLCONV FreeImage_GetBPP(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetWidth(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetHeight(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetLine(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetPitch(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDIBSize(FIBITMAP *dib);
DLL_API RGBQUAD *DLL_CALLCONV FreeImage_GetPalette(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterX(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterY(FIBITMAP *dib);
DLL_API BITMAPINFOHEADER *DLL_CALLCONV FreeImage_GetInfoHeader(FIBITMAP *dib);
DLL_API BITMAPINFO *DLL_CALLCONV FreeImage_GetInfo(FIBITMAP *dib);
DLL_API FREE_IMAGE_COLOR_TYPE DLL_CALLCONV FreeImage_GetColorType(FIBITMAP *dib);

// Conversion routines -----------------------------------------------------

DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_565_To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_555_To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To24(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To32(BYTE *target, BYTE *source, int width_in_pixels);

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo8Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits555(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits565(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo24Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo32Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantize(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize);

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertFromRawBits(BYTE *bits, int width, int height, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE));
DLL_API void DLL_CALLCONV FreeImage_ConvertToRawBits(BYTE *bits, FIBITMAP *dib, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE));

#ifdef __cplusplus
}
#endif

#endif // !FREEIMAGE_H
