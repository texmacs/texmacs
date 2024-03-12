/******************************************************************************
 * MODULE     : pdf_hummus_make_attachment.cpp
 * DESCRIPTION: Interface for embedding text files into pdf files
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#include "pdf_hummus_make_attachment.hpp"
#include "PDFWriter/DictionaryContext.h"
#include "PDFWriter/InputFileStream.h"
#include "PDFWriter/ObjectsContext.h"
#include "PDFWriter/OutputBufferedStream.h"
#include "PDFWriter/OutputFileStream.h"
#include "PDFWriter/PDFRectangle.h"
#include "PDFWriter/PDFStream.h"
#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/SafeBufferMacrosDefs.h"
#include "PDFWriter/Trace.h"
#include "file.hpp"
//#include "tm_debug.hpp"
using namespace PDFHummus;
using namespace IOBasicTypes;

class PDFAttachment {
public:
  PDFAttachment (void);
  PDFAttachment (string attachment_path);
  PDFAttachment (Byte inByte[], size_t inLenth, string inName);
  ~PDFAttachment (void);

  Byte*  file_content;
  size_t lenth;
  string name;
};

class PDFWriter;

typedef hashmap<PDFAttachment*, ObjectIDType> PDFAttachmentToObjectIDTypeMap;

class PDFAttachmentWriter : public DocumentContextExtenderAdapter {
public:
  PDFAttachmentWriter (PDFWriter* inPDFWriter);
  ~PDFAttachmentWriter (void);

  EStatusCode AttachToAllPage (PDFAttachment* inAttachment);

  virtual EStatusCode
  OnCatalogWrite (CatalogInformation* inCatalogInformation,
                  DictionaryContext*  inCatalogDictionaryContext,
                  ObjectsContext*     inPDFWriterObjectContext,
                  DocumentContext*    inPDFWriterDocumentContext);

private:
  PDFWriter*                     mPDFWriter;
  PDFAttachmentToObjectIDTypeMap mAttachment;

  void                       ListenOnCatalogWrite ();
  EStatusCodeAndObjectIDType WriteAttachment (PDFAttachment* inAttachment);
  void                       CleanupAttachment ();
};
bool
pdf_hummus_make_attachments (url pdf_path, array<url> attachment_paths,
                             url out_path) {
  if (N (attachment_paths) == 0) {
    if (DEBUG_CONVERT) debug_convert << "N (attachment_paths) is 0\n";
    return false;
  }
  for (int i= 0; i < N (attachment_paths); i++) {
    if (!is_regular (attachment_paths[i])) {
      if (DEBUG_CONVERT)
        debug_convert << attachment_paths[i] << " is not regular\n";
      return false;
    }
  }
  if (!is_regular (pdf_path)) {
    if (DEBUG_CONVERT) debug_convert << pdf_path << " is not regular\n";
    return false;
  }

  PDFWriter   pdfWriter;
  EStatusCode status;
  do {
    status= pdfWriter.ModifyPDF (as_charp (as_string (pdf_path)), ePDFVersion16,
                                 as_charp (as_string (out_path)));

    if (status != eSuccess) {
      if (DEBUG_CONVERT)
        debug_convert << "ModifyPDF fail to open " << pdf_path << LF;
      break;
    }
    PDFAttachmentWriter attachmentWriter (&pdfWriter);
    for (int i= 0; i < (int) N (attachment_paths); i++) {
      string          attachment_path= as_string (attachment_paths[i]);
      InputFileStream tm_file_stream;
      status= tm_file_stream.Open (as_charp (attachment_path));

      if (status != PDFHummus::eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "failed to open " << attachment_path << "\n";
        continue;
      }
      else {
        if (DEBUG_CONVERT)
          debug_convert << "success to open " << attachment_path << "\n";
      }

      LongFilePositionType file_size= tm_file_stream.GetFileSize ();

      Byte* file_content= new Byte[file_size + 16];
      tm_file_stream.Read (file_content, file_size);

      string attachment_name= as_string (tail (attachment_paths[i]));

      PDFAttachment* aAttachment=
          new PDFAttachment (file_content, file_size, attachment_name);
      status= attachmentWriter.AttachToAllPage (aAttachment);
      if (status != eSuccess) {
        if (DEBUG_CONVERT)
          debug_convert << "fail to attach " << attachment_path << "\n";
        continue;
      }
      else {
        if (DEBUG_CONVERT)
          debug_convert << "success to attach " << attachment_path << "\n";
        continue;
      }
    }
    status= pdfWriter.EndPDF ();
  } while (false);

  if (eSuccess == status) {
    if (DEBUG_CONVERT) debug_convert << "Succeeded in creating PDF file\n";
    return true;
  }
  else {
    if (DEBUG_CONVERT) debug_convert << "Failed in creating PDF file\n";
    return false;
  }
}

PDFAttachment::PDFAttachment (void) {
  file_content= NULL;
  lenth       = 0;
}

PDFAttachment::PDFAttachment (string attachment_path) {
  InputFileStream tm_file_stream;
  EStatusCode     status= tm_file_stream.Open (as_charp (attachment_path));
  if (status != PDFHummus::eSuccess) {
    if (DEBUG_CONVERT) debug_convert << "failed to open tm\n";
    return;
  }

  lenth= (size_t) tm_file_stream.GetFileSize ();

  file_content= new Byte[lenth + 16];
  tm_file_stream.Read (file_content, lenth);

  int i, last_slash_index= 0;
  for (i= 0; i < N (attachment_path); i++) {
    if (attachment_path[i] == '/') last_slash_index= i;
  }
  name= attachment_path (last_slash_index + 1, N (attachment_path));
}
PDFAttachment::PDFAttachment (Byte inByte[], size_t inLenth, string inName) {
  file_content= inByte;
  lenth       = inLenth;
  name        = inName;
}

PDFAttachment::~PDFAttachment (void) {
  if (file_content) delete file_content;
}

PDFAttachmentWriter::PDFAttachmentWriter (PDFWriter* inPDFWriter) {
  mPDFWriter= inPDFWriter;
  ListenOnCatalogWrite ();
}

void
PDFAttachmentWriter::ListenOnCatalogWrite () {
  mPDFWriter->GetDocumentContext ().AddDocumentContextExtender (this);
}

PDFAttachmentWriter::~PDFAttachmentWriter (void) {
  if (N (mAttachment) != 0)
    TRACE_LOG ("PDFAttachmentWriter::~PDFAttachmentWriter, Exception. Has "
               "attachement that were not associated with pages");
  CleanupAttachment ();
}

EStatusCode
PDFAttachmentWriter::OnCatalogWrite (
    CatalogInformation* inCatalogInformation,
    DictionaryContext*  inCatalogDictionaryContext,
    ObjectsContext*     inPDFWriterObjectContext,
    DocumentContext*    inPDFWriterDocumentContext) {
  if (N (mAttachment) == 0) return eSuccess;

  inCatalogDictionaryContext->WriteKey ("Names");

  DictionaryContext* dictionaryContext_0=
      inPDFWriterObjectContext->StartDictionary ();
  dictionaryContext_0->WriteKey ("EmbeddedFiles");
  DictionaryContext* dictionaryContext_1=
      inPDFWriterObjectContext->StartDictionary ();
  dictionaryContext_0->WriteKey ("Names");
  inPDFWriterObjectContext->StartArray ();

  iterator<PDFAttachment*> it_0          = iterate (mAttachment);
  ObjectIDType             the_main_tm_id= 999999999;
  PDFAttachment*           the_main_tm   = NULL;
  while (it_0->busy ()) {
    PDFAttachment* cur_attachment= it_0->next ();
    if (the_main_tm_id > mAttachment (cur_attachment)) {
      the_main_tm   = cur_attachment;
      the_main_tm_id= mAttachment (cur_attachment);
    }
  }
  if (the_main_tm != NULL) {
    inPDFWriterObjectContext->WriteLiteralString (as_charp (the_main_tm->name));
    DictionaryContext* dictionaryContext_2=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_2->WriteKey ("EF");
    DictionaryContext* dictionaryContext_3=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_3->WriteKey ("F");
    inPDFWriterObjectContext->WriteIndirectObjectReference (
        mAttachment[the_main_tm]);
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_3);
    dictionaryContext_2->WriteKey ("F");
    dictionaryContext_2->WriteLiteralStringValue (as_charp (the_main_tm->name));
    dictionaryContext_2->WriteKey ("Type");
    dictionaryContext_2->WriteNameValue ("F");
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_2);
  }

  iterator<PDFAttachment*> it= iterate (mAttachment);
  while (it->busy ()) {
    PDFAttachment* cur_attachment= it->next ();
    if (cur_attachment == the_main_tm) continue;
    inPDFWriterObjectContext->WriteLiteralString (
        as_charp (cur_attachment->name));
    DictionaryContext* dictionaryContext_2=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_2->WriteKey ("EF");
    DictionaryContext* dictionaryContext_3=
        inPDFWriterObjectContext->StartDictionary ();
    dictionaryContext_3->WriteKey ("F");
    inPDFWriterObjectContext->WriteIndirectObjectReference (
        mAttachment[cur_attachment]);
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_3);
    dictionaryContext_2->WriteKey ("F");
    dictionaryContext_2->WriteLiteralStringValue (
        as_charp (cur_attachment->name));
    dictionaryContext_2->WriteKey ("Type");
    dictionaryContext_2->WriteNameValue ("F");
    inPDFWriterObjectContext->EndDictionary (dictionaryContext_2);
  }
  inPDFWriterObjectContext->EndArray ();
  inPDFWriterObjectContext->EndDictionary (dictionaryContext_1);
  inPDFWriterObjectContext->EndDictionary (dictionaryContext_0);
  CleanupAttachment ();
  return eSuccess;
}

void
PDFAttachmentWriter::CleanupAttachment () {

  iterator<PDFAttachment*> it= iterate (mAttachment);
  while (it->busy ()) {
    delete it->next ();
  }
  mAttachment= PDFAttachmentToObjectIDTypeMap ();
}

EStatusCode
PDFAttachmentWriter::AttachToAllPage (PDFAttachment* inAttachment) {
  return WriteAttachment (inAttachment).first;
}

EStatusCodeAndObjectIDType
PDFAttachmentWriter::WriteAttachment (PDFAttachment* inAttachment) {
  EStatusCodeAndObjectIDType result;
  if (mAttachment->contains (inAttachment)) {
    result.first = eSuccess;
    result.second= mAttachment[inAttachment];
    return result;
  }

  do {
    ObjectsContext& objectsContext= mPDFWriter->GetObjectsContext ();

    result.second= objectsContext.StartNewIndirectObject ();

    DictionaryContext* dictionaryContext= objectsContext.StartDictionary ();
    dictionaryContext->WriteKey ("Type");
    dictionaryContext->WriteNameValue ("EmbeddedFile");
    PDFStream*   pdfStream= objectsContext.StartPDFStream (dictionaryContext);
    IByteWriter* mwriter  = pdfStream->GetWriteStream ();
    mwriter->Write (inAttachment->file_content, inAttachment->lenth);
    objectsContext.EndPDFStream (pdfStream);

    mAttachment (inAttachment)= result.second;
    result.first              = eSuccess;
  } while (false);
  return result;
}
