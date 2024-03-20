/******************************************************************************
 * MODULE     : pdf_hummus_make_attachment.hpp
 * DESCRIPTION: Interface for embedding text files into pdf files
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PDF_HUMMUS_MAKE_ATTACHMENT_H
#define PDF_HUMMUS_MAKE_ATTACHMENT_H

#include "hashmap.hpp"
#include "iterator.hpp"
#include "list.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "url.hpp"

/**

Embed attachments into pdf in Embedded File Streams format
@param pdf_path The path of the PDF file where attachments need to be embedded.
@param attachment_path A list that specifies the paths of attachments to be
embedded.
@param out_path The path of the new PDF file with the embedded attachments.
@return Returns true if the embedding is successful, false otherwise.
*/
bool pdf_hummus_make_attachments (url pdf_path, array<url> attachment_path,
                                  url out_path);

#endif // ifdef PDF_HUMMUS_MAKE_ATTACHMENT_H
