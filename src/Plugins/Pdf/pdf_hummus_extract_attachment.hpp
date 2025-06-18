/******************************************************************************
 * MODULE     : pdf_hummus_extract_attachment.cpp
 * DESCRIPTION: Interface for extract attachment file in pdf
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/
#ifndef PDF_HUMMUS_GET_ATTACHMENT_H
#define PDF_HUMMUS_GET_ATTACHMENT_H
#include "hashmap.hpp"
#include "iterator.hpp"
#include "list.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "tree.hpp"
#include "url.hpp"

#ifdef PDF_RENDERER
/**

@brief Extracts attachments from a PDF file.
@param pdf_path The path of the PDF file.
@param names A reference to a list that will store the paths of extracted
attachments.
@return Returns true if the extraction is successful, false otherwise.
*/
bool extract_attachments_from_pdf (url pdf_path, list<url>& names);

/**

@brief Extracts attachments from a PDF file in a simplified way for SCM
glueoperations.
@param pdf_path The path of the PDF file.
@return Returns true if the extraction is successful, false otherwise.
*/
bool scm_extract_attachments (url pdf_path);

/**

@brief Retrieves the absolute paths of linked files in the texmacs tree based on
the specified directory path.
@param t The texmacs tree to be traversed.
@param path The directory path where the tree's TM file is located.
@return An array of URLs representing the absolute paths of linked files.
@note
This function performs a depth-first traversal to search for the URLs of all
linked files in the given t tree. It generates the absolute paths of these
linked files based on the specified path where the tree's TM file is located.
*/
array<url> get_linked_file_paths (tree t, url path);

/**

@brief Replaces the relative paths of linked files in the texmacs tree with the
absolute path relative to the specified directory.
@param t The texmacs tree to be traversed.
@param path The target directory path.
@return The tree after the replacements.
@note
This function searches for the URLs of all linked files in the given t tree
using a depth-first traversal. It replaces these URLs by converting them to be
relative to the path directory.
*/
tree replace_with_relative_path (tree t, url path);

/**

@brief Retrieves the main TM (TeXmacs) file embedded in a PDF, based on the
provided PDF path.
@param pdf_path The path to the PDF file containing the embedded TM documents.
@return The URL of the main TM file embedded in the PDF.
@note
This function retrieves the main TM (TeXmacs) file from a PDF file that contains
embedded TM documents. The first file in the array of embedded files is
considered as the main TM file, due to the mechanism of embedding TM documents
in Mogan.
*/
url get_main_tm (url pdf_path);

#else
/*
 * when the pdf plugin is not enabled, you can still include the pdf headers files.
 * in that case the pdf functions will alaways return an error.
 */
inline bool extract_attachments_from_pdf (url pdf_path, list<url>& names) {
    return false;
}

inline bool scm_extract_attachments (url pdf_path) {
    return false;
}

inline array<url> get_linked_file_paths (tree t, url path) {
    return array<url>();
}

inline tree replace_with_relative_path (tree t, url path) {
    return t;
}

inline url get_main_tm (url pdf_path) {
    return url_none();
}
#endif // ifdef PDF_RENDERER

#endif // ifdef PDF_HUMMUS_MAKE_ATTACHMENT_H
