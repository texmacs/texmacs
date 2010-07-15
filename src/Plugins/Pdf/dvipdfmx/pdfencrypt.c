/*  $Header: /home/cvsroot/dvipdfmx/src/pdfencrypt.c,v 1.14 2010/02/07 12:58:48 chofchof Exp $
 
    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2007 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef WIN32
#include <conio.h>
#define getch _getch
#else  /* !WIN32 */
#include <unistd.h>
#endif /* WIN32 */

#include "system.h"
#include "mem.h"
#include "error.h"
#include "pdfobj.h"
#include "dpxcrypt.h"

#include "pdfencrypt.h"

#define MAX_KEY_LEN 16
#define MAX_STR_LEN 32
#define MAX_PWD_LEN 128

static unsigned char algorithm, revision, key_size;
static long permission;

static unsigned char key_data[MAX_KEY_LEN], id_string[MAX_KEY_LEN];
static unsigned char opwd_string[MAX_STR_LEN], upwd_string[MAX_STR_LEN];

static unsigned long current_label = 0;
static unsigned current_generation = 0;

static ARC4_KEY key;
static MD5_CONTEXT md5_ctx;

static unsigned char md5_buf[MAX_KEY_LEN], key_buf[MAX_KEY_LEN];
static unsigned char in_buf[MAX_STR_LEN], out_buf[MAX_STR_LEN];

static const unsigned char padding_string[MAX_STR_LEN] = {
  0x28, 0xbf, 0x4e, 0x5e, 0x4e, 0x75, 0x8a, 0x41,
  0x64, 0x00, 0x4e, 0x56, 0xff, 0xfa, 0x01, 0x08,
  0x2e, 0x2e, 0x00, 0xb6, 0xd0, 0x68, 0x3e, 0x80,
  0x2f, 0x0c, 0xa9, 0xfe, 0x64, 0x53, 0x69, 0x7a
};

static char owner_passwd[MAX_PWD_LEN], user_passwd[MAX_PWD_LEN];

static unsigned char verbose = 0;

void pdf_enc_set_verbose (void)
{
  if (verbose < 255) verbose++;
}

#define PRODUCER "%s-%s, Copyright \251 2002-2010 by Jin-Hwan Cho, Matthias Franz, and Shunsaku Hirata"
void pdf_enc_compute_id_string (char *dviname, char *pdfname)
{
  char *date_string, *producer;
  time_t current_time;
  struct tm *bd_time;

  MD5_init(&md5_ctx);

  date_string = NEW (15, char);
  time(&current_time);
  bd_time = localtime(&current_time);
  sprintf (date_string, "%04d%02d%02d%02d%02d%02d",
	   bd_time -> tm_year+1900, bd_time -> tm_mon+1, bd_time -> tm_mday,
	   bd_time -> tm_hour, bd_time -> tm_min, bd_time -> tm_sec);
  MD5_write(&md5_ctx, (unsigned char *)date_string, strlen(date_string));
  RELEASE (date_string);

  producer = NEW (strlen(PRODUCER)+strlen(PACKAGE)+strlen(VERSION), char);
  sprintf(producer, PRODUCER, PACKAGE, VERSION);
  MD5_write(&md5_ctx, (unsigned char *)producer, strlen(producer));
  RELEASE (producer);

  MD5_write(&md5_ctx, (unsigned char *)dviname, strlen(dviname));
  MD5_write(&md5_ctx, (unsigned char *)pdfname, strlen(pdfname));
  MD5_final(id_string, &md5_ctx);
}

static void passwd_padding (unsigned char *src, unsigned char *dst)
{
  register int len = strlen((char *)src);

  if (len > MAX_STR_LEN)
    len = MAX_STR_LEN;

  memcpy(dst, src, len);
  memcpy(dst+len, padding_string, MAX_STR_LEN-len);
}

static void compute_owner_password (void)
{
  register unsigned char i, j;
  /*
   * Algorithm 3.3 Computing the encryption dictionary's O (owner password)
   *               value
   *
   * 1. Pad or truncate the owner password string as described in step 1
   *    of Algorithm 3.2. If there is no owner password, use the user
   *    password instead. (See implementation note 17 in Appendix H.)
   */
  passwd_padding((unsigned char *)(strlen(owner_passwd) > 0 ? owner_passwd : user_passwd), in_buf);
  /*
   * 2. Initialize the MD5 hash function and pass the result of step 1
   *    as input to this function.
   */
  MD5_init(&md5_ctx);
  MD5_write(&md5_ctx, in_buf, MAX_STR_LEN);
  MD5_final(md5_buf, &md5_ctx);
  /*
   * 3. (Revision 3 only) Do the following 50 times: Take the output
   *    from the previous MD5 hash and pass it as input into a new
   *    MD5 hash.
   */
  if (revision == 3)
    for (i = 0; i < 50; i++) {
      /*
       * NOTE: We truncate each MD5 hash as in the following step.
       *       Otherwise Adobe Reader won't decrypt the PDF file.
       */
      MD5_init(&md5_ctx);
      MD5_write(&md5_ctx, md5_buf, key_size);
      MD5_final(md5_buf, &md5_ctx);
    }
  /*
   * 4. Create an RC4 encryption key using the first n bytes of the output
   *    from the final MD5 hash, where n is always 5 for revision 2 but
   *    for revision 3 depends on the value of the encryption dictionary's
   *    Length entry.
   */
  ARC4_set_key(&key, key_size, md5_buf);
  /*
   * 5. Pad or truncate the user password string as described in step 1
   *    of Algorithm 3.2.
   */
  passwd_padding((unsigned char *)user_passwd, in_buf);
  /*
   * 6. Encrypt the result of step 5, using an RC4 encryption function
   *    with the encryption key obtained in step 4.
   */
  ARC4(&key, MAX_STR_LEN, in_buf, out_buf);
  /*
   * 7. (Revision 3 only) Do the following 19 times: Take the output
   *    from the previous invocation of the RC4 function and pass it
   *    as input to a new invocation of the function; use an encryption
   *    key generated by taking each byte of the encryption key obtained
   *    in step 4 and performing an XOR (exclusive or) operation between
   *    that byte and the single-byte value of the iteration counter
   *    (from 1 to 19).
   */
  if (revision == 3)
    for (i = 1; i <= 19; i++) {
      memcpy(in_buf, out_buf, MAX_STR_LEN);
      for (j = 0; j < key_size; j++)
        key_buf[j] = md5_buf[j] ^ i;
      ARC4_set_key(&key, key_size, key_buf);
      ARC4(&key, MAX_STR_LEN, in_buf, out_buf);
    }
  /*
   * 8. Store the output from the final invocation of the RC4 function
   *    as the value of the O entry in the encryption dictionary.
   */
  memcpy(opwd_string, out_buf, MAX_STR_LEN);
}

static void compute_encryption_key (unsigned char *pwd)
{
  register unsigned char i;
  /*
   * Algorithm 3.2 Computing an encryption key
   *
   * 1. Pad or truncate the password string to exactly 32 bytes. If the
   *    password string is more than 32 bytes long, use only its first
   *    32 bytes; if it is less than 32 bytes long, pad it by appending
   *    the required number of additional bytes from the beginning of
   *    the following padding string:
   *
   *    < 28 BF 4E 5E 4E 75 8A 41 64 00 4E 56 FF FA 01 08
   *      2E 2E 00 B6 D0 68 3E 80 2F 0C A9 FE 64 53 69 7A >
   *
   *    That is, if the password string is n bytes long, append the
   *    first 32 - n bytes of the padding string to the end of the
   *    password string. If the password string is empty (zero-length),
   *	meaning there is no user password, substitute the entire
   *	padding string in its place.
   */
  passwd_padding(pwd, in_buf);
  /*
   * 2. Initialize the MD5 hash function and pass the result of step 1
   *    as input to this fuction.
   */
  MD5_init(&md5_ctx);
  MD5_write(&md5_ctx, in_buf, MAX_STR_LEN);
  /*
   * 3. Pass the value of the encryption dictionary's O entry to the
   *    MD5 hash function. (Algorithm 3.3 shows how the O value is
   *    computed.)
   */
  MD5_write(&md5_ctx, opwd_string, MAX_STR_LEN);
  /*
   * 4. Treat the value of the P entry as an unsigned 4-byte integer
   *    and pass these bytes to the MD5 hash function, low-order byte
   *    first.
   */
  in_buf[0] = (unsigned char)(permission) & 0xFF;
  in_buf[1] = (unsigned char)(permission >> 8) & 0xFF;
  in_buf[2] = (unsigned char)(permission >> 16) & 0xFF;
  in_buf[3] = (unsigned char)(permission >> 24) & 0xFF;
  MD5_write(&md5_ctx, in_buf, 4);
  /*
   * 5. Pass the first element of the file's file identifier array
   *    (the value of the ID entry in the document's trailer dictionary;
   *    see Table 3.12 on page 68) to the MD5 hash function and
   *    finish the hash.
   */
  MD5_write(&md5_ctx, id_string, MAX_KEY_LEN);
  MD5_final(md5_buf, &md5_ctx);
  /*
   * 6. (Revision 3 only) Do the following 50 times; Take the output from
   *    the previous MD5 hash and pass it as input into a new MD5 hash.
   */
  if (revision == 3)
    for (i = 0; i < 50; i++) {
      /*
       * NOTE: We truncate each MD5 hash as in the following step.
       *       Otherwise Adobe Reader won't decrypt the PDF file.
       */
      MD5_init(&md5_ctx);
      MD5_write(&md5_ctx, md5_buf, key_size);
      MD5_final(md5_buf, &md5_ctx);
    }
  /*
   * 7. Set the encryption key to the first n bytes of the output from
   *    the final MD5 hash, where n is always 5 for revision 2 but for
   *    revision 3 depends on the value of the encryption dictionary's
   *    Length entry.
   */
  memcpy(key_data, md5_buf, key_size);
}

static void compute_user_password (void)
{
  register unsigned char i, j;
  /*
   * Algorithm 3.4 Computing the encryption dictionary's U (user password)
   *               value (Revision 2)
   *
   * 1. Create an encryption key based on the user password string, as
   *    described in Algorithm 3.2.
   *
   * 2. Encrypt the 32-byte padding string shown in step 1 of Algorithm
   *    3.2, using an RC4 encryption fuction with the encryption key from
   *    the preceeding step.
   *
   * 3. Store the result of step 2 as the value of the U entry in the
   *    encryption dictionary.
   */
  /*
   * Algorithm 3.5 Computing the encryption dictionary's U (user password)
   *               value (Revision 3)
   *
   * 1. Create an encryption key based on the user password string, as
   *    described in Algorithm 3.2.
   *
   * 2. Initialize the MD5 hash function and pass the 32-byte padding
   *    string shown in step 1 of Algorithm 3.2 as input to this function.
   *
   * 3. Pass the first element of the file's file identifier array (the
   *    value of the ID entry in the document's trailer dictionary; see
   *    Table 3.12 on page 68) to the hash function and finish the hash.
   *
   * 4. Encrypt the 16-byte result of the hash, using an RC4 encryption
   *    function with the encryption key from step 1.
   *
   * 5. Do the following 19 times: Take the output from the previous
   *    invocation of the RC4 function and pass it as input to a new
   *    invocation of the function; use an encryption key generated by
   *    taking each byte of the original encryption key (obtained in
   *    step 1) and performing an XOR (exclusive or) operation between
   *    that byte and the single-byte value of the iteration counter
   *    (from 1 to 19).
   *
   * 6. Append 16 bytes of arbitrary padding to the output from the
   *    final invocation of the RC4 function and store the 32-byte
   *    result as the value of the U entry in the encryption dictionary.
   */
  compute_encryption_key((unsigned char *)user_passwd);

  switch (revision) {
  case 2:
    ARC4_set_key(&key, key_size, key_data);
    ARC4(&key, MAX_STR_LEN, padding_string, out_buf);
    break;
  case 3:
    MD5_init(&md5_ctx);
    MD5_write(&md5_ctx, (unsigned char *)padding_string, MAX_STR_LEN);

    MD5_write(&md5_ctx, id_string, MAX_KEY_LEN);
    MD5_final(md5_buf, &md5_ctx);

    ARC4_set_key(&key, key_size, key_data);
    ARC4(&key, MAX_KEY_LEN, md5_buf, out_buf);

    for (i = 1; i <= 19; i++) {
      memcpy(in_buf, out_buf, MAX_KEY_LEN);
      for (j = 0; j < key_size; j++)
        key_buf[j] = key_data[j] ^ i;
      ARC4_set_key(&key, key_size, key_buf);
      ARC4(&key, MAX_KEY_LEN, in_buf, out_buf);
    }
    break;
  default:
    ERROR("Invalid revision number.\n");
  }

  memcpy(upwd_string, out_buf, MAX_STR_LEN);
}

#ifdef WIN32
static char *getpass (const char *prompt)
{
  static char pwd_buf[128];
  size_t i;

  fputs(prompt, stderr);
  fflush(stderr);
  for (i = 0; i < sizeof(pwd_buf)-1; i++) {
    pwd_buf[i] = getch();
    if (pwd_buf[i] == '\r')
      break;
  }
  pwd_buf[i] = '\0';
  fputs("\n", stderr);
  return pwd_buf;
}
#endif

void pdf_enc_set_passwd (unsigned bits, unsigned perm, char *dviname, char *pdfname)
{
  register char *retry_passwd;

  while (1) {
    strcpy(owner_passwd, getpass("Owner password: "));
    retry_passwd = getpass("Re-enter owner password: ");
    if (!strcmp(owner_passwd, retry_passwd))
      break;
    fputs("Password is not identical.\nTry again.\n", stderr);
    fflush(stderr);
  }

  while (1) {
    strcpy(user_passwd, getpass("User password: "));
    retry_passwd = getpass("Re-enter user password: ");
    if (!strcmp(user_passwd, retry_passwd))
      break;
    fputs("Password is not identical.\nTry again.\n", stderr);
    fflush(stderr);
  }

  key_size = (unsigned char)(bits / 8);
  algorithm = (key_size == 5 ? 1 : 2);
  permission = (long) (perm | 0xC0U);
  revision = ((algorithm == 1 && permission < 0x100L) ? 2 : 3);
  if (revision == 3)
    permission |= ~0xFFFL;

  compute_owner_password();
  compute_user_password();
}

void pdf_encrypt_data (unsigned char *data, unsigned long len)
{
  unsigned char *result;

  memcpy(in_buf, key_data, key_size);
  in_buf[key_size]   = (unsigned char)(current_label) & 0xFF;
  in_buf[key_size+1] = (unsigned char)(current_label >> 8) & 0xFF;
  in_buf[key_size+2] = (unsigned char)(current_label >> 16) & 0xFF;
  in_buf[key_size+3] = (unsigned char)(current_generation) & 0xFF;
  in_buf[key_size+4] = (unsigned char)(current_generation >> 8) & 0xFF;

  MD5_init(&md5_ctx);
  MD5_write(&md5_ctx, in_buf, key_size+5);
  MD5_final(md5_buf, &md5_ctx);
  
  result = NEW (len, unsigned char);
  ARC4_set_key(&key, (key_size > 10 ? MAX_KEY_LEN : key_size+5), md5_buf);
  ARC4(&key, len, data, result);
  memcpy(data, result, len);
  RELEASE (result);
}

pdf_obj *pdf_encrypt_obj (void)
{
  pdf_obj *doc_encrypt;

#ifdef DEBUG
  fprintf (stderr, "(pdf_encrypt_obj)");
#endif

  doc_encrypt = pdf_new_dict ();

  /* KEY  : Filter
   * TYPE : name
   * VALUE: (Required) The name of the security handler for this document;
   *        see below. Default value: Standard, for the built-in security
   *        handler.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("Filter"),
		pdf_new_name ("Standard"));
  /* KEY  : V
   * TYPE : number
   * VALUE: (Optional but strongly recommended) A code specifying the
   *        algorithm to be used in encrypting and decrypting the document:
   *        0  An algorithm that is undocumented and no longer supported,
   *           and whose use is strongly discouraged.
   *        1  Algorithm 3.1 on page 73, with an encryption key length
   *           of 40 bits; see below.
   *        2  (PDF 1.4) Algorithm 3.1 on page 73, but allowing encryption
   *           key lengths greater than 40 bits.
   *        3  (PDF 1.4) An unpublished algorithm allowing encryption key
   *           lengths ranging from 40 to 128 bits. (This algorithm is
   *           unpublished as an export requirement of the U.S. Department
   *           of Commerce.)
   *        The default value if this entry is omitted is 0, but a value
   *        of 1 or greater is strongly recommended.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("V"),
		pdf_new_number (algorithm));
  /* KEY  : Length
   * TYPE : integer
   * VALUE: (Optional; PDF 1.4; only if V is 2 or 3) The length of the
   *        encryption key, in bits. The value must be a multiple of 8,
   *        in the range 40 to 128. Default value: 40.
   */
  if (algorithm > 1)
    pdf_add_dict (doc_encrypt, 
		  pdf_new_name ("Length"),
		  pdf_new_number (key_size * 8));
  /* KEY  : R
   * TYPE : number
   * VALUE: (Required) A number specifying which revision of the standard
   *        security handler should be used to interpret this dictionary.
   *        The revison number should be 2 if the document is encrypted
   *        with a V value less than 2; otherwise this value should be 3.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("R"),
		pdf_new_number (revision));
  /* KEY  : O
   * TYPE : string
   * VALUE: (Required) A 32-byte string, based on both the owner and
   *        user passwords, that is used in computing the encryption
   *        key and in determining whether a valid owner password was
   *        entered.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("O"),
		pdf_new_string (opwd_string, 32));
  /* KEY  : U
   * TYPE : string
   * VALUE: (Required) A 32-byte string, based on the user password,
   *        that is used in determining whether to prompt the user
   *        for a password and, if so, whether a valid user or owner
   *        password was entered.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("U"),
		pdf_new_string (upwd_string, 32));
  /* KEY  : P
   * TYPE : (signed 32 bit) integer
   * VALUE: (Required) A set of flags specifying which operations are
   *        permitted when the document is opened with user access.
   */
  pdf_add_dict (doc_encrypt, 
		pdf_new_name ("P"),
		pdf_new_number (permission));

  return doc_encrypt;
}

pdf_obj *pdf_enc_id_array (void)
{
  pdf_obj *id = pdf_new_array();
  pdf_add_array(id, pdf_new_string(id_string, MAX_KEY_LEN));
  pdf_add_array(id, pdf_new_string(id_string, MAX_KEY_LEN));
  return id;
}

void pdf_enc_set_label (unsigned long label)
{
  current_label = label;
}

void pdf_enc_set_generation (unsigned generation)
{
  current_generation = generation;
}
