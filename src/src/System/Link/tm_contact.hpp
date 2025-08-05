
/******************************************************************************
* MODULE     : tm_contact.hpp
* DESCRIPTION: Contact for sockets
* COPYRIGHT  : (C) 2022  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_CONTACT_H
#define TM_CONTACT_H

#include "sys_utils.hpp"

/******************************************************************************
* Socket types
******************************************************************************/

#define SOCKET_DEFAULT  0
#define SOCKET_CLIENT   1
#define SOCKET_SERVER   2

/******************************************************************************
* The tm_contact class
******************************************************************************/

struct tm_contact_rep: abstract_struct {
  int type;
  array<array<string> > args;
public:
  tm_contact_rep (array<array<string> > args2=
		  array<array<string> > ()) :
    type (0), args (args2) {};
  virtual ~tm_contact_rep () {}
  virtual void start (int io) = 0;
  virtual void stop () = 0;
  virtual int send (const void* buffer, size_t length) = 0;
  virtual int receive (void* buffer, size_t length) = 0;
  virtual bool active () = 0;
  virtual string last_error () = 0;
};

class tm_contact {
public:
  ABSTRACT_NULL(tm_contact);
  inline bool operator == (tm_contact s);
  inline bool operator != (tm_contact s);
};

ABSTRACT_NULL_CODE(tm_contact);
inline bool tm_contact::operator == (tm_contact l) { return rep == l.rep; }
inline bool tm_contact::operator != (tm_contact l) { return rep != l.rep; }

inline bool is_server (tm_contact contact) {
return contact.rep -> type == SOCKET_SERVER; }

inline void start (tm_contact& contact, int io) {
  contact.rep -> start (io); }

inline void stop (tm_contact& contact) {
  contact.rep -> stop (); }

inline int send (tm_contact contact, const void* buffer, size_t length) {
  return contact.rep -> send (buffer, length); }

inline int receive (tm_contact contact, void* buffer, size_t length) {
  return contact.rep -> receive (buffer, length); }

inline bool is_active (tm_contact contact) {
  return contact.rep -> active (); }

inline string last_error (tm_contact contact) {
  return contact.rep -> last_error (); }

#endif // TM_CONTACT_H
