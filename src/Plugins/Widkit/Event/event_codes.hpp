
/******************************************************************************
* MODULE     : event_codes.hpp
* DESCRIPTION: Unique codes for each event and
*              modules for creating the corresponding events.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EVENT_CODES_H
#define EVENT_CODES_H

/*** basic events ***/
#define GET_SIZE_EVENT        0x00000000
#define GET_WIDGET_EVENT      0x00000001
#define SET_WIDGET_EVENT      0x00000002
#define ATTACH_WINDOW_EVENT   0x00000003
#define POSITION_EVENT        0x00000004
#define MOVE_EVENT            0x00000005
#define RESIZE_EVENT          0x00000006
#define DESTROY_EVENT         0x00000007
#define KEYPRESS_EVENT        0x00000008
#define KEYBOARD_FOCUS_EVENT  0x00000009
#define MOUSE_EVENT           0x0000000a
#define ALARM_EVENT           0x0000000b
#define CLEAR_EVENT           0x0000000c
#define REPAINT_EVENT         0x0000000d
#define UPDATE_EVENT          0x0000000e
#define REFRESH_EVENT         0x0000000f
#define INVALIDATE_EVENT      0x00000010
#define KEYBOARD_GRAB_EVENT   0x00000011
#define MOUSE_GRAB_EVENT      0x00000012
#define REQUEST_ALARM_EVENT   0x00000013
#define FIND_CHILD_EVENT      0x00000014

/*** composite events ***/
#define CLEAN_EVENT           0x00000015
#define INSERT_EVENT          0x00000016
#define REMOVE_EVENT          0x00000017

/*** attribute events ***/
#define GET_INTEGER_EVENT     0x00000018
#define GET_DOUBLE_EVENT      0x00000019
#define GET_STRING_EVENT      0x0000001a
#define GET_COORD1_EVENT      0x0000001b
#define GET_COORD2_EVENT      0x0000001c
#define GET_COORD3_EVENT      0x0000001d
#define GET_COORD4_EVENT      0x0000001e
#define SET_INTEGER_EVENT     0x0000001f
#define SET_DOUBLE_EVENT      0x00000020
#define SET_STRING_EVENT      0x00000021
#define SET_COORD1_EVENT      0x00000022
#define SET_COORD2_EVENT      0x00000023
#define SET_COORD3_EVENT      0x00000024
#define SET_COORD4_EVENT      0x00000025

/*** scroll events ***/
#define SCROLL_EVENT          0x00000026

/*** shared implementation for event_ptr<> ***/
template<class R> struct event_ptr;

class event_ptr_base {
  event_rep* rep;
public:
  event_ptr_base (const event_ptr_base &ev) :
    rep (ev.rep) { rep->ref_count++; }
  event_ptr_base (const event& ev) : rep (ev.rep) { rep->ref_count++; }
  ~event_ptr_base () { if ((--rep->ref_count)==0) tm_delete (rep); }
  operator event () { return event (rep); }
  event_rep* operator -> () { return rep; }
  event_ptr_base& operator = (const event_ptr_base& ev) {
    ev.rep->ref_count++;
    if ((--rep->ref_count)==0) tm_delete (rep);
    rep=ev.rep;
    return *this;
  }
};

/*** event_ptr template ***/
template<class R> struct event_ptr : private event_ptr_base {
  inline event_ptr (const event_ptr<R>& ev) : event_ptr_base(ev) {}
  inline event_ptr (const event& ev) : event_ptr_base(ev) {}
  inline ~event_ptr () {}
  inline event_ptr<R>& operator = (const event_ptr<R>& ev) {
    return static_cast<event_ptr<R>&>(event_ptr_base::operator=(ev)); }
  inline operator event()  {
    return event_ptr_base::operator event(); }
  inline R* operator -> () {
    return static_cast<R*>(event_ptr_base::operator->()); }
};

#define EVENT(PTR) typedef event_ptr<PTR##_rep> PTR; 

#endif // defined EVENT_CODES_H
