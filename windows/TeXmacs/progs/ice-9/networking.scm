;;; installed-scm-file

;;;; Copyright (C) 1999 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

(define (gethostbyaddr addr) (gethost addr))
(define (gethostbyname name) (gethost name))

(define (getnetbyaddr addr) (getnet addr))
(define (getnetbyname name) (getnet name))

(define (getprotobyname name) (getproto name))
(define (getprotobynumber addr) (getproto addr))

(define (getservbyname name proto) (getserv name proto))
(define (getservbyport port proto) (getserv port proto))

(define (sethostent . stayopen) 
  (if (pair? stayopen)
      (sethost (car stayopen))
      (sethost #f)))
(define (setnetent . stayopen) 
  (if (pair? stayopen)
      (setnet (car stayopen))
      (setnet #f)))
(define (setprotoent . stayopen) 
  (if (pair? stayopen)
      (setproto (car stayopen))
      (setproto #f)))
(define (setservent . stayopen) 
  (if (pair? stayopen)
      (setserv (car stayopen))
      (setserv #f)))

(define (gethostent) (gethost))
(define (getnetent) (getnet))
(define (getprotoent) (getproto))
(define (getservent) (getserv))

(define (endhostent) (sethost))
(define (endnetent) (setnet))
(define (endprotoent) (setproto))
(define (endservent) (setserv))

(define (hostent:name obj) (vector-ref obj 0))
(define (hostent:aliases obj) (vector-ref obj 1))
(define (hostent:addrtype obj) (vector-ref obj 2))
(define (hostent:length obj) (vector-ref obj 3))
(define (hostent:addr-list obj) (vector-ref obj 4))

(define (netent:name obj) (vector-ref obj 0))
(define (netent:aliases obj) (vector-ref obj 1))
(define (netent:addrtype obj) (vector-ref obj 2))
(define (netent:net obj) (vector-ref obj 3))

(define (protoent:name obj) (vector-ref obj 0))
(define (protoent:aliases obj) (vector-ref obj 1))
(define (protoent:proto obj) (vector-ref obj 2))

(define (servent:name obj) (vector-ref obj 0))
(define (servent:aliases obj) (vector-ref obj 1))
(define (servent:port obj) (vector-ref obj 2))
(define (servent:proto obj) (vector-ref obj 3))

(define (sockaddr:fam obj) (vector-ref obj 0))
(define (sockaddr:path obj) (vector-ref obj 1))
(define (sockaddr:addr obj) (vector-ref obj 1))
(define (sockaddr:port obj) (vector-ref obj 2))
