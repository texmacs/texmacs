/* classes: h_files */

#ifndef SCM_NULL_THREADS_H
#define SCM_NULL_THREADS_H

/* Copyright (C) 2005, 2006 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



/* The null-threads implementation.  We provide the subset of the
   standard pthread API that is used by Guile, but no new threads can
   be created.

   This file merely exits so that Guile can be compiled and run
   without using pthreads.  Improving performance via optimizations
   that are possible in a single-threaded program is not a primary
   goal.
*/

#include <errno.h>

/* Threads 
*/
#define scm_i_pthread_t                     int
#define scm_i_pthread_self()                0
#define scm_i_pthread_create(t,a,f,d)       (*(t)=0, (void)(f), ENOSYS)
#define scm_i_pthread_detach(t)             do { } while (0)
#define scm_i_pthread_exit(v)               exit(0)
#define scm_i_sched_yield()                 0

/* Signals
 */
#define scm_i_pthread_sigmask               sigprocmask

/* Mutexes
 */
#define SCM_I_PTHREAD_MUTEX_INITIALIZER     0
#define scm_i_pthread_mutex_t               int
#define scm_i_pthread_mutex_init(m,a)       (*(m) = 0)
#define scm_i_pthread_mutex_destroy(m)      do { (void)(m); } while(0)
#define scm_i_pthread_mutex_trylock(m)      ((*(m))++)
#define scm_i_pthread_mutex_lock(m)         ((*(m))++)
#define scm_i_pthread_mutex_unlock(m)       ((*(m))--)
#define scm_i_pthread_mutexattr_recursive   0

/* Condition variables
 */
#define SCM_I_PTHREAD_COND_INITIALIZER      0
#define scm_i_pthread_cond_t                int
#define scm_i_pthread_cond_init(c,a)        (*(c) = 0)
#define scm_i_pthread_cond_destroy(c)       do { (void)(c); } while(0)
#define scm_i_pthread_cond_signal(c)        (*(c) = 1)
#define scm_i_pthread_cond_broadcast(c)     (*(c) = 1)
#define scm_i_pthread_cond_wait(c,m)        (abort(), 0)
#define scm_i_pthread_cond_timedwait(c,m,t) (abort(), 0)

/* Onces
 */
#define scm_i_pthread_once_t                int
#define SCM_I_PTHREAD_ONCE_INIT             0
#define scm_i_pthread_once(o,f)             do { \
                                              if(!*(o)) { *(o)=1; f (); } \
                                            } while(0)

/* Thread specific storage
 */
typedef struct scm_i_pthread_key_t {
  struct scm_i_pthread_key_t *next;
  void *value;
  void (*destr_func) (void *);
} scm_i_pthread_key_t;

SCM_API int scm_i_pthread_key_create (scm_i_pthread_key_t *key,
				      void (*destr_func) (void *));
#define scm_i_pthread_setspecific(k,p)      ((k).value = (p))
#define scm_i_pthread_getspecific(k)        ((k).value)

/* Convenience functions
 */
#define scm_i_scm_pthread_mutex_lock        scm_i_pthread_mutex_lock
#define scm_i_dynwind_pthread_mutex_lock    scm_i_pthread_mutex_lock
#define scm_i_scm_pthread_cond_wait         scm_i_pthread_cond_wait
#define scm_i_scm_pthread_cond_timedwait    scm_i_pthread_cond_timedwait


#endif  /* SCM_NULL_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
