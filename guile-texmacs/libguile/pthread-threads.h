/* classes: h_files */

#ifndef SCM_PTHREADS_THREADS_H
#define SCM_PTHREADS_THREADS_H

/* Copyright (C) 2002, 2005, 2006 Free Software Foundation, Inc.
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



/* The pthreads-threads implementation.  This is a direct mapping.
*/

#include <pthread.h>
#include <sched.h>

/* Threads 
*/
#define scm_i_pthread_t                     pthread_t
#define scm_i_pthread_self                  pthread_self
#define scm_i_pthread_create                pthread_create
#define scm_i_pthread_detach                pthread_detach
#define scm_i_pthread_exit                  pthread_exit
#define scm_i_sched_yield                   sched_yield

/* Signals
 */
#define scm_i_pthread_sigmask               pthread_sigmask

/* Mutexes
 */
#if SCM_NEED_BRACES_ON_PTHREAD_MUTEX_INITIALIZER
# define SCM_I_PTHREAD_MUTEX_INITIALIZER     { PTHREAD_MUTEX_INITIALIZER }
#else
# define SCM_I_PTHREAD_MUTEX_INITIALIZER     PTHREAD_MUTEX_INITIALIZER
#endif
#define scm_i_pthread_mutex_t               pthread_mutex_t
#define scm_i_pthread_mutex_init            pthread_mutex_init
#define scm_i_pthread_mutex_destroy         pthread_mutex_destroy
#define scm_i_pthread_mutex_trylock         pthread_mutex_trylock
#define scm_i_pthread_mutex_lock            pthread_mutex_lock
#define scm_i_pthread_mutex_unlock          pthread_mutex_unlock
extern pthread_mutexattr_t scm_i_pthread_mutexattr_recursive[1];

/* Condition variables
 */
#define SCM_I_PTHREAD_COND_INITIALIZER      PTHREAD_COND_INITIALIZER
#define scm_i_pthread_cond_t                pthread_cond_t
#define scm_i_pthread_cond_init             pthread_cond_init
#define scm_i_pthread_cond_destroy          pthread_cond_destroy
#define scm_i_pthread_cond_signal           pthread_cond_signal
#define scm_i_pthread_cond_broadcast        pthread_cond_broadcast
#define scm_i_pthread_cond_wait             pthread_cond_wait
#define scm_i_pthread_cond_timedwait        pthread_cond_timedwait

/* Onces
 */
#define scm_i_pthread_once_t                pthread_once_t
#define scm_i_pthread_once                  pthread_once
#if SCM_NEED_BRACES_ON_PTHREAD_ONCE_INIT
#define SCM_I_PTHREAD_ONCE_INIT             { PTHREAD_ONCE_INIT }
#else
#define SCM_I_PTHREAD_ONCE_INIT             PTHREAD_ONCE_INIT
#endif

/* Thread specific storage
 */
#define scm_i_pthread_key_t                 pthread_key_t
#define scm_i_pthread_key_create            pthread_key_create
#define scm_i_pthread_setspecific           pthread_setspecific
#define scm_i_pthread_getspecific           pthread_getspecific

/* Convenience functions
 */
#define scm_i_scm_pthread_mutex_lock        scm_pthread_mutex_lock
#define scm_i_dynwind_pthread_mutex_lock    scm_dynwind_pthread_mutex_lock
#define scm_i_scm_pthread_cond_wait         scm_pthread_cond_wait
#define scm_i_scm_pthread_cond_timedwait    scm_pthread_cond_timedwait

#endif  /* SCM_PTHREADS_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
