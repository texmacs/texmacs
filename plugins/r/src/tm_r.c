
/******************************************************************************
* MODULE     : tm_r.c
* DESCRIPTION: Glue between TeXmacs and R
* COPYRIGHT  : (C) 2003 Michael Lachmann Tamarlin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <sys/select.h>
#if HAVE_PTY_H
#include <pty.h>
#endif

#include <utmp.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <util.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

/* do we want to talk with texmacs, or with a human? */
#define INTERFACE_TEXMACS
#define USE_DEBUG

#ifdef INTERFACE_TEXMACS
#define DATA_BEGIN (2)
#define DATA_END (5)
#define DATA_COMMAND (16)
#else
#define DATA_BEGIN '{'
#define DATA_END '}'
#define DATA_COMMAND '@'
#endif

#define IN_VERBATIM( TXB ) { if( in_data == 0 ) { in_data=1 ; copy_to_B( TXB, st, snprintf(st,4096,"%cverbatim:",DATA_BEGIN) ) ; } }
#define END_VERBATIM( TXB ) { while( in_data>0 ) { copy_to_B( TXB, st, snprintf(st,4096,"%c",DATA_END) ) ; in_data-- ;}}
#define B_DATA_BEGIN( TXB ) { copy_to_B( TXB, st, snprintf(st,4096,"%c",DATA_BEGIN) ); in_data++; }
#define B_DATA_END( TXB ) { copy_to_B( TXB, st, snprintf(st,4096,"%c",DATA_END) ); in_data--; }


#ifndef USE_DEBUG
#define WRITELOG(s,n)
int DEBUG = (1==0) ;

/* #define VERBOSE_TERM */
#undef VERBOSE_TERM
#else
#define WRITELOG(s,n) {if( DEBUG ) write(LOG,s,n);}
int LOG ;
int DEBUG = (1==1) ;
#endif

char st[4096] ;

char *temp_buf;

tcflag_t prompt_flags[][4] = {
{ 00500,  000005, 0277,      0005021},
{00000,  000005, 0277,      0105061},
{00000,  000004, 0277,      0005061},
{02400,  014005, 0277,      0105001},
{0,          05, 0277, 037777763425},
{-1,-1,-1,-1}
} ;



int check_terminal( int f )
{
  int p_i ;
  int got_prompt = (1==0) ;
  struct termios termi ;
  tcgetattr(f, &termi ) ;
  WRITELOG(st,snprintf(st,4096,"term:i:%o o:%o c:%o l:%o\n",
		       (unsigned int) termi.c_iflag,
		       (unsigned int) termi.c_oflag,
		       (unsigned int) termi.c_cflag,
		       (unsigned int) termi.c_lflag)) ;
  
  /* check if we got the prompt from R */
  for( p_i = 0; prompt_flags[p_i][0]!=-1; p_i++) {  /* check terminal flags */
    WRITELOG(st,snprintf(st,4096,"p_i = %d %d\n", p_i,
			 (int) prompt_flags[p_i][0])) ;	
    WRITELOG(st,snprintf(st,4096,"prompt:i:%o o:%o c:%o l:%o\n",
			 (unsigned int) prompt_flags[p_i][0],
			 (unsigned int) prompt_flags[p_i][1],
			 (unsigned int) prompt_flags[p_i][2],
			 (unsigned int) prompt_flags[p_i][3])) ;	    
    if( (termi.c_iflag == prompt_flags[p_i][0]) &&
	   (termi.c_oflag == prompt_flags[p_i][1]) &&
	   (termi.c_cflag == prompt_flags[p_i][2]) &&
	   (termi.c_lflag = prompt_flags[p_i][3]) ) {
	  got_prompt = (1==1) ;
	  WRITELOG(st,snprintf(st,4096,"found prompt 1\n"))	 
	  break ; 
	} else {
	  WRITELOG(st,snprintf(st,4096,"no prompt 1\n"))	  		
	}
  }
  WRITELOG(st,snprintf(st,4096,"p_i = %d %d\n", p_i,
		       (int) prompt_flags[p_i][0])) ;	
  return got_prompt ;
}


char *prompt_string[] = {
"> ",
"; ",
"+ ",
"$ ",
":",
"# ",
"(B",
"Type  <Return>\t to start : ",
"Save workspace image? [y/n/c]: ",
"Hit <Return> to see next plot: ",
"Password: ",
0
} ;

struct my_buffer {
  char *buf ;
  int size, get, put ;
} ;


int compare_end_B( struct my_buffer *b, char *string )
{
  int n,ret;
  
  n = strlen( string ) ;
  if( n > b->put - b->get )
    return (1==0) ;
  
  ret = strncmp( b->buf+b->put-n, string,n ) ;
  return( ret==0 ) ;
}

int check_prompt_strings_B( struct my_buffer *B )
{
  int p_i ;
  int got_prompt = (1==0) ;
  for( p_i=0; prompt_string[p_i]!=0; p_i++)
    if( compare_end_B( B, prompt_string[p_i] ) ) {
      WRITELOG(st,snprintf(st,4096,"Found string:%s\n",prompt_string[p_i]) ) ;
      got_prompt=(1==1) ;
      break ;
    }
  return got_prompt ;
}  

int tab_comp_ptr=0;

char big[1000] ;

pid_t childpid ;

int not_done=(1==1) ;

int master ;

void child_died(int x) ;
void something_wrong(int x) ;

void signal_int(int x)
{
  char st[4096];
  WRITELOG(st,snprintf(st,4096,"got signal %d\n",childpid) ) ;
  write(master,"",1) ;
//  kill(childpid,SIGINT) ;
  signal (SIGINT, signal_int);
  signal (SIGCHLD, child_died);
  signal (SIGBUS, something_wrong ) ;
  signal (SIGABRT, something_wrong ) ;
  signal (SIGILL, something_wrong ) ;
  signal (SIGSEGV, something_wrong ) ;

}

void child_died(int x)
{
  not_done=(1!=1) ;
  if( tab_comp_ptr != 0 ) {
	WRITELOG(st,snprintf(st,4096,"in child died, sent TeXmacs tuple\n") ) ;
	printf("%c(tuple \"read.ta\" \"michael\" \"there\")%c\n", DATA_BEGIN,DATA_END) ;
  }
  printf("%cverbatim:child_died, I quit.\n%c\n", DATA_BEGIN,DATA_END) ;
  WRITELOG(st,snprintf(st,4096,"child died\n") ) ;
  exit(0) ;
}

void something_wrong(int x)
{
  not_done=(1!=1) ;
  WRITELOG(st,snprintf(st,4096,"something_wrong\n") ) ;
  if( tab_comp_ptr != 0 ) {
	printf("%c(tuple \"\" \"\" \"\")%c\n", DATA_BEGIN,DATA_END) ;
  }
  printf("%cverbatim:something_wrong, I quit.\n%c\n", DATA_BEGIN,DATA_END) ;
  WRITELOG(st,snprintf(st,4096,"child died\n") ) ;
  exit(0) ;
}


void copy_to_B( struct my_buffer *b, char *data, size_t count )
{
  while( b->put + count > b->size ) {
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
  }
  memcpy(b->buf+b->put,data,count) ;
  b->put += count ;
}

void copy_B_to_B( struct my_buffer *b, struct my_buffer *c )
{
  ssize_t count = c->put - c->get ;
  while( b->put + count > b->size ) {
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
  }
  memcpy(b->buf+b->put, c->buf + c->get,count) ;
  b->put += count ;
  c->put = c->get = 0 ;
}

// copy from c to b
void ncopy_B_to_B( struct my_buffer *b, struct my_buffer *c, ssize_t count )
{
  while( b->put + count > b->size ) {
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
  }
  memcpy(b->buf+b->put, c->buf + c->get,count) ;
  b->put += count ;
  c->get += count ;
  if( c->put == c->get ) 
    c->get = c->put = 0 ; /* reset buffer */
}

void delete_from_B( struct my_buffer *b, int i, int count ) 
{
  if( i+count > b->put )  {
	count = b->put - i ;
  } 
  if( i >= b->put ) 
	return ;
	
  memmove(b->buf+i, b->buf+i+count, b->put-(i+count) ) ;
  b->put -= count ;
}


void rem_nl_B( struct my_buffer *b)
{
  int i = 0,j=0 ;
  for( i=b->get,j=b->get; i<b->put-1; i++) {
    if( b->buf[i] == 13 ) {
      if( b->buf[i+1] != 10 ) {
		b->buf[j] = 10 ;
		j++ ;
      }
    } else {
      b->buf[j] = b->buf[i] ;
      j++ ;
    }
  }
  if( b->buf[i] == 13 )
    b->buf[j] = 10 ;
  else
    b->buf[j] = b->buf[i] ;
  b->put = j+1 ;
}



int del_last_nl_B( struct my_buffer *b)
{
  int count = 0 ;
  char *p = b->buf + b->put - 1 ;
  while( (b->put != b->get) && ((*p == 13) || (*p == 10)) ) {
    p-- ;
    b->put-- ;
    count ++ ;
  }
  return count ;
}

int del_first_nl_B( struct my_buffer *b)
{
  int count = 0 ;
  char *p = b->buf + b->get ;
  while( (b->put != b->get) && ((*p == 13) || (*p == 10)) ) {
    p++ ;
    b->get++ ;
    count ++ ;
  }
  return count ;
}


int copy_to_B_del_nl( struct my_buffer *b, char *data, size_t count )
{
  int n = 0 ;
  while( b->put + count > b->size ) {
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
  }
  while( ((data[count-1] == 13) || (data[count-1]==10))&&(count>0)  ) {
    n++ ;
    count-- ;
  }
  memcpy(b->buf+b->put,data,count) ;
  b->put += count ;
  return n ;
}


ssize_t read_B( int fd, struct my_buffer *b, ssize_t count )
{
  while( b->put + count > b->size ) {
	WRITELOG(st,snprintf(st,4096, "reallocating: old = %p size=%d\n",b->buf,b->size)) ;
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
	if( b->buf == NULL ) {
	  WRITELOG(st, snprintf(st,4096, "allocation failed!!\n") ) ;
	}
	WRITELOG(st,snprintf(st,4096, "reallocating: new = %p size=%d\n",b->buf,b->size)) ;
  }
  count = read( fd, b->buf+b->put, count ) ;
  if( count > 0 )
    b->put += count ;
  return count ;
}


int data_available_B( struct my_buffer *b)
{
  return (b->put - b->get) ;
}

int write_B( int fd, struct my_buffer *b )
{
  int nwrite =0;
  
  if( b->put > b->get ) {
    /* we have data available */
    WRITELOG(big,snprintf(big,1000,"wrote to %d:\n",fd) ) ;
    WRITELOG(b->buf+b->get, b->put - b->get ) ;
    WRITELOG(big,snprintf(big,1000,"<<\n") ) ;
	
    nwrite = write( fd, b->buf+b->get, b->put - b->get ) ;
    b->get += nwrite ;
    if( b->put == b->get ) 
      b->get = b->put = 0 ; /* reset buffer */
  }
  return nwrite ;
}



struct my_buffer *init_buffer( int size )
{
  struct my_buffer *b = (struct my_buffer *)malloc( sizeof( struct my_buffer ) ) ;
  b->buf = (char *)malloc( size ) ;
  b->size = size ;
  b->get = b->put = 0 ;
  return b ;
}


int main(int arc, char *argv[])
{
  
  
  char  name[100];
  
  int in_data = 0 ;
  
  struct my_buffer 
  *TEMPB ,
  *TXB,   /* buffer for interface with texmacs */
  *RB ;   /* buffer for interface with R */
  
  ssize_t nread;
  
  
  int i,j ;
  int got_prompt ;
  int in_quote, escaped ;
  int ignore=0 ;
  
  int last_nl = 0 ;
  
  
  char *TEXMACS_PATH, *TEXMACS_R, *TEXMACS_SEND,*TEXMACS_SEND_E, *TEXMACS_LIB, *HOME ;
  struct termios termi ;
  sigset_t sigmask, orig_sigmask;
  
  struct stat stat_buf;

  
#ifdef USE_DEBUG
  if( DEBUG ) {
    unlink("/tmp/log") ;
    LOG = open("/tmp/log",O_CREAT|O_WRONLY) ;
  }
#endif

  
  TXB = init_buffer( 4096 ) ;
  RB = init_buffer( 4096 ) ;
  TEMPB = init_buffer( 4096 ) ;

  HOME = getenv ("HOME") ;
  if( HOME == NULL ) HOME = "~" ;
  /*fprintf (stderr, "HOME=%s\n", HOME) ;*/

  TEXMACS_PATH = getenv("TEXMACS_PATH") ;
  if( TEXMACS_PATH == NULL ) {
    TEXMACS_PATH = (char *)malloc( 4096 ) ;
    snprintf(TEXMACS_PATH,4096,"%s/.TeXmacs",HOME) ;
  }
  /*fprintf (stderr, "TEXMACS_PATH=%s\n", TEXMACS_PATH) ;*/
  
  /* Lazy installing the TeXmacs package */
  TEXMACS_LIB = (char *)malloc(4096);
  snprintf(TEXMACS_LIB,4096,"%s/plugins/r/r",TEXMACS_PATH);
  if (stat(TEXMACS_LIB,&stat_buf))
	system("r_install"); 
  /*fprintf (stderr, "TEXMACS_LIB=%s\n", TEXMACS_LIB) ;*/
  
  TEXMACS_R = getenv("TEXMACS_CMD") ;
  if( TEXMACS_R == NULL ) {
    TEXMACS_R = "R"; 
	/* ignore 1 input request - i.e. do not generate a prompt channel in
     texmacs for it */
    ignore=1;
  } else {
    ignore=0;
  }
  
  TEXMACS_SEND_E = getenv("TEXMACS_SEND") ;
  TEXMACS_SEND = (char *)malloc( 4096 ) ;
#if 1
  if( TEXMACS_SEND_E == NULL ) {
    /*snprintf(TEXMACS_SEND,4096,"library(TeXmacs,lib.loc=paste(Sys.getenv(\"HOME\"),\"/.TeXmacs/plugins/r/r/\",sep=\"\"))\n") ; */
    snprintf(TEXMACS_SEND,4096,"library(TeXmacs,lib.loc=\"%s\")\n",
	     TEXMACS_LIB) ;
  } else {
    snprintf(TEXMACS_SEND,4096,"%s\n",TEXMACS_SEND_E) ;
  }
#endif
  /*fprintf (stderr, "TEXMACS_SEND=%s\n", TEXMACS_SEND) ;*/
  
  /* prepare for the string to be sent to the process */
  copy_to_B( RB, st, snprintf( st,4096, "%s",TEXMACS_SEND) ) ;
  
  if( (childpid=forkpty( &master, name, NULL, NULL ))==0 ) {
    /* I'm the child - I'll run the command */
    char **exec_argv ;
    int i,n,m ;
    m = strlen( TEXMACS_R ) ;
    for( i=0,n=0; i<m; i++)
      if( TEXMACS_R[i] == ' ' ) 
		n++ ;
	
    exec_argv = (char **) malloc( (n+2)*sizeof( char * ) ) ;
	
    /* split TEXMACS_R into arguments into exec_argv, 
	 at each " " that doesn't have a \ to escape it  */
    exec_argv[0] = TEXMACS_R ;
    for( i=0,n=0; i<m; i++)
      if( (TEXMACS_R[i] == ' ') && (i>0) && (TEXMACS_R[i-1]!='\\') ) {
		n++ ;
		exec_argv[n] = TEXMACS_R+i+1 ;
		TEXMACS_R[i] = 0 ;
      }
    exec_argv[n+1] = NULL ;
    execvp(TEXMACS_R,exec_argv) ;
  } else {
    /* I'm the parent - I'll handle input and output and watch the child.*/
	
    /* This is for pselect. Supposedly if pselect doesn't know what signals
	 we are waiting for, it will get confused. */
    sigemptyset (&sigmask);
    sigaddset (&sigmask, SIGCHLD);
    sigaddset (&sigmask, SIGINT);
    sigaddset (&sigmask, SIGILL);
    sigaddset (&sigmask, SIGSEGV);
    sigaddset (&sigmask, SIGABRT);
    sigaddset (&sigmask, SIGBUS);
    sigprocmask (SIG_BLOCK, &sigmask,
				 &orig_sigmask);
	
    signal (SIGINT, signal_int);
    signal (SIGCHLD, child_died);
	signal (SIGBUS, something_wrong ) ;
	signal (SIGABRT, something_wrong ) ;
	signal (SIGILL, something_wrong ) ;
	signal (SIGSEGV, something_wrong ) ;
	
    fcntl(master, F_SETFL, O_NONBLOCK) ;
	
    /* send the initial string */
    write_B( master, RB ) ;
	
    /* get terminal settings */
    tcgetattr(master, &termi ) ;
#if VERBOSE_TERM
    printf("%x %x %x %x\n",termi.c_iflag,termi.c_oflag,termi.c_lflag,termi.c_lflag&ECHO) ;
#endif
    termi.c_lflag &= ~ECHO  ; /* no echo */
    tcsetattr(master,TCSANOW, &termi ) ;
	
#if VERBOSE_TERM
    printf("%x %x %x %x\n",termi.c_iflag,termi.c_oflag,termi.c_lflag,termi.c_lflag&ECHO) ;
#endif
    
	
    
    while(not_done) {
      /* prepare the file sets for select to watch */
      fd_set rd, wr, er;
      WRITELOG("<<<\n",4) ;
	  
      FD_ZERO (&rd);
      FD_ZERO (&wr);
      FD_ZERO (&er);
      FD_SET (master, &rd);
	  
      
      if( data_available_B( RB ) )  /* if we have data available to send,  */
	    FD_SET (master, &wr);	    /*  then also wait to send. */
	  
      FD_SET (STDIN_FILENO, &rd ) ;
	  
#define WAIT_FOR_OUT
#ifdef WAIT_FOR_OUT
      if( data_available_B( TXB ) )
	    FD_SET (STDOUT_FILENO, &wr ) ;
#endif
	  
      /* Main pselect switch --------------- */
      if( pselect( master+1, &rd, &wr, &er, NULL, &orig_sigmask ) > 0 ) {
	    if( FD_ISSET( STDIN_FILENO, &rd ) ) {
	      /* =============== read input from TeXmacs */
	      nread = read_B( STDIN_FILENO, RB, 1000 ) ;
	      if( (nread > 2 ) &&
			 (RB->buf[RB->put-nread]   == '@' ) &&
			 (RB->buf[RB->put+1-nread] == '@' ) &&
			 (RB->buf[RB->put+2-nread] == '@' )
			 ) {
			RB->put -= nread ;
		  } else {
			for(i=0; i<nread-1; i++) {
			  WRITELOG(st,snprintf(st,4096,":%d:%c", RB->buf[RB->put+i-nread], RB->buf[RB->put+i-nread]) ) ;
			  
			  /* What is this:? */
			  if( (RB->buf[RB->put+i-nread]==';') &&  (RB->buf[RB->put+i-nread+1]==';') )
			  {
				RB->buf[RB->put+i-nread] = ' ' ; /* This is space then delete */
				RB->buf[RB->put+i-nread+1] = 10 ;
			  } 
			  else 
				if( RB->buf[RB->put+i-nread]==DATA_COMMAND)
				{
				  if (!tab_comp_ptr)
				  {
					ignore=0 ;
					WRITELOG(st,snprintf(st,4096,"TeXmacs completion!!!\n") ) ;
					WRITELOG(st,snprintf(st,4096,"com: %s\n",&(RB->buf[RB->put+i-nread+1])) ) ;
					
					strncpy(RB->buf+RB->put-nread,"t.tab.comp",10);
					tab_comp_ptr=1;
					//i += 10 ; //
					i=10;
					RB->buf[RB->put+(i)-nread] = '(' ; //
					
					//	strncpy(RB->buf+RB->put-nread, temp_buf, tab_comp_ptr);
					in_quote = 0 ; escaped = 0 ;
					for(i++;i<nread-1;i++) {
					  // WRITELOG(st,snprintf(st,4096,"i:%d nread:%d in_q: %d, esc: %d, chr:%s\n",i,nread,in_quote,escaped,&(RB->buf[RB->put+(i)-nread]))) ;
					  if(in_quote == 0 ) {
						if( RB->buf[RB->put+(i)-nread]==' ') {  
						  WRITELOG(st,snprintf(st,4096,"found space\n")) ;
						  RB->buf[RB->put+(i)-nread] = ',' ;	
						  break ;
						}
					  } 
					  if ( (in_quote==0) && ( RB->buf[RB->put+(i)-nread]=='"') && ( RB->buf[RB->put+(i)-nread-1]=='"')) {
						WRITELOG(st,snprintf(st,4096,"found double quote\n")) ;
						printf("\2scheme:(tuple \"\" \"\")\5");
						fflush(stdout);
						i=nread;
						break;
					  }
					  
					  if( (escaped == 0) && (RB->buf[RB->put+(i)-nread]=='"') ) 
						in_quote = 1-in_quote ;
					  if( (escaped == 0 ) && (RB->buf[RB->put+(i)-nread]=='\\') ) {
						escaped = 1 ;
					  } else {
						escaped = 0 ;
					  }
					}
					
					break;
					
				  } else {
					
					RB->put=RB->get=0;
					break;
					
				  }
				}
			}
		  }
		}
		
		if( FD_ISSET( master, &rd ) ) {
		  /* =================== read input from sub process (R) */
		  while( read_B( master, TEMPB, 4096) > 0 ) 
			usleep(5) ;
		  
		  got_prompt = check_terminal( master ) || check_prompt_strings_B( TEMPB ) ;
		  
		  if (tab_comp_ptr) {
			WRITELOG(st,snprintf(st,4096, "in if(tab_comp_ptr)\n") ) ;
			if( TEMPB == NULL ) {
			  WRITELOG(st,snprintf(st,4096,"TEMPB is null\n")) ;
			} else {
			  WRITELOG(st, snprintf(st,4096,"get = %d, put=%d size=%d buf=%p\n",TEMPB->get,TEMPB->put,TEMPB->size,TEMPB->buf)) ;
//			  WRITELOG(st, snprintf(st,4096,"get = %d, put=%d size=%d buf=%p\n",TEMPB->get,TEMPB->put,TEMPB->size,TEMPB->buf)) ;
			  if( TEMPB->buf == NULL ) {
				WRITELOG(st,snprintf(st,4096,"TEMPB buf is null\n")) ;
			  } else {
				WRITELOG(st,snprintf(st,4096, "temp buf is %s\n",TEMPB->buf)) ;
			  }
			  
			}
			for (i=TEMPB->get,j=TEMPB->get;i<TEMPB->put;i++)
			{
			  WRITELOG(st,snprintf(st,4096,"i=%d %s\n",i,TEMPB->buf+i)) ;
			  if ( (TEMPB->buf[i]=='\2') && (j==TEMPB->get ) ) {
				WRITELOG(st,snprintf(st,4096,"found begin, moved j by %d\n",i-j)) ;
				j=i ;
			  }
			  if (TEMPB->buf[i]=='\5') {	
				WRITELOG(st,snprintf(st,4096,"found end, total length %d\n",i-j)) ;
				fwrite( TEMPB->buf+j, 1, i - j + 1, stdout) ;
				TEMPB->get=i+1;
				tab_comp_ptr=0;
				fflush(stdout);
				break;
			  }
			}
		  }
//		  if( got_prompt ) 
			tab_comp_ptr = 0 ;
		  
		  if( tab_comp_ptr ) 
			TEMPB->get = TEMPB->put ;
		  
		  //fflush(ffile);
		  //		  tab_comp_ptr=0;
		  
		  if( (got_prompt  ) && (ignore==0) )  { 
			/* find the previous end-of-line, and use that for the
			 prompt */
			for( i= TEMPB->put - TEMPB->get; i > 0; i--) 
			  if( (TEMPB->buf[i-1]==13) || (TEMPB->buf[i-1]==10) ) 
				break ;
			if( i > 0 ) {
#if 0
			  if( TEMPB->buf[TEMPB->get] == DATA_BEGIN ) {
				ncopy_B_to_B( TXB, TEMPB, i ) ;
			  } else 
#endif
			  {
				IN_VERBATIM( TXB ) ;
				/* print everything before the previous end-of-line */
				while( last_nl > 0 ) {
				  copy_to_B( TXB, "\n", 1 ) ;
				  last_nl-- ;
				} 
				/* print everything before the pervious end-of-line */
				WRITELOG(st,snprintf(st,4096,"copy 1\n") ) ;
				ncopy_B_to_B( TXB, TEMPB, i ) ;
				del_last_nl_B( TXB ) ;
			  }
			}
			del_first_nl_B( TEMPB ) ;
			IN_VERBATIM( TXB ) ;
			
			if( data_available_B( TEMPB) ) {
			  
			  last_nl = 0 ;
			  B_DATA_BEGIN( TXB) ; {
				copy_to_B( TXB, st, snprintf(st,4096,"prompt#") ) ;
				B_DATA_BEGIN( TXB ) ; 
				{
				  copy_to_B( TXB, st, snprintf(st,4096,"latex:\\red ") ) ;
				  WRITELOG(st,snprintf(st,4096,"copy 2\n") ) ;				  
				  copy_B_to_B( TXB, TEMPB ) ;
				  copy_to_B( TXB, st, snprintf(st,4096,"\\black") ) ;
				} 
				B_DATA_END( TXB ) ;
			  }
			  B_DATA_END( TXB ) ;
			} 
			END_VERBATIM( TXB ) ;
		  } else {
			if( got_prompt && (ignore > 0) ) 
			  ignore-- ;
			/* terminal is not waiting for user - just print data. */
#if 0
			if( TEMPB->buf[TEMPB->get] == DATA_BEGIN ) {
			  copy_B_to_B( TXB, TEMPB ) ;
			} else 
#endif
			{
			  IN_VERBATIM( TXB ) ;
			  while( last_nl > 0 ) {
				copy_to_B( TXB, "\n", 1 ) ;
				last_nl-- ;
			  }
//			  WRITELOG(st,snprintf(st,4096,"before last: %s\n",TEMPB->buf + TEMPB->get)) ;
			  copy_B_to_B( TXB, TEMPB ) ;
//			  WRITELOG(st,snprintf(st,4096,"after last: \n")) ;
			  last_nl = del_last_nl_B( TXB ) ;
			}
		  } 
		  
		}
#ifdef WAIT_FOR_OUT
		if( FD_ISSET( STDOUT_FILENO, &wr ) ) {
#endif
		  /* ================= TeXmacs is ready to receive data */
		  rem_nl_B( TXB ) ;
		  write_B( STDOUT_FILENO, TXB ) ;
#ifdef WAIT_FOR_OUT
		}
#endif
		if( FD_ISSET( master, &wr ) ) {
		  /* ================= Terminal is ready to receive data */
		  if( RB->put > RB->get ) {
			tcgetattr(master, &termi ) ;
			termi.c_lflag |= ECHO  ; /* no echo */
			termi.c_lflag ^= ECHO  ; /* no echo */
			/* set tserminal settings */
			tcsetattr(master,TCSANOW, &termi ) ;
			
			write_B( master, RB ) ;
		  }
		}
		
		
	  }
	}
	
  }
  exit(0) ;
}





#if 0
temp_buf=(char*)malloc(RB->size+10+6+1000);
strncpy(temp_buf,"t.tab.comp",10) ;
tab_comp_ptr=10;
temp_buf[tab_comp_ptr++] = '(';

temp_buf[tab_comp_ptr++] = '"';
strncpy(&temp_buf[tab_comp_ptr], &RB->buf[RB->put-nread], i);
tab_comp_ptr+=i;
temp_buf[tab_comp_ptr++] = '"';
temp_buf[tab_comp_ptr++] = ',';
tab_comp_ptr+=sprintf(&temp_buf[tab_comp_ptr],"%d",i);
temp_buf[tab_comp_ptr++] = ')';
temp_buf[tab_comp_ptr++] = '\n';
RB->put-=nread;
while ((RB->put+tab_comp_ptr) > RB->size)
{
  RB->size *= 2 ;
  RB->buf = (char *)realloc( RB->buf, RB->size ) ;
}
strncpy(RB->buf+RB->put, temp_buf, tab_comp_ptr);
RB->put+=tab_comp_ptr;
nread=tab_comp_ptr;

free(temp_buf);
#endif
