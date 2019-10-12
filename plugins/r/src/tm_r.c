/******************************************************************************
 * MODULE     : tm_r.c
 * DESCRIPTION: Glue between TeXmacs and R
 * COPYRIGHT  : (C) 2003 Michael Lachmann Tamarlin
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "config.h"
#define TEXMACS_R_VERSION "0.15"
#include <stdio.h>
#include <sys/select.h>
#ifdef __FreeBSD__
#include <libutil.h>
#else
#if HAVE_PTY_H
#include <pty.h>
#else
#include <util.h>
#endif

#include <utmp.h>
#endif
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <setjmp.h>

/* do we want to talk with texmacs, or with a human? */
#define INTERFACE_TEXMACS
#define USE_DEBUG

#ifdef INTERFACE_TEXMACS
#define DATA_BEGIN (2)
#define DATA_END (5)
#define DATA_COMMAND (16)
#define COMMAND_TABCOMPLETE ("\20(complete")
#else
#define DATA_BEGIN '{'
#define DATA_END '}'
#define DATA_COMMAND '@'
#define COMMAND_TABCOMPLETE ("@(complete")
#endif

#define TRUE (1==1)
#define FALSE (1==0)

#define NEW_LINE ('\n')
#define CARRIAGE_RETURN ('\r')

#define min(a,b) ((a)>(b)?(a):(b))

typedef tcflag_t term_prompt_entry[4] ;

term_prompt_entry prompt_flags_default[] = {
  { 00500,  000005, 0277,      0005021},
  { 00000,  000005, 0277,      0105061},
  { 00000,  000004, 0277,      0005061},
  { 02400,  014005, 0277,      0105001},
  { 00000,  000005, 0277, 037777763425},
//  { 020006,  02, 045400, 0101},
  {-1,-1,-1,-1}
} ;

char *prompt_string_default[] = {
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
  "(y/n) ",
  "[ghostwheel:~/work/texmacs/src] michael% ",
  0
} ;


char *prompt_string_buffer; int allocated_prompt_string_buffer ; int cur_prompt_string_buffer ;

jmp_buf error_return_env ;

#define ERROR_ALLOC (1)
#define MAX_PROMPT_LEN (1024)

int N_data_begins = 0 ;

char *DEFAULT_TEXMACS_SEND = "source(paste(Sys.getenv(\"TEXMACS_PATH\"),\"/plugins/r/texmacs.r\",sep=\"\"))\n";


// Add one more DATA_BEGIN, i.e. open bracket.
#define B_DATA_BEGIN( TXB )					\
  {								\
    printf_B( TXB, "%c",DATA_BEGIN );				\
    N_data_begins++;						\
  }

// Add one more DATA_END, i.e. close bracket.
#define B_DATA_END( TXB )					\
  {								\
    printf_B( TXB, "%c",DATA_END );				\
    N_data_begins--;						\
  }


// IN_VERBATIM sends '{verbatim:' to TeXmacs if we are not already in data mode.
#define IN_VERBATIM( TXB )						\
  {									\
    if( N_data_begins == 0 ) {						\
      B_DATA_BEGIN( TXB ) ;						\
      printf_B( TXB, "verbatim:" );					\
    }									\
  }

// END_VERBATIM closes all open brackets (counted by N_data_begins)
#define END_VERBATIM( TXB )					\
  {								\
    while( N_data_begins>0 ) {					\
      B_DATA_END( TXB ) ;					\
    }								\
  }





//////////////////////////


#define TEMP_BUF_SIZE (4096)

char temp_buf[ TEMP_BUF_SIZE ] ;
#define PRINTF_TEMP( ... ) snprintf( temp_buf, TEMP_BUF_SIZE, __VA_ARGS__ )

#ifndef USE_DEBUG

#define DEBUG_LOG( ... )
int DEBUG = FALSE ;
#undef VERBOSE_TERM

#else // def USE_DEBUG

#define DEBUG_LOG( ... ) {if( DEBUG ) write(LOG, temp_buf, snprintf( temp_buf, 4096, __VA_ARGS__ ) );}
int LOG ;
int DEBUG = TRUE ;

#endif // else def USE_DEBUG
/////////////////////////

char **prompt_string; int n_prompt_string=0, allocated_prompt_strings =0 ;

int my_strnlen( char *s, int n ) {
  int i;
  for (i=0; i<n; i++)
    if (s[i] == '\0') break;
  return i;
}

void add_prompt( char *new_prompt ) {
  int n ;
  DEBUG_LOG("add_prompt\n") ;
  // Make sure prompt_string has room for new prompt
  if( n_prompt_string == allocated_prompt_strings ) {
    DEBUG_LOG("eq: %d %d\n",n_prompt_string, allocated_prompt_strings) ;
    if( allocated_prompt_strings == 0 ) {
      prompt_string = calloc( 100 , sizeof( char * ) ) ;
      if( prompt_string == NULL ) longjmp( error_return_env, ERROR_ALLOC ) ;
      allocated_prompt_strings = 100 ;
    } else {
      prompt_string = realloc( prompt_string, 2 * allocated_prompt_strings * (sizeof( char * ) ) ) ;
      if( prompt_string == NULL ) longjmp( error_return_env, ERROR_ALLOC ) ;
      allocated_prompt_strings *= 2 ;
    }
  }
  
  DEBUG_LOG("add_prompt all alloc\n") ;

  // allocate room for string
  n = my_strnlen( new_prompt, MAX_PROMPT_LEN ) ;
  DEBUG_LOG("strnlen: %d\n\n",n) ;

  prompt_string[ n_prompt_string ] = calloc( n, sizeof( char ) ) ;
  if( prompt_string[ n_prompt_string ] == NULL ) longjmp( error_return_env, ERROR_ALLOC ) ;

  // copy it
  strncpy(  prompt_string[ n_prompt_string ], new_prompt , n ) ;
  n_prompt_string++ ;
}

term_prompt_entry *prompt_flag; int n_prompt_flag=0, allocated_prompt_flags =0 ;

void add_prompt_flag( term_prompt_entry *new_flag ) {
  int n ;

  // Make sure prompt_string has room for new prompt
  if( n_prompt_flag == allocated_prompt_flags ) {
    if( allocated_prompt_flags == 0 ) {
      prompt_flag = calloc( 100 , sizeof( term_prompt_entry ) ) ;
      if( prompt_flag == NULL ) longjmp( error_return_env, ERROR_ALLOC ) ;
      allocated_prompt_flags = 100 ;
    } else {
      prompt_flag = realloc( prompt_flag, 
			     2 * allocated_prompt_flags * (sizeof( term_prompt_entry ) ) ) ;
      if( prompt_flag == NULL ) longjmp( error_return_env, ERROR_ALLOC ) ;
      allocated_prompt_flags *= 2 ;
    }
  }
  
  // copy it
  memcpy( prompt_flag + n_prompt_flag, new_flag, sizeof( term_prompt_entry ) ) ;
  //  prompt_flag[ n_prompt_flag ] = new_flag[0] ;
  n_prompt_flag++ ;
}


#if 0
// Reads a single integer from buffer CONF.
// If an empty line is found, FALSE is returned.
// anything from '#' till end of line is ignored
// Any other character is ignored
// integer can be in hex (0x) oct (0..) or dec
int conf_read_int_B( struct my_buffer *CONF, int *from, int *val)
{
  int data_since_nl = TRUE ;
  char tmp[20] ;

  for( ; *from >= 0; from-- ) {
    if( CONF->buf[CONF->put - from] == NEW_LINE ) {
      if( !data_since_nl ) return FALSE ;
      else( data_since_nl ) FALSE ;
    } else if( (CONF->buf[CONF->put - *from] != CARRIAGE_RETURN ) &&
	       (CONF->buf[CONF->put - *from] != ' ' ) && 
	       (CONF->buf[CONF->put - *from] != '\t' ) ) {
      data_since_nl = TRUE ;

      if( CONF->buf[CONF->put - *from] == '#' ) {
	d = B_find( CONF, *from, NEW_LINE ) ;
	if( d > 0 ) *from -= d ;
	else {
	  *from = 0 ;
	  return FALSE ;
	}
      }
      
      if( B_strcmp_put( CONF, *from, "0x" ) ) {
	sprintf( tmp, "%%x%d%%n", *from ) ;
	sscanf( tmp, val, &d ) ;
	*from -= d ;
	return TRUE ;
      }

      if( B_strcmp_put( CONF, *from, "0" ) ) {
	sprintf( tmp, "%%o%d%%n", *from ) ;
	sscanf( tmp, val, &d ) ;
	*from -= d ;
	return TRUE ;
      }

      if( B->buf[ B->put - *from ] > '0' && B->buf[ B->put - *from ] <= '9' ) {
	sprintf( tmp, "%%d%d%%n", *from ) ;
	sscanf( tmp, val, &d ) ;
	*from -= d ;
	return TRUE ;
      }
    }
	
  }
}

    

void read_config_numbers( char *fname, struct my_buffer *CONF, char *par, int **numbers, int *n_numbers )
{
  
  if( CONF==NULL )
    slurp_file_B( fname, CONF ) ;

  if( *numbers == NULL ) {
    *numbers = (int *) malloc( 256 * sizeof( int ) ) ;
    *n_numbers = 256 ;
  }

  i = B_find( CONF, CONF->put - CONF->get , par ) ;

  i = B_find_in( CONF, CONF->put - CONF->get - i, "0123456789" ) ;

  cur_read = 0 ;

  while( !done ) {
    if( cur_read > n_numbers - 10 ) {
      n_numbers *= 2 ;
      *numbers = (int *) realloc( *numbers, n_numbers * sizeof(int) ) ;
    }

    done = !conf_read_int_B( CONF, *numbers + cur_read ) ;
  
  }
}
#endif




/* check if we got the prompt from R */
// Prompt flags contains a list of flags that I saw when the terminal
// Was waiting for input.
// check terminal checks the current flags against all those flags.
int check_terminal( int f )
{
  int p_i ;
  int got_prompt = FALSE ;
  struct termios termi ;

  tcgetattr(f, &termi ) ; // get the flags from current terminal

  DEBUG_LOG( "term(hex):i:%x o:%x c:%x l:%x\n",
			(unsigned int) termi.c_iflag,
			(unsigned int) termi.c_oflag,
			(unsigned int) termi.c_cflag,
			(unsigned int) termi.c_lflag  ) ;
  DEBUG_LOG( "term(oct):i:%o o:%o c:%o l:%o\n",
			(unsigned int) termi.c_iflag,
			(unsigned int) termi.c_oflag,
			(unsigned int) termi.c_cflag,
			(unsigned int) termi.c_lflag  ) ;
  
  //  termi.c_lflag |= ECHO ;
  // tcsetattr( f, TCSASOFT, &termi ) ;
  
  
  // compare the terminal flags against those in prompt_flags.
  for( p_i = 0; p_i < n_prompt_flag; p_i++) {  

    if( (termi.c_iflag == prompt_flag[p_i][0]) &&
	(termi.c_oflag == prompt_flag[p_i][1]) &&
	(termi.c_cflag == prompt_flag[p_i][2]) &&
	(termi.c_lflag == prompt_flag[p_i][3]) ) {
		DEBUG_LOG( "p_i=%d \n",p_i) ;
      return TRUE ;
    } 
  }
  return FALSE ;
}





int tab_comp_ptr=0;

pid_t childpid ;

int not_done=(1==1) ;

int subprocess ;

void child_died(int x) ;
void something_wrong(int x) ;

char last_prompt_candidate[1025] ;


void signal_int(int x)
{
  char st[4096];
  DEBUG_LOG("got signal\n") ;
  write(subprocess,"",1) ;
  // write(subprocess,"\t\t",2) ;
  // kill(childpid,SIGINT) ;
  // N_data_begins = 0 ;

  signal (SIGINT, signal_int);
  signal (SIGCHLD, child_died);
  signal (SIGBUS, something_wrong ) ;
  signal (SIGABRT, something_wrong ) ;
  signal (SIGILL, something_wrong ) ;
  signal (SIGSEGV, something_wrong ) ;
  // add_prompt( last_prompt_candidate ) ;
}

void child_died(int x)
{
  not_done=(1!=1) ;
  if( tab_comp_ptr != 0 ) {
    printf("%c(tuple \"read.ta\" \"michael\" \"there\")%c\n", DATA_BEGIN,DATA_END) ;
  }
  printf("%cverbatim:child_died, I quit.\n%c\n", DATA_BEGIN,DATA_END) ;
  exit(0) ;
}

void something_wrong(int x)
{
  not_done=(1!=1) ;
  if( tab_comp_ptr != 0 ) {
    printf("%c(tuple \"\" \"\" \"\")%c\n", DATA_BEGIN,DATA_END) ;
  }
  printf("%cverbatim:something_wrong, I quit.\n%c\n", DATA_BEGIN,DATA_END) ;
  DEBUG_LOG( "child died\n" ) ;
  exit(0) ;
}

///////////////////////////////////////////
// my_buffer

struct my_buffer {
  char *buf ;
  int size, get, put ;
} ;

// Below are routines that handle my_buffer
int compare_end_B( struct my_buffer *b, char *string )
{
  int n,ret;
  
  n = strlen( string ) ;
  if( n > b->put - b->get )
    return (1==0) ;
  
  ret = strncmp( b->buf+b->put-n, string,n ) ;
  return( ret==0 ) ;
}


void store_last_prompt_candidate( struct my_buffer *B)
{
  int i,n ;
  for( i=B->put-1; i>= B->get; i-- ) if( B->buf[i] == '\n' ) break ;
  i++ ;
  n = B->put - i ;
  if( n > 1024 ) n=1024 ;
  if( n > 0 ) {
    DEBUG_LOG("found at %d: ||%s||",i,B->buf+i) ;
    strncpy( last_prompt_candidate, B->buf+i, n ) ;
  }
  last_prompt_candidate[n]=0 ;
}

int check_prompt_strings_B( struct my_buffer *B )
{
  int p_i ;
  int got_prompt = FALSE ;
  for( p_i=0; p_i < n_prompt_string; p_i++)
    if( compare_end_B( B, prompt_string[p_i] ) ) {
      DEBUG_LOG( "found prompt_string:%s\n", prompt_string[p_i])
      got_prompt= TRUE ;
      break ;
    }
  if( !got_prompt ) store_last_prompt_candidate( B ) ;
  return got_prompt ;
}  

void clear_B( struct my_buffer *B )
{
  B->get = B->put = 0 ;
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

#define printf_B( B, ... ) {copy_to_B( (B), temp_buf, snprintf( temp_buf, (TEMP_BUF_SIZE) , __VA_ARGS__) ) ;}

void sanitize_B( struct my_buffer *B ) {
  int i ;
  for( i= B->get; i< B->put; i++ ) {
    if( B->buf[i] == '%' ) B->buf[i]=' ' ;
  }

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
  // convert nl (CARRIAGE_RETURN) to cr (NEW_LINE)
  // and nl,cr to cr i.e. s/[nl][cr]{0,1}/cr/
  int i, j ;
  for( i= b->get, j= b->get;  i < (b->put - 1); i++ ) {
    if( b->buf[i] == CARRIAGE_RETURN ) {
      if( b->buf[i+1] != NEW_LINE ) {
        b->buf[j] = NEW_LINE ; 
        j++ ;
      }
    } else {
      b->buf[j] = b->buf[i] ;
      j++ ;
    }
  }
  if( b->buf[i] == CARRIAGE_RETURN )
    b->buf[j] = NEW_LINE ; 
  else
    b->buf[j] = b->buf[i] ;
  b->put = j+1 ;
}




// returns the number of new_lines that were removed
int del_last_nl_B( struct my_buffer *b)
{
  int count = 0 ;
  char *p = b->buf + b->put - 1 ;
  while( (b->put != b->get) && ((*p == CARRIAGE_RETURN) || (*p == NEW_LINE)) ) {
    p-- ;
    b->put-- ;
    if( *p == NEW_LINE ) count ++ ;
  }
  return count ;
}

// returns the number of new_lines that were removed
int del_first_nl_B( struct my_buffer *b)
{
  int count = 0 ;
  char *p = b->buf + b->get ;
  while( (b->put != b->get) && ((*p == CARRIAGE_RETURN) || (*p == NEW_LINE)) ) {
    p++ ;
    b->get++ ;
    if( *p == NEW_LINE ) count ++ ;
  }
  return count ;
}

ssize_t read_B( int fd, struct my_buffer *b, ssize_t count )
{
  while( b->put + count > b->size ) {
    b->size *= 2 ;
    b->buf = (char *)realloc( b->buf, b->size ) ;
    if( b->buf == NULL ) {
    }
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
    //	DEBUG_LOG("changed Termflag: %x",termi)
	
    nwrite = write( fd, b->buf+b->get, b->put - b->get ) ;
    b->get += nwrite ;
    if( b->put == b->get ) 
      b->get = b->put = 0 ; /* reset buffer */
  }
  return nwrite ;
}

int write_B2( int fd, struct my_buffer *b )
{
  struct termios termi ;	
  int nwrite =0;
  
  if( b->put > b->get ) {
    /* we have data available */
    tcgetattr( fd, &termi ) ;

//	DEBUG_LOG("termflag: %x",termi)
    termi.c_lflag &= ~ECHO  ; /* no echo */
//	DEBUG_LOG("termflag: %x",termi)
    tcsetattr( fd,TCSADRAIN, &termi ) ;
	tcgetattr( fd, &termi ) ;
//	DEBUG_LOG("changed Termflag: %x",termi)
	
    nwrite = write( fd, b->buf+b->get, b->put - b->get ) ;
    b->get += nwrite ;
    if( b->put == b->get ) 
      b->get = b->put = 0 ; /* reset buffer */
    tcgetattr( fd, &termi ) ;

//	DEBUG_LOG("termflag: %x",termi)
    termi.c_lflag |= ECHO  ; /* echo */
    termi.c_lflag &= ~ECHOE  ; /* echo */
//	DEBUG_LOG("termflag: %x",termi)
//  tcsetattr( fd,TCSADRAIN, &termi ) ;
    tcgetattr( fd, &termi ) ;
//	write( fd, "\x13", 1 ) ;
  }
  return nwrite ;
}


int debug_B( struct my_buffer *b )
{
  int nwrite =0;
  
  if( b->put > b->get ) {
    /* we have data available */
	
    nwrite = write( LOG, b->buf+b->get, b->put - b->get ) ;
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

// end of my_buffer
/////////////////////


int B_strncmp_put( struct my_buffer *B, int from, char *str, size_t n)
{
  return strncmp( B->buf + B->put - from, str, n )==0 ;
}

int B_strcmp_put( struct my_buffer *B, int from, char *str )
{
  int n = strlen( str ) ;
  if( from < n )
    return FALSE ;
  else return strncmp( B->buf + B->put - from, str, n )==0 ;
}



int B_replace_inplace( struct my_buffer *B, int len, char *find, char *rep ) {
  int find_n = strlen( find ) ;
  int rep_n = strlen( rep ) ;
  int i,j ;
  
  char *str = B->buf + B->put - len ;
  int d = find_n - rep_n ;
  int found = 0 ;

  for( i=0,j=0; i < len- find_n; ) {
    if( strncmp( str+i, find, find_n )==0 ) {
      strncpy( str+j, rep, rep_n ) ;
      i += find_n ;
      j += rep_n ;
      found ++ ;
    } else {
      str[j] = str[i] ;
      i++ ;
      j++ ;
    }
  }
  DEBUG_LOG( "i %d j %d\n",i,j) ;
  for( ; i < len; ) {
      str[j] = str[i] ;
      i++ ;
      j++ ;
  }
  DEBUG_LOG( "i %d j %d\n",i,j) ;
  B->put = B->put - len + j ;
  return found ;
}

int B_find( struct  my_buffer *B, int from, char *find ) {
  int i ;
  int find_n = strlen( find ) ;

  
  DEBUG_LOG("in find from=%d, find_n=%d\n", from, find_n ) ;
  for( i=0; i<from - find_n; i++ ) {
    DEBUG_LOG( "comparing ||%10s|| to ||%10s||\n", B->buf + B->put - from + i, find ) ;
    if( strncmp( B->buf + B->put - from + i, find, find_n )==0 ) {
      DEBUG_LOG("same\n")
      return i ;
    }
  }
  
  return -1 ;
}


// This will reformat the input from TeXmacs form. @(complete to R form t.tab.comp(
void prepare_tabcomplete_request( struct my_buffer *TO_R_B, int from ) 
{
  int i ;
  int in_quote, escaped ;
  DEBUG_LOG("prepareing tab request\n") ;
  i = strlen( COMMAND_TABCOMPLETE )+1 ;
  strncpy( TO_R_B->buf + TO_R_B->put- from, "t.tab.comp(", i ); // This hack assumes that the command is '@(complete'
  // And thus contains the same number of characters as "t.tab.comp"

  in_quote = FALSE ; escaped = FALSE ;
  for( ; i < from-1; i++) { // find the first ' ' not in quotes, and replace it with ",".
    DEBUG_LOG("i=%d [%c]\n",i,TO_R_B->buf[ TO_R_B->put+(i)- from]) ;
    if( in_quote ) DEBUG_LOG("quote\n") ;
    if( ! in_quote ) {
      if( TO_R_B->buf[ TO_R_B->put+(i)- from]==' ') {  
        DEBUG_LOG("found space\n"); 
        TO_R_B->buf[ TO_R_B->put+(i)- from] = ',' ;	
        break ;
      } 
      if ( B_strcmp_put( TO_R_B, from-i, "\"\"" ) ) {
        printf("\2scheme:(tuple \"\" \"\")\5"); ///// TODO: this shouldn't be here! We shouldn't just print to TeXmacs.
        fflush(stdout);
        break;
      }
    } 
				  
    if( ( ! escaped ) && (TO_R_B->buf[ TO_R_B->put+(i)-from]=='"') ) 
      in_quote = ! in_quote ;
    if( ( ! escaped ) && (TO_R_B->buf[ TO_R_B->put+(i)-from]=='\\') ) {
      escaped = TRUE ;
    } else {
      escaped = FALSE ;
    }
  }
}



void handle_command( struct my_buffer *B, int nread ) {
  char *s = B->buf + B->put - nread ;
  DEBUG_LOG("in handle command 6s=||%1.6s|| %d %ld\n",s,nread,strlen("prompt")) ;
  if( *s == '(' ) {s++ ; nread-- ;}

  DEBUG_LOG("strcmp: %d\n",  strncmp( s, "prompt", nread ) ) ;
  
  if( nread >= strlen("termstate") && strncmp(s, "termstate", nread )== 0 ) {

  } else if( nread >= strlen("prompt") && strncmp( s, "prompt", 6 )==0 ) {
    DEBUG_LOG("got promppt command\n") ;
    
    add_prompt(last_prompt_candidate) ;
  } else if( nread >= strlen("clear_prompt") && strncmp( s, "clear_prompt", nread )==0 ) {

  } else if( nread >= strlen("session_name") && strncmp( s, "session_name", nread )==0 ) {

  } 
}


int main(int argc, char *argv[])
{
  char *name=NULL ;
  
  
  struct my_buffer 
    *FROM_R_B ,
    *TXB,   /* buffer for interface with texmacs */
    *TO_R_B ;   /* buffer for interface with R */
  
  ssize_t nread;
  
  
  int i,j ;
  int got_prompt,  error ;
  int in_quote, escaped ;
  int n_ignore_prompts=0 ;

  int last_nl = 0, command_place ;
  
  
  char *TEXMACS_HOME_PATH, *TEXMACS_R, *TEXMACS_SEND_E, *TEXMACS_LIB, *HOME ;
  struct termios termi ;
  sigset_t sigmask, orig_sigmask;
  
  struct stat stat_buf;

  name = getenv("TEXMACS_R_SESSION") ;
  if( argc > 1 ) name = argv[1] ;
  
#ifdef USE_DEBUG
  if( DEBUG ) {
    unlink("/tmp/log") ;
    LOG = open("/tmp/log",O_CREAT | O_RDWR,0755) ;
  }
#endif
  
  
  HOME = getenv("HOME") ;
  if( HOME == NULL ) HOME = "~" ;

  TEXMACS_HOME_PATH = getenv("TEXMACS_HOME_PATH") ;
  if( TEXMACS_HOME_PATH == NULL ) {
    TEXMACS_HOME_PATH = (char *)malloc( 4096 ) ;
    snprintf( TEXMACS_HOME_PATH, 4096, "%s/.TeXmacs",HOME) ;
  }
  
  /* Lazy installing the TeXmacs package */
  TEXMACS_LIB = (char *)malloc(4096);
  snprintf(TEXMACS_LIB,4096,"%s/plugins/r/r",TEXMACS_HOME_PATH);
  if (stat(TEXMACS_LIB,&stat_buf))
    system("r_install"); 

  setenv( "TERM", "dumb", 1) ;
  
  
  // Build the command tp execute
  TEXMACS_R = getenv("TEXMACS_CMD") ;
  if( TEXMACS_R == NULL ) {
    TEXMACS_R = "R"; 
    /* ignore 1 input request - i.e. do not generate a prompt channel in
       texmacs for it */
    n_ignore_prompts=0;
  } else {
    n_ignore_prompts=0;
  }
  
  // Send commands to the process we just started. This is usually to load the TeXmacs library.
  TEXMACS_SEND_E = getenv("TEXMACS_SEND") ;
  if( TEXMACS_SEND_E == NULL ) TEXMACS_SEND_E = DEFAULT_TEXMACS_SEND ;
  DEBUG_LOG( "TEXMACS_SEND=%s",TEXMACS_SEND_E) ;

  

  if( (childpid=forkpty( &subprocess, NULL, NULL, NULL ))==0 ) {
    /* I'm the child - I'll run the command */
    char **exec_argv ;
    int i,n,m;
    m = strlen( TEXMACS_R ) ;
    for( i=0,n=0; i<m; i++)
      if( TEXMACS_R[i] == ' ' ) 
        n++ ;
	
    unsetenv( "DYLD_LIBRARY_PATH") ;
	
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
	
    /* This is for pselect. Supposedly if pselect doesn't kn ow what signals
       we are waiting for, it will get confused. */

    TXB = init_buffer( 4096 ) ;
    TO_R_B = init_buffer( 4096 ) ;
    FROM_R_B = init_buffer( 4096 ) ;


    // Add default prompt strings to search array
    if( (error=setjmp( error_return_env ))== 0 ) {
      for( i=0; prompt_string_default[i] != 0; i++ )
        add_prompt( prompt_string_default[i]  ) ;
    } else {
      IN_VERBATIM( TXB ) ;
      copy_to_B( TXB, temp_buf, snprintf( temp_buf, TEMP_BUF_SIZE, "error: out of memory error in tm_r\n" ) );
      END_VERBATIM( TXB ) ;
    }

    // Add default term flags for prompt to array
    if( (error=setjmp( error_return_env ))== 0 ) {
      for( i=0; prompt_flags_default[i][0] != -1; i++ )
        add_prompt_flag( &(prompt_flags_default[i])  ) ;
    } else {
      IN_VERBATIM( TXB ) ;
      copy_to_B( TXB, temp_buf, snprintf( temp_buf,  TEMP_BUF_SIZE, "error: out of memory error in tm_r\n" ) );
      END_VERBATIM( TXB ) ;
    }
	
		  
    // Send initial commands
    copy_to_B( TO_R_B, temp_buf, snprintf( temp_buf, TEMP_BUF_SIZE, "%s",TEXMACS_SEND_E ) ) ;  

    DEBUG_LOG("setting mask\n") ;
    sigemptyset (&sigmask);
    sigaddset (&sigmask, SIGINT);
    sigaddset (&sigmask, SIGCHLD);
    sigaddset (&sigmask, SIGBUS);
    sigaddset (&sigmask, SIGABRT);
    sigaddset (&sigmask, SIGILL);
    sigaddset (&sigmask, SIGSEGV);
    sigprocmask (SIG_BLOCK, &sigmask, &orig_sigmask);
	
    signal (SIGINT, signal_int);
    signal (SIGCHLD, child_died);
    signal (SIGBUS, something_wrong ) ;
    signal (SIGABRT, something_wrong ) ;
    signal (SIGILL, something_wrong ) ;
    signal (SIGSEGV, something_wrong ) ;
	
    fcntl(subprocess, F_SETFL, O_NONBLOCK) ;
	
    /* send the initial string */
    write_B( subprocess, TO_R_B ) ;
	
    /* get terminal settings */
    tcgetattr(subprocess, &termi ) ;

//    termi.c_lflag &= ~ECHO  ; /* no echo */
//	    termi.c_lflag &= ~CANON  ; /* no echo */
    tcsetattr(subprocess,TCSANOW, &termi ) ;
	
    
    while(not_done) { // main loop. Every iteration we wait for data or data send with pselect.

      /* prepare the file sets for pselect to watch */
      fd_set rd, wr, er;
 	  
      FD_ZERO (&rd);
      FD_ZERO (&wr);
      FD_ZERO (&er);

      FD_SET (subprocess, &rd); // wait for sub-process to send data (i.e. data from R).
	       
      FD_SET (STDIN_FILENO, &rd ) ; /* wait for data from TeXmacs */
	  
      if( data_available_B( TXB ) ) /* if we have data to send to TeXmacs */
        FD_SET (STDOUT_FILENO, &wr ) ; /* then also wait to send to TeXmacs */

      if( data_available_B( TO_R_B ) )  /* if we have data available to send to R,  */
        FD_SET (subprocess, &wr);	    /*  then also wait to send to R. */
	  
	  

      // We will wait till something happens using pselect.

      if( (pselect( subprocess+1, &rd, &wr, &er, NULL, &orig_sigmask )) > 0 ) {
        if( FD_ISSET( STDIN_FILENO, &er) ) exit(0) ;
        if( FD_ISSET( STDOUT_FILENO, &er) ) exit(0) ;
        if( FD_ISSET( subprocess, &er) ) exit(0) ;
      
        ////////////////////////////////////////////
        // input ready from TeXmacs
        if( FD_ISSET( STDIN_FILENO, &rd ) ) {
          /* =============== read input from TeXmacs */
          nread = read_B( STDIN_FILENO, TO_R_B, 1000 ) ;
          DEBUG_LOG("got from TeXmacs:||" ) ;
          debug_B( TO_R_B ) ;
          DEBUG_LOG("||\n" ) ;

          /* TeXmacs sent EOF, nread=0 */
          if( nread == 0 ) exit(0) ;

          if( B_strcmp_put( TO_R_B, nread, "@@@" ) ) {
            DEBUG_LOG("got @@@\n" ) ;
            handle_command( TO_R_B, nread-3 ) ;
            TO_R_B->put -= nread ;
            //	    copy_to_B( TO_R_B, temp_buf, snprintf( temp_buf, TEMP_BUF_SIZE, "prin\t\t" ) );
          } else {
            n_ignore_prompts += 0 ;
            B_replace_inplace( TO_R_B, nread, ";;", "\n" ) ;
            B_replace_inplace( TO_R_B, nread, "@@EOF", "Hi\n\04\n") ;
            DEBUG_LOG("replaced ';;':||" ) ;
            debug_B( TO_R_B ) ;
            DEBUG_LOG("||\n" ) ;

            if( (command_place = B_find( TO_R_B, nread, COMMAND_TABCOMPLETE )) > -1 ) {
              DEBUG_LOG("got tab complete\n" ) ;
              if( !tab_comp_ptr ) {
                prepare_tabcomplete_request( TO_R_B, nread - command_place ) ;
                tab_comp_ptr = TRUE ;
              } else
                clear_B( TO_R_B ) ;
            }
          }
          DEBUG_LOG("end of TeXmacs:||" ) ;
          debug_B( TO_R_B ) ;
          DEBUG_LOG("||\n" ) ;
        }
        // end handle input from TeXmacs
        /////////////////////////////////////////

        //////////////////////////////////////////////////////////////////////
        // start of read from R
        if( FD_ISSET( subprocess, &rd ) ) {
          /* =================== read input from sub process (R) */
          DEBUG_LOG("BBefore reading from R %d %d:||",FROM_R_B->put, FROM_R_B->get ) ;
          debug_B( FROM_R_B ) ;
          DEBUG_LOG("||\n" ) ;

          while( read_B( subprocess, FROM_R_B, 4096) > 0 ) // read as much as we can.
            usleep(100000) ;

          DEBUG_LOG("got from R:||" ) ;
          debug_B( FROM_R_B ) ;
          DEBUG_LOG("||\n" ) ;
          if( compare_end_B( FROM_R_B, "--More--" ) ) 
            copy_to_B( TO_R_B, " ", 1 ) ;
          
          // Nefore we do anything, check for prompt. Don't want to spoil it.
          got_prompt = check_terminal( subprocess ) || check_prompt_strings_B( FROM_R_B ) ;
        	  
          if( got_prompt ) DEBUG_LOG( "This is a prompt\n") ;
          
          ///////
          // handle return from tabcomplete
          if (tab_comp_ptr) { // check if we got from R the complete completion.

            for(  i=FROM_R_B->get, j=FROM_R_B->get;  i<FROM_R_B->put;  i++ ) {
              if ( (FROM_R_B->buf[i]== DATA_BEGIN) && ( j==FROM_R_B->get ) ) { // we got first data begin
                j=i ;
              } else if ( FROM_R_B->buf[i]== DATA_END ) {  // we got data end
                fwrite( FROM_R_B->buf+j, 1, i - j + 1, stdout) ; // TODO: here, we're also directly printing to TeXmacs...
                FROM_R_B->get=i+1;
                fflush(stdout);
                break;
              }
            }
            tab_comp_ptr = FALSE ;
          }  // end handle tabcomplete
	
          //////////////////
          // If it isn't tabcomplete, give the data to TeXmacs.
          //
          if(  !got_prompt ) { // didn't got_prompt
            /* terminal is not waiting for user - just print data. */
            IN_VERBATIM( TXB ) ; // Make sure we're in verbatim mode.
            while( last_nl > 0 ) {
              copy_to_B( TXB, "\n", 1 ) ; // copy the right number of new-lines (according to last_nl)
              last_nl-- ;
            }
            copy_B_to_B( TXB, FROM_R_B ) ;
            //	    last_nl = del_last_nl_B( TXB ) ;
            // no need for END_VERBATIM it will be closed when we get  a prompt and print it.
          } else { // Did get a prompt
            if( n_ignore_prompts > 0 ) {
              DEBUG_LOG("ignoring prompt\n") ;
              n_ignore_prompts-- ;
            }
            else { // We have a prompt, and we're not ignoring it

              /* The prompt is assumed to be the last line of the output.
                 So, we send every thin but the last line, and then nicely format the last line as a prompt.
              */

              // First, find previous end of line
              for( i= FROM_R_B->put - FROM_R_B->get; i > 0; i--) 
                if( (FROM_R_B->buf[i-1]==CARRIAGE_RETURN) || (FROM_R_B->buf[i-1]==NEW_LINE) ) 
                  break ;

              if( i > 0 ) { // Found end-of-line
                /* print everything before the previous end-of-line */
                IN_VERBATIM( TXB ) ; // make sure we're in verbatim mode.
                while( last_nl > 0 ) {  // put in buffer TXB last_nl NEW_LINEs
                  copy_to_B( TXB, "\n", 1 ) ;
                  last_nl-- ;
                } 
                ncopy_B_to_B( TXB, FROM_R_B, i ) ;		
                //		del_last_nl_B( TXB ) ;
              }

              del_first_nl_B( FROM_R_B ) ;
              /* Now print the prompt a bit nicely */
              IN_VERBATIM( TXB ) ;   // make sure we're in verbatim mode.
              if( data_available_B( FROM_R_B) ) {
                DEBUG_LOG("N_data_begins=%d\n",N_data_begins) ;
                last_nl = 0 ;
                B_DATA_BEGIN( TXB) ; {
                  printf_B( TXB, "prompt#" ) ;
                  B_DATA_BEGIN( TXB ) ; {
                    printf_B( TXB, "latex:\\red " ) ;
                    if( name != NULL ) printf_B(TXB, "[%s]",name) ;
                    sanitize_B( FROM_R_B ) ;
                    copy_B_to_B( TXB, FROM_R_B ) ;
                    printf_B( TXB, "\\black" ) ;
                  } 
                } 
              } 
              END_VERBATIM( TXB ) ; // close all parenthesis
	          } 
	        } 
	      }
        // end of read from R
        //////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////
        // TeXmacs is ready to receive input
        if( FD_ISSET( STDOUT_FILENO, &wr ) ) {
          /* ================= TeXmacs is ready to receive data */
          rem_nl_B( TXB ) ;
        
          DEBUG_LOG("sending to TeX:||" ) ;
          debug_B( TXB ) ;
          DEBUG_LOG("||\n" ) ;
          write_B( STDOUT_FILENO, TXB ) ;
        }
        
        //////////////////////////////////////
        // R is ready to receive data
        if( FD_ISSET( subprocess, &wr ) ) {
          /* ================= R is ready to receive data */
          if( TO_R_B->put > TO_R_B->get ) {
            // tcgetattr(subprocess, &termi ) ;
            // termi.c_lflag |= ECHO  ; /* no echo */
            // termi.c_lflag ^= ECHO  ; /* no echo */
        	  /* set tserminal settings */
            // tcsetattr(subprocess,TCSANOW, &termi ) ;
            /* get terminal settings */
            tcgetattr(subprocess, &termi ) ;
        
            termi.c_lflag &= ~ECHO  ; /* no echo */
            tcsetattr(subprocess,TCSADRAIN, &termi ) ;
        
            DEBUG_LOG("sending to R:||" ) ;
            debug_B( TO_R_B ) ;
            DEBUG_LOG("||\n" ) ;
            DEBUG_LOG("before: %d %d\n",TO_R_B->put, TO_R_B->get) ;
            write_B2( subprocess, TO_R_B ) ;
            DEBUG_LOG("after: %d %d\n",TO_R_B->put, TO_R_B->get) ;
          }
        }
      }
    }
  } // I'm the parent
  exit(0) ;
}
