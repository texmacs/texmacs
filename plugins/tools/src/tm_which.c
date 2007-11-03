#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#define DIR_SEPARATOR '/'
#define SEARCHPATH_SEPARATOR ':'
#define FALSE 0
#define TRUE 1

typedef enum
{
  FILE_TEST_IS_REGULAR    = 1 << 0,
  FILE_TEST_IS_SYMLINK    = 1 << 1,
  FILE_TEST_IS_DIR        = 1 << 2,
  FILE_TEST_IS_EXECUTABLE = 1 << 3,
  FILE_TEST_EXISTS        = 1 << 4
} FileTest;

int file_test (const char *filename, FileTest test)
{ if ((test & FILE_TEST_EXISTS) && (access(filename, F_OK) == 0))
    return TRUE;
  
  if ((test & FILE_TEST_IS_EXECUTABLE) && (access(filename, X_OK) == 0))
    {
      if (getuid() != 0)
	return TRUE;

      /* For root, on some POSIX systems, access (filename, X_OK)
       * will succeed even if no executable bits are set on the
       * file. We fall through to a stat test to avoid that.
       */
    }
  else
    test &= ~FILE_TEST_IS_EXECUTABLE;

  if (test & FILE_TEST_IS_SYMLINK)
    {
      struct stat s;

      if ((lstat (filename, &s) == 0) && S_ISLNK (s.st_mode))
        return TRUE;
    }
  
  if (test & (FILE_TEST_IS_REGULAR |
	      FILE_TEST_IS_DIR |
	      FILE_TEST_IS_EXECUTABLE))
    {
      struct stat s;
      
      if (stat(filename, &s) == 0)
	{
	  if ((test & FILE_TEST_IS_REGULAR) && S_ISREG(s.st_mode))
	    return TRUE;
	  
	  if ((test & FILE_TEST_IS_DIR) && S_ISDIR(s.st_mode))
	    return TRUE;

	  /* The extra test for root when access (file, X_OK) succeeds.
	   */
	  if ((test & FILE_TEST_IS_EXECUTABLE) &&
	      ((s.st_mode & S_IXOTH) ||
	       (s.st_mode & S_IXUSR) ||
	       (s.st_mode & S_IXGRP)))
	    return TRUE;
	}
    }

  return FALSE;
}

static char *my_strchrnul (const char *str, char c)
{ char *p = (char*)str;
  while (*p && (*p != c)) ++p;
  return p;
}

char *find_program_in_path(const char *program)
{ const char *path, *p;
  char *name, *freeme;
  size_t len, pathlen;

  if (program == NULL) return NULL;

  /* If it is an absolute path, or a relative path including subdirectories,
   * don't look in PATH.
   */
  if (strchr(program, DIR_SEPARATOR))
    {
      if (file_test(program, FILE_TEST_IS_EXECUTABLE) &&
	  !file_test(program, FILE_TEST_IS_DIR))
        return strdup(program);
      else
        return NULL;
    }
  
  path = getenv("PATH");

  if (path == NULL)
    {
      /* There is no `PATH' in the environment.  The default
       * search path in GNU libc is the current directory followed by
       * the path `confstr' returns for `_CS_PATH'.
       * In GLib we put . last, for security, and don't use the
       * unportable confstr(); UNIX98 does not actually specify
       * what to search if PATH is unset. POSIX may, dunno.
       */
      path = "/bin:/usr/bin:.";
    }
  
  len = strlen(program)+1;
  pathlen = strlen(path);
  freeme = name = malloc(pathlen+len+1);
  
  /* Copy the file name at the top, including '\0'  */
  memcpy(name+pathlen+1, program, len);
  name = name+pathlen;
  /* And add the slash before the filename  */
  *name = DIR_SEPARATOR;
  
  p = path;
  do
    {
      char *startp;

      path = p;
      p = my_strchrnul(path, SEARCHPATH_SEPARATOR);

      if (p == path)
        /* Two adjacent colons, or a colon at the beginning or the end
         * of `PATH' means to search the current directory.
         */
        startp = name+1;
      else
        startp = memcpy(name - (p-path), path, p-path);

      if (file_test(startp, FILE_TEST_IS_EXECUTABLE) &&
	  !file_test(startp, FILE_TEST_IS_DIR))
        { char *ret;
          ret = strdup(startp);
          free(freeme);
          return ret;
        }
    }
  while (*p++ != '\0');
  free(freeme);
  return NULL;
}

int main(int argc,char **argv)
{ char *path;
  if (argc<2) exit(1);
  if (path=find_program_in_path(argv[1])) printf("%s\n",path);
  else exit(1);
}
