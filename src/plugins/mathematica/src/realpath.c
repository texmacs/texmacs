#include <stdlib.h>
#include <stdio.h>
#define SIZE 4096

int main(int argc, char **argv)
{ char path[SIZE];
  if (argc<2) exit(1);
  if (realpath(argv[1],path)) printf("%s\n",path);
  else exit(1);
}
