
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <string>
using namespace std;

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  int c,first,d;
  // passer l'annonce et afficher le premier prompt
  fflush(stdout);

  while (true) {
    char buffer[300];
    cout<<"\\begin{mapleinput}\n";
    cin.getline(buffer,300);
    // string debut(buffer,0,11);
    // if (debut=="\\mapleplot{" ) {
    //   cout << buffer;
    //   fflush(stdout);
    //   cin.getline(buffer,200);
    // }

    if ((strcmp (buffer, "quit") == 0) || (strcmp (buffer, "quit;") == 0)) {
      cout << buffer; fflush(stdout); break; }
    cout << buffer;
    cout << "\n\\end{mapleinput}\n";
    fflush(stdout);
  } // while

  return 0;
} // main
