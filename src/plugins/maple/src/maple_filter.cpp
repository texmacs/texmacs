
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <string>
using namespace std;

/*#define DATA_BEGIN   ((string) "debut" )
#define DATA_END     ((string) "fin" )
#define DATA_ESCAPE  ((string) "escape" )*/

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

static int counter= 0;

void
next_input () {
  counter++;
  cout << DATA_BEGIN << "channel:prompt" << DATA_END;
  cout << DATA_BEGIN << "latex:\\brown Maple " << counter
       << "]\\ " << DATA_END;
}

string
remplacer(string in, string what, string by) {
  string result= in;
  while (result.find(what)!=string::npos) {
    result= result.replace (result.find(what), what.length(), by);
  }
  return result;
}

void
passer (string &s, int indentation) {
  s= remplacer (s, "\\textbf{proc}", "\\proc");  
  s= remplacer (s, "\\textbf{end proc}", "\\endproc \n");
  s= remplacer (s, "\\textbf{if}", "\\if");  
  s= remplacer (s, "\\textbf{then}", "\\then"); 
  s= remplacer (s, "\\textbf{else}", "\\else"); 	
  s= remplacer (s, "\\textbf{end if}", "\\endif");
  
  s= remplacer(s, "\\,", "\\mult ");
  // l'espace correspond souvent a une multiplication,
  // pas toujours malheureusement!
  s= remplacer(s, "\\ast ", "\\mult ");// 	* =\\mult
  // s= remplacer(s,"\\mathrm","\\mathrom");
  // Joris devrait arranger ce probleme de conversion LaTeX
  // a moins que je ne trouve qqc
  // Maple est un peu incoherent a ce sujet
  s= remplacer (s, "{\\displaystyle ", "\\dsp{ ");
  //on peut faire mieux?

  s= remplacer (s, "(", " \\bigpl ");
  //les parentheses deviennent modulables cf session_maple.ts
  s= remplacer (s, ")", " \\bigpr ");
  //modulables cf session_maple.ts
  
  if (indentation == 0) {
    s= remplacer (s, "\\maplemultiline{","\\hfill $");
    //hfill donne un prob avec les procedures qui sont indentees
    s= remplacer (s, "\\\\","$\\hfill\\  \\cr \\  \\hfill $ ");
    //idem: prob avec les procedures
  }
  else s= remplacer (s, "\\maplemultiline{","$");
  cout <<s;
}

int
search_matching (char entree[], int &niveau) {
  int pos=0;
  while (pos<=strlen(entree)) {
    switch(entree[pos]) {
    case '{': niveau++;break; 
    case '}': niveau--;break;
    }
    if (niveau==0) break;pos++;
  }
  return pos;
}

int
main() {
  int c,d;
  const int max_length=1000;
  char buffer[max_length];

  /* passer l'annonce et afficher le premier prompt */
  /*lire le premier*/
  c=0;
  cout << DATA_BEGIN << "verbatim:";
  cout << "    |\\^/|     Maple \n";
  cout << "._|\\|   |/|_. Copyright by Waterloo Maple Inc\n";
  cout << " \\  MAPLE  /  All rights reserved. Maple is a registered trademark of\n";
  cout << " <____ ____>  Waterloo Maple Inc.\n";
  cout << "      |       Type ? for help.\n";
  cout << "\n";
  cout << "Interface with TeXmacs by Christian Even (c) 2002.\n";
  next_input ();
  cout << DATA_END;
  fflush(stdout);

  /* lecture de l'annonce et premiere entree */
  while(1) {
    cin.getline(buffer,max_length);
    if (strcmp (buffer,"\\begin{mapleinput}" ) == 0) break;
  } /*while*/
 
  /*boucle principale*/
  while (true) { 
    c=0;
    while(true) { /*boucle entree/sortie*/
      int i,j,multi;	
      cin.getline(buffer,max_length);
      string debut(buffer,0,11);
	
      // comment lire l'aide en ligne?
      // if (buffer[0]='?') {
      //   lire tout sans convertir en string jusqu'a \begin{mapleinput}  ???} 
      // else

      if (debut=="\\mapleplot{" ) {
	c=0;
	i=11;
	while (buffer[i] != '}') i++;
	string nomduplot(buffer,11,i-11);
	string message="cat $HOME/" + nomduplot;
	cout << DATA_BEGIN << "latex:\\hfill" << DATA_END;
	cout << DATA_BEGIN << "ps:";
	fflush(stdout);
	
	i=0;
	while (i<=100000000) i++;
	// boucle de temporisation: a ameliorer!!!!!
	// il faudrait tester l'arrivee du plot.eps

	system(message.c_str());
	cout << DATA_END;
	cout << DATA_BEGIN << "latex:\\hfill" << DATA_END;		 
	cout <<"\n";
      }
      else if (strcmp (buffer, "quit;" ) == 0) break;
      else if (strcmp (buffer, "quit" ) == 0) break;
      else if (strcmp (buffer,"\\begin{mapleinput}" ) == 0) { c=0; break; }
      else if (strcmp (buffer,"\\end{mapleinput}" ) == 0) {
	cout << DATA_BEGIN << "verbatim:"; c=1; }

      else if (strcmp (buffer,"\\begin{maplelatex}" ) == 0) {
	c=1;
	cout << DATA_BEGIN << "latex:";
	cin.getline(buffer,max_length);
	while (strcmp(buffer,"\\end{maplelatex}" ) != 0) {
	  if (strcmp (buffer,"\\maplemultiline{" ) == 0) {
	    int niveau =1;
	    int pos;
	    int indentation=0;
	    string sortielatex="\\maplemultiline{";
	    niveau=1;
	    cin.getline (buffer, max_length);
	    pos=search_matching (buffer, niveau);
	    while (true) {
	      pos= search_matching (buffer, niveau);
	      if (niveau==0) break;
	      sortielatex= sortielatex+buffer;
	      cin.getline (buffer, max_length);
	    } // fin du while
	    string debut (buffer, 0, pos);
	    sortielatex=sortielatex+ debut;
	    if (sortielatex.find ("\\textbf{proc}") != string::npos)
	      indentation =1;
	    // cout <<"l'indentation vaut "<< indentation <<"\n";
	    passer (sortielatex, indentation);
	    // string fin(buffer,pos+1,max_length-pos);
	    cout << "$\\hfill \\cr";
	    cin.getline (buffer, max_length);
	  }  	
	  /* else {string sortielatex(buffer,0,strlen(buffer));
	     passer(sortielatex,0);
	     cin.getline(buffer,max_length);}; */
	  else {
	    string sortielatex(buffer,0,strlen(buffer));
	    while (strcmp (buffer,"\\]" ) != 0) {
	      cin.getline(buffer,max_length);
	      string suite(buffer,0,strlen(buffer));	
	      sortielatex=sortielatex+suite;
	    }
	    passer(sortielatex,0);
	    cin.getline(buffer,max_length);
	  }
	} // fin du while latex
	cout <<DATA_END;
      }

      // else if (strcmp (buffer,"\\end{maplelatex}" ) == 0) cout << DATA_END;
      else if (strcmp (buffer," \\[ " ) == 0) cout << buffer;
      else if (strcmp (buffer," \\] " ) == 0) cout << buffer;
      else if (strcmp (buffer,"\\begin{maplettyout}" ) == 0) c= 1;
      else if (strcmp (buffer,"\\end{maplettyout}" ) == 0) c= 1;
      else if (c==1)  fputs(buffer,stdout); fputs("\n",stdout);
      // cout << buffer;
    } /* fin de la boucle entree/sortie* */

    if (strcmp (buffer, "quit" ) == 0) {
      cout << DATA_END << DATA_BEGIN
	   << "latex: \\red $$\\mathrm{La session Maple est termin\\'ee}$$";
      cout << DATA_END;
      cout << DATA_ESCAPE;fflush(stdout);break;
    }
 
    if (!cin) {
      cout<< "La session Maple est arretee.3";
      cout << DATA_ESCAPE;
      cout << DATA_END;    
      fflush(stdout);
      break;
    }

    if (strcmp (buffer, "quit;" ) == 0) {
      cout << "La session est arretee.1";
      cout << DATA_ESCAPE;
      cout << DATA_END;
      fflush(stdout);
      break;
    }
    
    next_input ();
    cout << DATA_END;
    fflush(stdout);
  } // fin de la boucle  principale

  return 0;
} // main
