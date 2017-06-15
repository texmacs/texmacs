/*
** Denis RAUX - LIX 2013
** avoid calling external lib in case of static functions
*/
#if defined(__TMSTATIC__) && !defined(OS_MINGW) && !defined(__WIN32__)
int dlopen(const char *name, int flag) {
   return(-1);
}
#endif
