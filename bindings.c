#include <stdio.h>
#include <stdlib.h>
#include <time.h>

char *prompt()
{
   static char str[50];
   scanf("%s", str);
   return str;
}

int print_endline(){
   printf("\n");
   return 0;
}

int print_sameline(char *s){
   printf("%s", s);
   return 0;	
}

int diceThrow()
{
   time_t t;
   srand((unsigned) time(&t));
   return((rand() % 6)+1);
}

int absl(int x){
	return abs(x);
}