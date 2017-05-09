#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

// get length of a string
int getLen(char* x){
   return strlen(x);
}

// get user input
int prompt(char* msg)
{
   // static char str[50];
   printf("%s",msg);
   static int x;
   scanf("%d", &x);
   return x;
}

// print to console with a newline
int print_endline(){
   printf("\n");
   return 0;
}

// print string value of integer
int print_int_sameline(int x){
   printf("%d",x);
   return 0;
}

// print to console without a newline
int print_sameline(char *s){
   printf("%s", s);
   return 0;	
}

// random number generator
int diceThrow()
{
   time_t t;
   srand((unsigned) time(&t));
   return((rand() % 6)+1);
}

// return absolute value of an integer
int absl(int x){
	return abs(x);
}