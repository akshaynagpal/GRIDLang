#include <stdio.h>

char *input()
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

