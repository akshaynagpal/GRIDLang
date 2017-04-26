#include <stdio.h>

char *input()
{
static char str[50];
scanf("%s", str);
return str;
}

void pretty_print(int rows,int cols,int arr[][cols]){
	int num_row = rows;
	int num_col = cols;
	int i,j;
	for(i=0;i<num_row;i++){
		for (int j = 0; j < num_col; j++)
		{
			printf("|%d",arr[i][j]);
		}
		printf("|\n");
	}
}