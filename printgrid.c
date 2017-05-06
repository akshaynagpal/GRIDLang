#include <stdio.h>
#include <string.h>

// prints grid with equal sized columns 
void pretty_print(int rows,int cols, const char *arr[][4]){
	int i,j,k;
	int max_width=0,rem_space=0;

	// finding max width
	for(i=0;i<rows;i++){
		for (int j = 0; j < cols; j++)
		{
			if(strlen(arr[i][j])>max_width){
				max_width = strlen(arr[i][j]);
			}
		}
	}

	// printing the upper border 
	int border_len = max_width * cols + (cols+1);
	for (i = 0; i < border_len; ++i)
	{
		printf("_");
	}

	printf("\n");

	// printing the grid
	for(i=0;i<rows;i++){
		for (j = 0; j < cols; j++)
		{
			printf("|%s",arr[i][j]);
			rem_space = max_width - strlen(arr[i][j]);
			if(rem_space>0){
				for(k=0; k<rem_space;k++){
					printf(" ");
				}
			}
		}
		printf("|\n");
	}

	// printing the lower border
	for (i = 0; i < border_len; ++i)
	{
		printf("-");
	}
}

int main(int argc, char const *argv[])
{
	const char* grid[3][4];
	grid[0][0] = "P2";
	grid[0][1] = "P1";
	grid[0][2] = "King";
	grid[0][3] = "bye";
	grid[1][0] = "P2";
	grid[1][1] = "King";
	grid[1][2] = "yo";
	grid[1][3] = "bye";
	grid[2][0] = "P2";
	grid[2][1] = "helloooo";
	grid[2][2] = "King";
	grid[2][3] = "P1";
	pretty_print(3,4,grid);
	return 0;
}