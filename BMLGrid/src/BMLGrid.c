#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<R_ext/Arith.h>
#include<R.h>

/**
 * Function: move
 * ------------------------------------------------------
 * move different kind of cars in one step(by color)
 * parameters:
 *            *r: the number of rows of a grid
 *            *c: the number of columns of a grid
 *            *grid: the content of grid matrix (stored by columns)
 *            *color: which kind of car need to move(1 means red and 2 means blue)
 *            *velocity: the velocity of cars in this moving step    
 */
void move(int *r, int *c, int *grid, int *color, double *velocity)
{

    int tot = r[0] * c[0];   /*the total number of elements in grid*/
    

    /*get the number of cars in that color*/
    int numColor = 0;
    for(int j = 0; j < tot; j++){
    	if(grid[j] == *color )	{
    		numColor = numColor + 1;
    	}
    }
    
    /*If there is no car in that color, set velocity to 0 and no need to move*/
    if(numColor == 0){
        *velocity = (double)0;
        return;
    }

    /*record current position and next position of cars which can move*/
    int* curPos = (int *) malloc ( sizeof(int) * numColor );
    int* nexPos = (int *) malloc ( sizeof(int) * numColor );

    if(curPos == NULL || nexPos == NULL){
        return;
    }else{
        for(int i = 0; i < numColor; i++){
            curPos[i] = 0;
            nexPos[i] = 0;
        }
    }
    
    

    int k = 0;   /*the number of moveable cars*/
    int nexPosI = 0;

    
    if(*color == 1){
        /*move red cars to the right*/
        for(int i = 0; i < tot; i++){
            if(grid[i] == color[0]){
                
                /*compute the next position*/
                nexPosI = i + r[0];
                if(nexPosI >= tot)
                    nexPosI = nexPosI - tot;

                /*if the car can move, record current position and next position*/
                if(grid[nexPosI] == 0)
                {
                    curPos[k] = i;
                    nexPos[k] = nexPosI;
                    k = k + 1;
                }   
            }
        }

    }else if(*color == 2){
        /*move blue cars upwards*/
        int numColum = 0;
        int numRow = 0;
        for(int i = 0; i < tot; i++){
            if(grid[i] == color[0]){

                /*compute the next position*/
                numColum = i / r[0];
                numRow = i % r[0] ;
                nexPosI = numRow + 1;
                if(nexPosI >= r[0])
                    nexPosI = nexPosI - r[0];
                nexPosI = numColum * r[0] + nexPosI;

                /*if the car can move, record current position and next position*/
                if(grid[nexPosI] == 0)
                {
                    curPos[k] = i;
                    nexPos[k] = nexPosI;
                    k = k + 1;
                }   
            }
        }

    }else{
        *velocity = (double)0;
        return;
    }

    velocity[0] = (double)k / (double)numColor; /*update velocity*/

    /*change positions in grid*/
    for(int i = 0; i < k; i++){
    	grid[curPos[i]] = 0;
    	grid[nexPos[i]] = *color;

    }

    free(curPos);
    free(nexPos);
    curPos = NULL;
    nexPos = NULL;

}


/**
 * Function: move2
 * ------------------------------------------------------
 * another method of moving different kind of cars in one step(by color)
 * parameters:
 *            *r: the number of rows of a grid
 *            *c: the number of columns of a grid
 *            *grid: the content of grid matrix (stored by columns)
 *            *color: which kind of car need to move(1 means red and 2 means blue)
 *            *velocity: the velocity of cars in this moving step    
 */
void move2(int *r, int *c, int *grid, int *color, double *velocity)
{

    int tot = r[0] * c[0];  /*the total number of elements in grid*/
    
    /*copy content ofgrid to nexGrid*/
    int* nexGrid = (int *) malloc ( sizeof(int) * tot );
    if(nexGrid == NULL){
        return;
    }else{
        for(int i = 0; i < tot; i++){
            nexGrid[i] = 0;
        }
    }

    memcpy(nexGrid, grid, tot * sizeof(int));


    int numColor = 0; /*the number of cars in that color*/
    int numMove = 0; /*the number of moveable cars*/
    int nexPosI = 0; /*inner variable to record next postion of each car*/

    if(color[0] == 1){
        /*move red cars to the right*/
    	for(int i = 0; i < tot; i++){
    		if(nexGrid[i] == color[0]){
    			numColor = numColor + 1;

                /*compute new position*/
    			nexPosI = i + r[0];
    			if(nexPosI >= tot)
    				nexPosI = nexPosI - tot;

                /*if the car can move, update grid directly*/
    			if(nexGrid[nexPosI] == 0){
    				grid[nexPosI] = color[0];
    				grid[i] = 0;
    				numMove = numMove + 1;
    			}
    		}
    	}
    }else if(color[0] == 2){
        /*move blue cars upwards*/
    	int numColum = 0;
    	int numRow = 0;

    	for(int i = 0; i < tot; i++){
    		if(nexGrid[i] == color[0]){
    			numColor = numColor + 1;

                /*compute new position*/
    			numColum = i / r[0];
    			numRow = i % r[0] ;
    			nexPosI = numRow + 1;
    			if(nexPosI >= r[0])
    				nexPosI = 0;
    			nexPosI = numColum * r[0] + nexPosI;	

                /*if the car can move, update grid directly*/
    			if(nexGrid[nexPosI] == 0){

    				grid[nexPosI] = color[0];
    				grid[i] = 0;
    				numMove = numMove + 1;
    			}
    		}
    	}

    }

    if (numColor == 0)
    {
        velocity[0] = (double)0;
    }else{
        velocity[0] = (double)numMove / (double)numColor; /*update velocity*/
    }
    

    free(nexGrid);
    nexGrid = NULL;

}




/**
 * Function: runSteps
 * ------------------------------------------------------
 * move the cars several times
 * parameters:
 *            *numSteps: number of runs
 *            *r: the number of rows of a grid
 *            *c: the number of columns of a grid
 *            *grid: the content of grid matrix (stored by columns)
 *            *velocity: the velocity of cars in each step  
 */
void runSteps(int *numSteps, int *r, int *c, int *grid, double *velocity)
{
	double v = 0;
	double *p1 = &v;

	int color = 1;
	int *c1 = &color;

	for(int i = 0; i < numSteps[0]; i++){
        /*move blue cars first and then red cars and replicate*/
		if(i%2 == 0)
			*c1 = 2;
		else
			*c1 = 1;

		move(r, c, grid, c1, p1);
		velocity[i] = *p1;
	}

}


void runSteps2(int *numSteps, int *r, int *c, int *grid, double *velocity)
{
    double v = 0;
    double *p1 = &v;

    int color = 1;
    int *c1 = &color;

    for(int i = 0; i < numSteps[0]; i++){
        /*move blue cars first and then red cars and replicate*/
        if(i%2 == 0)
            *c1 = 2;
        else
            *c1 = 1;

        move2(r, c, grid, c1, p1);
        velocity[i] = *p1;
    }

}



