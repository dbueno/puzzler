/*
 * sudoku is a 9x9 grid. each grid has 9 possible numbers it can have.
 *
 * pijk <-> number i chosen for grid row j column k
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <picosat.h>

/* [possible-num][row][col] */
static int p[9][9][9];
FILE *mapFile;
FILE *coreFile;
FILE *cnfFile;


int main(int argc, char **argv)
{
  int i,j,k,l;

  picosat_init();
  picosat_enable_trace_generation();
  mapFile = fopen("var-map.txt", "w"), assert(mapFile);
  coreFile = fopen("clausal-core.txt", "w"), assert(coreFile);
  cnfFile = fopen("general.cnf", "w"), assert(cnfFile);

  /* create vars */
  for (i = 0; i < 9; i++) {
    for (j = 0; j < 9; j++) {
      for (k = 0; k < 9; k++) {
        p[i][j][k] = picosat_inc_max_var();
        fprintf(mapFile, "%d = p[%d][%d][%d]\n",
                p[i][j][k], i, j, k);
      }
    }
  }
  fprintf(stderr, "%d variables\n", picosat_variables());

#define LINE(i,ii,ie, j,ji,je, k,ki,ke, body)  \
  for (i = ii; i < ie; i++) {                   \
    for (j = ji; j < je; j++) {                 \
      for (k = ki; k < ke; k++) {               \
        body;                                   \
      }                                         \
    }                                           \
  }

  /* each cell set to exactly one value */
  LINE(i,0,9, j,0,9, k,0,9, {
      for (l = 0; l < 9; l++) {
        if (l != i) {
          /* pijk -> !pljk (when l!=i) */
          picosat_add(-p[i][j][k]);
          picosat_add(-p[l][j][k]);
          picosat_add(0);
        }
      }
    });
  
  /* row: no cell has the same value */
  LINE(i,0,9, j,0,9, k,0,9,{
      for (l = 0; l < 9; l++) {
        if (l != j) {
          picosat_add(-p[i][j][k]);
          picosat_add(-p[i][l][k]);
          picosat_add(0);
        }
      }
    });
      

  /* column: no cell has the same value */
  LINE(i,0,9, j,0,9, k,0,9, {
      for (l = 0; l < 9; l++) {
        if (l != k) {
          /* pijk -> !pijl (when l!=k)*/
          picosat_add(-p[i][j][k]);
          picosat_add(-p[i][j][l]);
          picosat_add(0);
        }
      }
    });


#define SQUARE(i, j,ji,je, k,ki,ke)             \
  LINE(i,0,9, j,ji,je, k,ki,ke, {              \
      for (l = ki; l < ke; l++) {               \
        if (l != k) {                           \
          picosat_add(-p[i][j][k]);             \
          picosat_add(-p[i][j][l]);             \
          picosat_add(0);                       \
        }                                       \
      }                                         \
      for (l = ji; l < je; l++) {               \
        if (l != j) {                           \
          picosat_add(-p[i][j][k]);             \
          picosat_add(-p[i][l][k]);             \
          picosat_add(0);                       \
        }                                       \
      }                                         \
    });

  int m,n;                      /* m,n count squares */
  /* 3x3 square: no cell has the same value */
  for (m = 0; m < 3; m++) {
    for (n = 0; n < 3; n++) {
      /* squares start at 0, 3, 6 (row and col) */
      SQUARE(i, j,m*3,m+3, k,n*3,n+3);
    }
  }
  fprintf(stderr, "%d clauses\n", picosat_added_original_clauses());
  picosat_print(cnfFile);


  /* read board */
  fprintf(stderr, "reading board from stdin ...\n");
  int in = fgetc(stdin);
  int row = 0, col = 0;
  while (in != EOF)
  {
    char c = (char) in;
    fprintf(stderr, "%c", c);
    if (c > '0' && c <= '9') {
      int num = c - '0';
      /* fprintf(stderr, "p[%d][%d][%d] = 1\n", num, row, col); */
      /* picosat_assume(p[num][row][col]); */
      picosat_add(p[num-1][row][col]);
      picosat_add(0);
      for (i = 0; i < 9; i++) {
        /* if (i != num-1) { */
        /*   /\* picosat_assume(-p[i][row][col]); *\/ */
        /*   picosat_add(-p[i][row][col]); */
        /*   picosat_add(0); */
        /* } */
      }
    }

    if (col == 8) { row++, col = 0; fprintf(stderr, "\n"); }
    else col++;

    if (row >= 9) break;

    in = fgetc(stdin);
  }
  assert(in != EOF);

  assert(0);
  picosat_print(stdout);
  fprintf(stderr, "solving ...\n");
  int result = picosat_sat(-1);
  switch (result) {
  case PICOSAT_SATISFIABLE:
    fprintf(stderr, "solution found\n");
    for (row = 0; row < 9; row++) {
      for (col = 0; col < 0; col++) {
        for (i = 0; i < 9; i++) {
          if (picosat_deref(p[i][row][col])) {
            printf("%d", i);
            break;
          }
        }
      }
      printf("\n");
    }

  case PICOSAT_UNSATISFIABLE:
    fprintf(stderr, "bad!\n");
    picosat_write_clausal_core(coreFile);
    break;

  default:
    assert(0);
  }

  fclose(mapFile), fclose(coreFile);
  return 0;
}
