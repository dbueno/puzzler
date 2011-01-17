/*
 * sudoku is a 9x9 grid. each grid has 9 possible numbers it can have.
 *
 * pijk <-> number i chosen for grid row j column k
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <picosat.h>

#define N 9
#define picosat_deref_boolb(l)


/* [possible-num][row][col] */
static int p[N][N][N];
FILE *mapFile;
FILE *coreFile;
FILE *cnfFile;
FILE *inputFile;


int main(int argc, char **argv)
{
  int i,j,k,l,m;

  picosat_init();
  picosat_enable_trace_generation();

  mapFile = fopen("var-map.txt", "w"), assert(mapFile);
  coreFile = fopen("clausal-core.txt", "w"), assert(coreFile);
  cnfFile = fopen("general.cnf", "w"), assert(cnfFile);
  if (0 == strcmp(argv[1], "-") || argc <= 1) {
    inputFile = stdin;
  } else {
    inputFile = fopen(argv[1], "r");
  }

  /* create vars */
  for (j = 0; j < N; j++) {
    for (k = 0; k < N; k++) {
      for (i = 0; i < N; i++) {
        p[i][j][k] = picosat_inc_max_var();
        fprintf(mapFile, "%d = p[%d][%d][%d]\n", p[i][j][k], i, j, k);
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

  /* each cell set to at least one value */
  for (j = 0; j < N; j++) {
    for (k = 0; k < N; k++) {
      for (i = 0; i < N; i++) {
        picosat_add(p[i][j][k]);
      }
      picosat_add(0);
    }
  }

  /* each cell set to at most one value */
  for (j = 0; j < N; j++) {
    for (k = 0; k < N; k++) {
      for (i = 0; i < N; i++) {
        for (l = 0; l < N; l++) {
          if (i != l) {
            /* pijk -> !pljk (when l!=i) */
            picosat_add(-p[i][j][k]);
            picosat_add(-p[l][j][k]);
            picosat_add(0);
          }
        }
      }
    }
  }


  /* column: no cell has the same value */
  LINE(i,0,N, j,0,N, k,0,N, {
      for (l = 0; l < N; l++) {
        if (l != j) {
          picosat_add(-p[i][j][k]);
          picosat_add(-p[i][l][k]);
          picosat_add(0);
        }
      }
    });

      

  /* row: no cell has the same value */
  LINE(i,0,N, j,0,N, k,0,N, {
      for (l = 0; l < N; l++) {
        if (l != k) {
          picosat_add(-p[i][j][k]);
          picosat_add(-p[i][j][l]);
          picosat_add(0);
        }
      }
    });


#define SQUARE(i, j,ji,je, k,ki,ke)             \
  /* column: no cell has the same value */      \
    LINE(i,0,N, j,ji,je, k,ki,ke, {             \
        for (l = ji; l < je; l++) {             \
          if (l != j) {                         \
            picosat_add(-p[i][j][k]);           \
            picosat_add(-p[i][l][k]);           \
            picosat_add(0);                     \
          }                                     \
        }                                       \
      });                                       \
    /* row: no cell has the same value */       \
    LINE(i,0,N, j,ji,je, k,ki,ke, {             \
        for (l = ki; l < ke; l++) {             \
          if (l != k) {                         \
            picosat_add(-p[i][j][k]);           \
            picosat_add(-p[i][j][l]);           \
            picosat_add(0);                     \
          }                                     \
        }                                       \
    });


  /* 3x3 blocks: all unique numbers */
  int s,t;                      /* count squares */
  for (s = 0; s < 3; s++) {
    for (t = 0; t < 3; t++) {
      for (i = 0; i < 9; i++) {
        for (j = s*3; j < (s+1)*3; j++) {
          for (k = t*3; k < (t+1)*3; k++) {
            for (l = s*3; l < (s+1)*3; l++) {
              for (m = t*3; m < (t+1)*3; m++) {
                if (j != l && k != m) {
                  picosat_add(-p[i][j][k]);
                  picosat_add(-p[i][l][m]);
                  picosat_add(0);
                }
              }
            }
          }
        }
      }
    }
  }

#if 0

  /* 3x3 square: no cell has the same value */
  /* for (m = 0; m < 3; m++) { */
  /*   for (n = 0; n < 3; n++) { */
  /*     /\* squares start at 0, 3, 6 (row and col) *\/ */
  /*     SQUARE(i, j,m*3,m+3, k,n*3,n+3); */
  /*   } */
  /* } */

#endif
  fprintf(stderr, "%d clauses\n", picosat_added_original_clauses());


  /* read board */
  fprintf(stderr, "reading board ...\n");
  int in = fgetc(inputFile);
  int row = 0, col = 0;
  while (in != EOF)
  {
    char c = (char) in;
    if (0 == (col % 3) && col != 0) {
      fprintf(stderr, " ");
    }
    fprintf(stderr, "%c", c);
    if (c > '0' && c <= '9') {
      int num = c - '0';
      /* if (c != 0) fprintf(stderr, "p[%d][%d][%d] (%d) = 1\n", num-1, row, col, p[num-1][row][col]); */
      /* picosat_assume(p[num][row][col]); */
      picosat_add(p[num-1][row][col]);
      picosat_add(0);
      for (i = 0; i < N; i++) {
        /* if (i != num-1) { */
        /*   /\* picosat_assume(-p[i][row][col]); *\/ */
        /*   picosat_add(-p[i][row][col]); */
        /*   picosat_add(0); */
        /* } */
      }
    }

    if (col == 8) {
      row++, col = 0;
      fprintf(stderr, "\n");
      if (row != 0 && row != 9 && 0 == (row % 3)) printf("\n");
    }
    else col++;

    if (row >= N) break;

    in = fgetc(inputFile);
  }
  assert(in != EOF);

  fprintf(stderr, "solving ...");
  picosat_print(cnfFile);
  int result = picosat_sat(-1);
  switch (result) {
  case PICOSAT_SATISFIABLE:
    fprintf(stderr, " solution found\n");
    for (row = 0; row < N; row++) {
      if (row != 0 && 0 == (row % 3)) printf("\n");
      for (col = 0; col < N; col++) {
        int printed = 0;
        for (i = 0; i < N; i++) {
          if (1 == picosat_deref(p[i][row][col])) {
            if (0 == (col % 3) && col != 0) {
              printf(" ");
            }
            printf("%c", i+1 + '0');
            printed = 1;
            for (j = i+1; j < N; j++) {
              assert(-1 == picosat_deref(p[j][row][col]));
            }
            break;
          }
        }
        assert(printed);
      }
      printf("\n");
    }
    break;

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
