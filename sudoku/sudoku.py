#!/usr/bin/env python -i

# /*
#  * sudoku is a 9x9 grid. each grid has 9 possible numbers it can have.
#  *
#  * pijk <-> number i chosen for grid row j column k
#  */



from ctypes import *
from types  import *
from sat import cnf

pico = cdll.LoadLibrary("/Users/denbuen/lib/libpicosat.so")

alwaysRemoveLearned = 0
N = 9

p = {}

board = '4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'

cnf.init()

for n in board:
  pass

for i in range(0,N):
  p[i] = {}
  for j in range(0,N):
    p[i][j] = {}
    for k in range(0,N):
      p[i][j][k] = cnf.newVar('[%d][%d] = %d' % (j,k,i))

print cnf.numVars()

for j in range(0,N):
  for k in range(0,N):
    clause = []
    for i in range(0,N):
      clause.append(p[i][j][k])
    cnf.addClause('>=1', clause)

for j in range(0,N):
  for k in range(0,N):
    for i in range(0,N):
      for l in range(0,N):
        if i != l:
          cnf.addClause('<=1', [-p[i][j][k], -p[l][j][k]])

for i in range(0,N):
  for j in range(0,N):
    for k in range(0,N):
      for l in range(0,N):
        if l != j:
          cnf.addClause('col_val', [-p[i][j][k], -p[i][l][k]])

for i in range(0,N):
  for j in range(0,N):
    for k in range(0,N):
      for l in range(0,N):
        if l != k:
          cnf.addClause('row_val', [-p[i][j][k], -p[i][j][l]])

for s in range(0,3):
  for t in range(0,3):
    for j in range(s*3, (s+1)*3):
      for k in range(t*3, (t+1)*3):
        for l in range(s*3, (s+1)*3):
          for m in range(t*3, (t+1)*3):
            if j != l and k != m:
              cnf.addClause('3x3', [-p[i][j][k], -p[i][l][m]])

row = 0
col = 0
for b in board:
  if b == '.' or b == '0':
    pass
  else:
    i = int(b)
    print '(%d,%d) = i = %d' % (row,col,i)
    cnf.assume(p[i][row][col])
  col += 1
  if col == 9:
    col = 0
    row += 1

result = cnf.solve()
print result
#   int c;
#   i = 1;
#   while (EOF != (c = fgetc(inputFile))) {
#     if (isdigit(c) || ((char) c) == '.') {
#       solveNext(c);
#       i++;
#     }
#     /* if (i == 5) break; */
#   }

#   picosat_reset();
#   fclose(mapFile);
#   return 0;
# }
