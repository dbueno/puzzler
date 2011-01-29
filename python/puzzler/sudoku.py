#!/usr/bin/env python
from types  import *
from sat import cnf
import sys


# if True, prints the resulting table
printResult = True
# if True, check whether the solution is unique
checkUnique = True
humanReadable = True
printStats = False
showUnique = False

numUnique = 0


N = 9
# dictionary of [1-9][row][col]
p = {}
checkAssumps = []

def showBoard(board):
  for row in range(0,N):
    if row != 0:
      print ""
      if (row % 3) == 0:
        print ("-" * 11)
    for col in range(0,N):
      if col != 0 and (col % 3) == 0:
        print "|",
      for i in range(0,N):
        if cnf.assignment(p[i][row][col]):
          sys.stdout.write('%d' % (i+1))
  print ""


def solveBoard(board):
  global numUnique, printResult, checkUnique, showUnique
  def assumeBoard(printBoard):
    for v in checkAssumps:
      cnf.assume(-v)
    row = 0
    col = 0
    for b in board:
      if (printBoard):
        sys.stdout.write(b)
      if b == '.' or b == '0':
        pass
      else:
        i = int(b)
        # print '(%d,%d) = i = %d' % (row,col,i)
        cnf.assume(p[i-1][row][col])
      col += 1
      if col == 9:
        col = 0
        row += 1
        if row == 9:
          break
    if printBoard:
      sys.stdout.write("\n")

  assumeBoard(printResult)
  result = cnf.solve()
  assert result == cnf.RESULT_SAT
  if printResult:
    showBoard(board)

  if checkUnique:
    v = cnf.newVar('uniq')
    c = [-v]
    for j in range(0,N):
      for k in range(0,N):
        for i in range(0,N):
          if cnf.assignment(p[i][j][k]):
            c.append(-p[i][j][k])
    cnf.addClause('uniq', c)
    assumeBoard(False)
    cnf.assume(v)
    checkAssumps.append(v)
    result = cnf.solve()
    if showUnique and result == cnf.RESULT_SAT:
      print ""
      showBoard(board)
    if result == cnf.RESULT_UNSAT:
      numUnique += 1


def setup():
  """
  Sets up universal sudoku constraints. Nothing set up here is specific to the
  actual board being solved.
  """
  cnf.init()
  cnf.pico.picosat_set_seed(0)
  # cnf.pico.picosat_set_global_default_phase(0) # 1361274
  cnf.pico.picosat_set_global_default_phase(1) # 1353908

  # create vars
  for i in range(0,N):
    p[i] = {}
    for j in range(0,N):
      p[i][j] = {}
      for k in range(0,N):
        p[i][j][k] = cnf.newVar('[%d][%d] = %d' % (j,k,i))

  # must choose at least one possibility per square
  for j in range(0,N):
    for k in range(0,N):
      clause = []
      for i in range(0,N):
        clause.append(p[i][j][k])
      cnf.addClause('>=1', clause)

  # must choose at most one possibility per square
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

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser("usage: %prog [options] board-file")
  parser.add_option("-b", action="store_false", dest="humanReadable",
                    help="print machine-readable board")
  parser.add_option("-n", action="store_false", dest="printResult",
                    help="do not print board solutions")
  parser.add_option("-s", action="store_true", dest="printStats",
                    help="print picosat stats at end")
  # parser.add_option("-f", "--file", dest="filename",
  #                   help="write report to FILE", metavar="FILE")
  # parser.add_option("-q", "--quiet",
  #                   action="store_false", dest="verbose", default=True,
  #                   help="don't print status messages to stdout")

  (options, args) = parser.parse_args()

  if len(sys.argv) < 2:
    print "please supply a board file (one board per line) as first argument"
    parser.print_help()
    exit(1)
  print "solving boards from '%s' ..." % sys.argv[1]
  if not printResult:
    print "set printResult in this script if you want the resulting boards"
  setup()
  c = 0
  for line in file(sys.argv[1]):
    if len(line) >= 9*9:
      # print "solving line %d '%s' ..." % (c, line)
      solveBoard(line)
      c += 1
  print "solved %d boards" % c,
  if checkUnique:
    print "/ %d unique" % numUnique
  if printStats:
    cnf.pico.picosat_stats()

