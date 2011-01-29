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


# assumptions for solving. we use this so that uniqueness constraints from the
# last board don't mess up solving the next board.
checkAssumps = []
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
    c = []
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
      print "non-unique board"
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

  # columns have 1-9 at most once
  for i in range(0,N):
    for r in range(0,N):
      for c in range(0,N):
        for r2 in range(0,N):
          if r2 != r:
            cnf.addClause('col_val', [-p[i][r][c], -p[i][r2][c]])

  # rows have 1-9 at most once
  for i in range(0,N):
    for r in range(0,N):
      for c in range(0,N):
        for c2 in range(0,N):
          if c2 != c:
            cnf.addClause('row_val', [-p[i][r][c], -p[i][r][c2]])

  # 3x3 grids have 1-9 at most once
  for s in range(0,3):
    for t in range(0,3):
      for r in range(s*3, (s+1)*3):
        for c in range(t*3, (t+1)*3):
          for r2 in range(s*3, (s+1)*3):
            for c2 in range(t*3, (t+1)*3):
              # comment this out to see if your python doesn't suck.
              # it doesn't error iff it sucks
              for i in range(0,N):
                if r != r2 and c != c2:
                  # print "%d %d %d -- %d %d %d" % (i,r,c, i,r2,c2)
                  cnf.addClause('3x3', [-p[i][r][c], -p[i][r2][c2]])

if __name__ == "__main__":
  from optparse import OptionParser
  parser = OptionParser("usage: %prog [options] board-file")
  parser.add_option("-b", action="store_false", dest="humanReadable", default=True,
                    help="print machine-readable board")
  parser.add_option("-n", action="store_false", dest="printResult", default=True,
                    help="do not print board solutions")
  parser.add_option("-s", action="store_true", dest="printStats", default=False,
                    help="print picosat stats at end")
  parser.add_option("-u", action="store_true", dest="showUnique", default=False,
                    help="show a second solution for non-unique board")
  # parser.add_option("-f", "--file", dest="filename",
  #                   help="write report to FILE", metavar="FILE")
  # parser.add_option("-q", "--quiet",
  #                   action="store_false", dest="verbose", default=True,
  #                   help="don't print status messages to stdout")

  (options, args) = parser.parse_args()
  showUnique = options.showUnique
  printResult = options.printResult
  humanReadable = options.humanReadable
  printStats = options.printStats

  if len(sys.argv) < 2:
    print "please supply a board file (one board per line) as first argument"
    parser.print_help()
    exit(1)
  print "solving boards from '%s' ..." % args[0]
  if not printResult:
    print "set printResult in this script if you want the resulting boards"
  setup()
  c = 0
  for line in file(args[0]):
    if len(line) >= 9*9:
      # print "solving line %d '%s' ..." % (c, line)
      solveBoard(line)
      c += 1
  print "solved %d boards" % c,
  if checkUnique:
    print "/ %d unique" % numUnique
  if printStats:
    cnf.pico.picosat_stats()

