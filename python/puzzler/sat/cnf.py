
from ctypes import *
from types import *

pico = cdll.LoadLibrary("libpicosat.so")

LIT_TRUE = 2**32-1
LIT_FALSE = -LIT_TRUE
RESULT_SAT = 'SAT'
RESULT_UNSAT = 'UNSAT'
RESULT_UNKNOWN = 'unknown'

NO_DECISION_LIMIT = -1

allVars = set()
def numVars():
  global allVars
  return len(allVars)
allClauses = []
# nextClause = []
introVars  = []

class LogicGateCounter():
  def __init__(self):
    self.conj = 0
    self.disj = 0
cnt = LogicGateCounter()



# logical operations

def lequal(a, b, o):
  lxor(a, b, -o)

def lxor(a, b, o):
  addClause('xor', [-a,-b,-o])
  addClause('xor', [a,b,-o])
  addClause('xor', [-a,b,o])
  addClause('xor', [a,-b,o])

def land(a, b, o):
  cnt.conj += 1
  name = 'and%d' % cnt.conj
  addClause(name, [a, -o])
  addClause(name, [b, -o])
  addClause(name, [-a, -b, o])

def lor(a, b, o):
  cnt.disj += 1
  name = 'or%d' % cnt.disj
  addClause(name, [-a, o])
  addClause(name, [-b, o])
  addClause(name, [a, b, -o])

def newlIte(c, t, f, name='nobody'):
  if c == SatTrue: return t
  if c == SatFalse: return f
  if t == f: return t

  r = newVar(name)
  lite(c, t, f, r, name)
  return r

def lite(c, t, f, o, name='nobody'):
  (x, y) = (newVar(name + "'"), newVar(name + "''"))
  land(c, t, x)
  land(-c, f, y)
  lor(x, y, o)

def limplies(a, b, o):
  lor(-a, b, o)





# sat solver interface

def assume(*lits):
  s = set(lits)
  for l in s:
    pico.picosat_assume(l)

def addClause(name, lits):
  assert type(name) is StringType
  assert len(lits) > 0
  global allClauses
  s = set(lits)
  allClauses.append((name, s))
  assert len(s) > 0
  for lit in s:
    pico.picosat_add(lit)
  pico.picosat_add(0)

# def addNextClauseLit(l):
#   global nextClause
#   nextClause.append(l)
# def finishClause():
#   global nextClause, allClauses
#   for l in nextClause:
#     pico.picosat_add(l)
#   pico.picosat_add(0)
#   allClauses.append(('finishClause', nextClause))
#   nextClause = []

def numClauses():
  return pico.picosat_added_original_clauses()

def printClauses():
  global allClauses
  cPerL = 4
  i = 0
  for labeledClause in allClauses:
    label = labeledClause[0]
    c = labeledClause[1]
    print "%s " % label,
    print " v ".join(str(l) for l in c),
    i += 1
    if i%cPerL == 0:
      print ""
    else:
      print " & ",


def solve(decLim=NO_DECISION_LIMIT):
  result = pico.picosat_sat(decLim)
  if result == 0:
    return RESULT_UNKNOWN
  elif result == 10:
    return RESULT_SAT
  elif result == 20:
    return RESULT_UNSAT
  else:
    raise Exception("unknown result from picosat: %d" % result)

def assignment(l):
  r = pico.picosat_deref(l)
  assert r != 0
  if r == -1:
    return False
  elif r == 1:
    return True

def init():
  global allVars, allClauses, nextClause
  allVars.clear()
  allClauses = []
  nextClause = []

  pico.picosat_init()

def reset():
  global allVars
  allVars.clear()
  pico.picosat_reset()

def newVar(name="nobody"):
  global allVars, introVars
  l = pico.picosat_inc_max_var()
  allVars.add(l)
  if name != 'nobody':
    introVars.append( (l, name) )
  return l
