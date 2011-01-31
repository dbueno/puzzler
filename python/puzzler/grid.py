#!/usr/bin/env python
from sat import cnf
import sys




salaries = [86000, 139000, 188000, 211000, 217000]
names    = ['braxton', 'ellie', 'guadalupe', 'hayley', 'valerie']
penpals  = ['comoran', 'congolese', 'mozambican', 'panamanian', 'urugayan']
cookies  = ['blackwhite', 'chocolatechip', 'gingerbread', 'oatmealraisin', 'peanutbutter']


class Grid():
  def __init__(self, name, rows, cols):
    self.tbl = {}
    self.rows = rows
    self.cols = cols
    for r in rows:
      self.tbl[r] = {}
      clause = []
      for c in cols:
        self.tbl[r][c] = cnf.newVar(name)
        clause.append( self.tbl[r][c] )
      cnf.addClause('>=1', clause)
    for r in rows:
      for c1 in cols:
        for c2 in cols:
          if c1 != c2:
            cnf.addClause('<=1', [-self.tbl[r][c1], -self.tbl[r][c2]])

    def entry(row, col):
      assert row in self.rows
      assert col in self.cols
      return self.tbl[row][col]

cnf.init()

sn = Grid('sal_with_name', salaries, names)
sp = Grid('sal_with_penpal', salaries, penpals)
sc = Grid('sal_with_cookie', salaries, cookies)
cn = Grid('cookie_with_name', cookies, names)
pn = Grid('penpal_with_name', penpals, names)

# - The employee with the $211,000 salary is Ellie.
cnf.addClause([ sn.entry(211000, 'ellie') ])
# - The baker who made chocolate chip cookies is not Braxton.
cnf.addClause([ -cn.entry('chocolatechip', 'braxton') ])
# - Guadalupe earns less than Valerie.
for valSal in salaries:
  for guaSal in salaries:
    options = []
    if guaSal < valSal:
      options.add(cnf.lnewOr(sn.entry(guaSal, 'guadalupe'))
      cnf.limplies(sn.entry(guaSal, 'guadalupe'), -sn.entry(valSal, 'valerie'))
# - The 5 people were the employee with the $188,000 salary, the baker who made
# peanut butter cookies, the person with the Comoran penpal, Hayley, and the
# employee with the $217,000 salary.
distinct([ 188000, 'peanutbutter', 'comoran', 'hayley', 217000 ])
# - Braxton earns more than the baker who made gingerbread cookies.
for braxSal in salaries:
  for s in salaries:
    if not braxSal > s:
      cnf.limplies(sn.entry(braxSal, 'braxton'), -sc.entry(s, 'gingerbread'))
# - The person with the Congolese penpal earns less than the baker who made
# chocolate chip cookies.
for congSal in salaries:
  for s in salaries:
    if not congSal < s:
      cnf.limplies(sp.entry(congSal, 'congolese'), -sc.entry(s, 'chocolatechip'))
      cnf.limplies(sc.entry(s, 'chocolatechip'), -sp.entry(congSal, 'congolese'))

# - The baker who made oatmeal raisin cookies doesn't have the Congolese penpal.
for n in names:
  cnf.addClause([ -cn.entry(n, 'oatmealraisin'), pn.entry(n, 'congolese') ])

# - The employee with the $188,000 salary didn't bake black and white cookies.
for n in names:
  cnf.addClause([ -sn.entry(n, 188000), -cn.entry(n, 'blackwhite') ])

# - Either the baker who made oatmeal raisin cookies or the baker who made
# - peanut butter cookies is Valerie.
cnf.addClause([ cn.entry('oatmealraisin', 'valerie'),
                cn.entry('peanutbutter', 'valerie') ])

# - Of the baker who made peanut butter cookies and Braxton, one earns $86,000
# - per year and the other has the Uruguayan penpal.
for n in names:
  pn.entry(n, 'uruguayan')
sc.entry(86000, 'peanutbutter')
sn.entry(86000, 'braxton')
pn.entry('braxton', 'uruguayan')

# - The employee with the $86,000 salary has the Panamanian penpal.


result = cnf.solve()
print result



