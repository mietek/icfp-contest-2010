#!/usr/bin/env python

import solve_car
import sys
import login
from subprocess import Popen, PIPE

'''
<jmi> wczyta liste par nr, liczba
<jmi> wygeneruje ./circuits liczba
<jmi> i uruchomi solve_car nr wynikGeneracji
'''

if __name__ == '__main__':
  if len(sys.argv) != 2:
    print "Usage: solver.py <file_with_pairs>"
    sys.exit()

  login.login()
  for line in file(sys.argv[1]).readlines():
      if line[:1] != 'n':

          tmp = line.split(',')
          circ = Popen(["../circuit/factcomp",tmp[1]], stdout=PIPE).communicate()[0]
          file('tmp.circ','w').write(circ)
#          login.login()
          if solve_car.solve_car(tmp[0], 'tmp.circ'):
              print "*** SOLVED ***" + "\007"
              print "\n"




