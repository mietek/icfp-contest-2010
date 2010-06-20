#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import list_all_cars
import login
import solve_car


if __name__ == '__main__':
  if len(sys.argv) < 2:
    print "Usage: solve.py <circuit>"
  else:
    login.login()
    cars = list_all_cars.list_all_cars()
    for car in cars:
      print car + ":"
      solve_car.solve_car(car, sys.argv[1])
      print
