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
    try:
      solved_file = open("solved_cars")
      solved_cars = set([line.strip() for line in solved_file.readlines()])
      solved_file.close()
    except IOError:
      solved_cars = set([])
    solved_file = open("solved_cars", "a")
    cars = list_all_cars.list_all_cars()
    cars.reverse()
    for car in cars:
      if car in solved_cars:
        print car + ": solved"
      else:
        print car + ":"
        if solve_car.solve_car(car, sys.argv[1]):
          print "(*** SOLVED ***)"
          solved_cars.update(car)
          solved_file.write(car + "\n")
          solved_file.flush()
      print
