#!/usr/bin/env python

from subprocess import Popen, PIPE
import sys

import list_all_cars
import login
import solve_car


def solved_file_name():
  return "solved_cars"

def read_solved_file():
  try:
    solved_file = open(solved_file_name())
    solved_list = [line.strip() for line in solved_file.readlines()]
    solved_file.close()
  except IOError:
    solved_list = []
  return solved_list

def write_solved_file(solved_list):
  solved_file = open(solved_file_name(), "w")
  [solved_file.write(car + "\n") for car in solved_list]
  solved_file.close()


if __name__ == '__main__':
  if len(sys.argv) < 2:
    print "Usage: solve_all_cars.py <circuit> [--reverse]"
  else:
    solved_list = sorted(read_solved_file(), key=int)
    write_solved_file(solved_list)
    solved_set = set(solved_list)
    print str(len(solved_list)) + " cars solved"
    print
    login.login()
    cars = list_all_cars.list_all_cars()
    if len(sys.argv) > 2 and sys.argv[2] == "--reverse":
      cars.reverse()
    for car in cars:
      if car in solved_set:
        print car + ": solved"
      else:
        print car + ":"
        if solve_car.solve_car(car, sys.argv[1]):
          print "(*** SOLVED ***)"
          solved_set.add(car)
          solved_list = sorted(list(solved_set), key=int)
          write_solved_file(solved_list)
      print
