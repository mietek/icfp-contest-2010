#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def solve_page(car):
  return "http://icfpcontest.org/icfp10/instance/%s/solve/form" % car

def get_cars():
  html = Popen(["curl",
                "-b", "cookie_jar",
                "-s",
                cars_page()], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  tds = table.findAll("td", attrs={"style": "width: 20%;"})
  cars = [td.string for td in tds]
  cars.sort()
  print "\n".join(cars)

def get_car(car):
  html = Popen(["curl",
                "-b", "cookie_jar",
                "-s",
                solve_page(car)], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  div = title_div.find("div", attrs={"id": "roo_solution_instance"})
  print div.contents[1]


if len(sys.argv) < 2:
  print "Usage: solve.py <car>"
else:
  login.login()
  get_car(sys.argv[1])
