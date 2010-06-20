#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def solve_page(car):
  return "http://icfpcontest.org/icfp10/instance/%s/solve" % car

def solve_car(car, circuit):
  html = Popen(["curl",
                "-d", "contents=" + circuit,
                "-b", "cookie_jar",
                "-s",
                solve_page(car)], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  pre = title_div.find("pre")
  ok = False
  if pre != None and pre.string != None:
    ok = True
    print pre.string
  errors = title_div.find("span", attrs={"id": "solution.errors"})
  if errors != None and errors.string != None:
    ok = True
    print errors.string
  if not ok:
    print title_div.prettify()


if __name__ == '__main__':
  if len(sys.argv) < 3:
    print "Usage: solve.py <car> <circuit>"
  else:
    login.login()
    circuit = open(sys.argv[2]).read()
    solve_car(sys.argv[1], circuit)
