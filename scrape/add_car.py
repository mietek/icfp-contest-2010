#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def add_page():
  return "http://icfpcontest.org/icfp10/instance"

def add_car(car, circuit):
  html = Popen(["curl",
                "-d", "problem=0&exampleSolution.contents=" + circuit,
                "-b", "cookie_jar",
                "-s",
                add_page()], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  pre = title_div.find("pre")
  ok = False
  if pre != None and pre.string != None:
    ok = True
    print pre.string
  errors = title_div.find("span", attrs={"id": "instance.errors"})
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
    add_car(sys.argv[1], circuit)
