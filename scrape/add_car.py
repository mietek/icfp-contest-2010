#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def add_page():
  return "http://icfpcontest.org/icfp10/instance"

def add_car(car, circuit):
  html = Popen(["curl",
                "--data-urlencode", "problem=" + car,
                "--data-urlencode", "exampleSolution.contents@" + circuit,
                "-b", "cookie_jar",
                "-s",
                add_page()], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  ok = False
  title_div = soup.find("div", attrs={"id": "_title_div"})
  if title_div:
    pre = title_div.find("pre")
    if pre and pre.string:
      ok = True
      print pre.string.strip()
    errors = title_div.find("span", attrs={"id": "instance.errors"})
    if errors and errors.string:
      ok = True
      print errors.string.strip()
  if not ok:
    print soup.prettify()


if __name__ == '__main__':
  if len(sys.argv) != 3:
    print "Usage: add_car.py <car> <circuit>"
    sys.exit()
  login.login()
  add_car(sys.argv[1], sys.argv[2])
