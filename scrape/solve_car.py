#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def solve_page(car):
  return "http://icfpcontest.org/icfp10/instance/" + car + "/solve"

def solve_car(car, circuit):
  html = Popen(["curl",
                "--data-urlencode", "contents@" + circuit,
                "-b", "cookie_jar",
                "-s",
                solve_page(car)], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  solved = False
  ok = False
  title_div = soup.find("div", attrs={"id": "_title_div"})
  if title_div:
    pre = title_div.find("pre")
    if pre and pre.string:
      ok = True
      print pre.string.strip()
    errors = title_div.find("span", attrs={"id": "solution.errors"})
    if errors and errors.string:
      if errors.string.startswith("You have already submitted this solution"):
        solved = True
      ok = True
      print errors.string.strip()
  else:
    main_div = soup.find("div", attrs={"id": "main"})
    if main_div:
      msg_div = main_div.find("div", attrs={"version": "2.0"})
      if msg_div and msg_div.contents and len(msg_div.contents) > 1:
        msg = str(msg_div.contents[0]).strip()
        if msg.startswith("You have submitted fuel for car"):
          solved = True
          ok = True
          print msg
      pre = main_div.find("pre")
      if pre and pre.string:
        ok = True
        print pre.string.strip()
  if not ok:
    print soup.prettify()
  return solved


if __name__ == '__main__':
  if len(sys.argv) != 3:
    print "Usage: solve_car.py <car> <circuit>"
    sys.exit()
  login.login()
  if solve_car(sys.argv[1], sys.argv[2]):
    print "*** SOLVED ***" + "\007"
  login.logout()
