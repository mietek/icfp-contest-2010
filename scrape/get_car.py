#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def solve_page(car):
  return "http://icfpcontest.org/icfp10/instance/" + car + "/solve/form"

def get_car(car):
  html = Popen(["curl",
                "-b", "cookie_jar",
                "-s",
                solve_page(car)], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  div = title_div.find("div", attrs={"id": "roo_solution_instance"})
  return div.contents[1].strip()


if __name__ == '__main__':
  if len(sys.argv) < 2:
    print "Usage: get_car.py <car>"
  else:
    login.login()
    print get_car(sys.argv[1])
