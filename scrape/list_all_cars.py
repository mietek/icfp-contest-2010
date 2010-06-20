#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

import login


def cars_page():
  return "http://icfpcontest.org/icfp10/score/instanceTeamCount"

def list_all_cars():
  html = Popen(["curl",
                "-b", "cookie_jar",
                "-s",
                cars_page()], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  table = title_div.find("table")
  tds = table.findAll("td", attrs={"style": "width: 20%;"})
  cars = [td.string.strip() for td in tds]
  unique_cars = list(set(cars))
  return sorted(unique_cars, key=int)


if __name__ == '__main__':
  if len(sys.argv) != 1:
    print "Usage: list_all_cars.py"
    sys.exit()
  login.login()
  print "\n".join(list_all_cars())
  login.logout()
