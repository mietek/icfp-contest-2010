#!/usr/bin/env python

from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE

import login


def cars_page():
  return "http://icfpcontest.org/icfp10/score/instanceTeamCount"

def list_cars():
  html = Popen(["curl",
                "-b", "cookie_jar",
                "-s",
                cars_page()], stdout=PIPE).communicate()[0]
  soup = BeautifulSoup(html)
  title_div = soup.find("div", attrs={"id": "_title_div"})
  table = title_div.find("table")
  tds = table.findAll("td", attrs={"style": "width: 20%;"})
  cars = [td.string for td in tds]
  cars.sort()
  print "\n".join(cars)


login.login()
list_cars()
