#!/usr/bin/env python

from subprocess import Popen, PIPE


def username():
  return "clfp"

def password():
  return "686762992444636725270476656697909779877319603925237455888864"

def login_page():
  return "http://icfpcontest.org/icfp10/static/j_spring_security_check"

def login():
  Popen(["curl",
         "--data-urlencode", "j_username=" + username(),
         "--data-urlencode", "j_password=" + password(),
         "-c", "cookie_jar",
         "-s",
         login_page()], stdout=PIPE).communicate()[0]


if __name__ == '__main__':
  if len(sys.argv) != 1:
    print "Usage: login.py"
    sys.exit()
  login()
