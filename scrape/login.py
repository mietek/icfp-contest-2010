#!/usr/bin/env python

from subprocess import Popen, PIPE


def login_data():
  return "j_username=clfp&j_password=686762992444636725270476656697909779877319603925237455888864"

def login_page():
  return "http://icfpcontest.org/icfp10/static/j_spring_security_check"

def login():
  Popen(["curl",
         "-d", login_data(),
         "-c", "cookie_jar",
         "-s",
         login_page()], stdout=PIPE).communicate()[0]


login()
