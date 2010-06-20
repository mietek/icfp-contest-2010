#!/usr/bin/env python
from BeautifulSoup import BeautifulSoup
from subprocess import Popen, PIPE
import sys

cars_page = "http://nfa.imn.htwk-leipzig.de/recent_cars/#hotspot"

def get_cars(id):
    html = Popen(["curl",
                  "-F", "G0=" + str(id) , "-s",
                  cars_page], stdout=PIPE).communicate()[0]
    soup = BeautifulSoup(html)
    ret = []
    asff = "0"
    for elem in soup('pre'):
        touple = elem.string.split(',')
        asff = int(str(touple[0])[1:])
        temp = str(touple[2].replace('&quot;',''))
        ret += [(asff,temp[:len(temp) -1])]

    nextid = int(asff)
   # print nextid
    if nextid > id:
        ret += get_cars(nextid)
    return ret

def aux(dict,x):
    if dict.has_key(x) :
        return (dict[x] < 25)
    else:
        return True

def foo(dict):
    return sorted(filter(lambda x: aux(dict,x[0]),get_cars(1)) ,key=lambda x: len(x[1]))

if __name__ == '__main__':
  if len(sys.argv) != 1:
    print "Usage: get_cars.py"
    sys.exit()
  dict = {}
  for l in file('solved_no').readlines():
      tmp = l.split(' ')
      dict[int(tmp[0])] = int(tmp[1])

  for e in foo(dict):
      print e

