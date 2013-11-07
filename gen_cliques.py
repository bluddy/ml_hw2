#!/usr/bin/python

from NetworkStats import NetworkStats

if len(sys.argv) < 3:
    print "Requires network_file out_file"
    sys.exit()

networkFileName = sys.argv[1]
outfile = sys.argv[2]


# parse network information (using NetworkStats)
f = open(networkFileName,'r')
numVars = int(f.readline())
s = NetworkStats(numVars)
for i in xrange(s.numVars):
  line = f.readline()
  s.parseVar(line)
f.close()

s.generateCliques(outfile)
