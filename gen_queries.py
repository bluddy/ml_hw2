#!/usr/bin/python
import sys

if len(sys.argv) < 6:
    print "Requires evidence_file out_file num_rows num_cols num_times"
    sys.exit()

evidencefile = sys.argv[1]
outfile = sys.argv[2]
numrows = int(sys.argv[3])
numcols = int(sys.argv[4])
numtimes = int(sys.argv[5])
f = open(evidencefile,'r')
out = open(outfile,'w+')
evidence = f.read()

suf = "_" + str(numtimes-1)
for r in xrange(1,numrows+1):
  for c in xrange(1,numcols+1):
    print >> out, "PositionRow"+suf+"=" + str(r) + ",PositionCol"+suf+"=" + str(c) + " " + evidence


f.close()
out.close()
