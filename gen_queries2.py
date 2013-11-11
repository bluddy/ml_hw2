#!/usr/bin/python
import sys

if len(sys.argv) < 3:
    print "Requires evidence_file out_file"
    sys.exit()

evidencefile = sys.argv[1]
outfile = sys.argv[2]
f = open(evidencefile,'r')
out = open(outfile,'w+')
evidence = f.read()

print >> out, "ObserveLandmark1_N_8=True " + evidence
print >> out, "ObserveLandmark1_S_8=True " + evidence
print >> out, "ObserveLandmark1_E_8=True " + evidence
print >> out, "ObserveLandmark1_W_8=True " + evidence
f.close()
out.close()
