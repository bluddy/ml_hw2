#!/usr/bin/python

import sys
from NetworkStats import NetworkStats
from MotionCounter import MotionCounter
from LandmarkCounter import LandmarkCounter

if len(sys.argv) < 4:
    print "Requires network_file training_file output_file"
    sys.exit()

# parse command line arguments
networkFileName = sys.argv[1]
trainingFileName = sys.argv[2]
outputFileName = sys.argv[3]

# open file for output 
out = open(outputFileName,'w')

# parse network information (using NetworkStats)
f = open(networkFileName,'r')
numVars = int(f.readline())
s = NetworkStats(numVars)
for i in xrange(s.numVars):
  line = f.readline()
  s.parseVar(line)
f.close()

# initialize data structures
l = LandmarkCounter(s.landmarkDict,s.landmarkList)
m = MotionCounter(s.gridRows,s.gridCols)

# training / populate data structures
training = open(trainingFileName,'r')

last_pos = None
last_trj = None
last_action = None
for line in training:
  line = line.replace('\n','')
  # parse line
  p = line.split(" ");
  curr_trj    = int(p[0])
  curr_t      = int(p[1])
  curr_row    = int(p[2].split("=")[1])
  curr_col    = int(p[3].split("=")[1])
  curr_action = p[4].split("=")[1]
  curr_obs    = p[5:]
  curr_obs =  ["".join((x.split("_")[:-1])) for x in curr_obs]   
  
  # motion statistics to be captured
  if(last_trj == curr_trj):
    m.incrementCounts(curr_row,curr_col,last_row,last_col,last_action)
  
  # landmark statistics to be captured
  l.incrementCounts(curr_row,curr_col,curr_obs)
  
  last_action = curr_action
  last_row    = curr_row
  last_col    = curr_col
  last_trj    = curr_trj
training.close()

# generate output

mn = "MoveNorth"
ms = "MoveSouth"
me = "MoveEast"
mw = "MoveWest"
pr = "PositionRow_"
pc = "PositionCol_"
a = "Action_"
numRows = s.gridRows
numCols = s.gridCols

numTimes = s.numRows

def getRowNum(i):
  temp = i % numRows
  if temp == 0:
    temp = numRows
  return temp

def getColNum(i):
  temp = i % numCols
  if temp == 0:
    temp = numCols
  return temp



for t in xrange(numTimes):
  # rows!
  for i in xrange(1,numRows+1):
	# North Success
	s1 = pr + str(t+1) + "=" + str(getRowNum(i+1))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + mn
	s4 = "%.13e" % (m.getProb('success',mn))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# North Fail
	s1 = pr + str(t+1) + "=" + str(getRowNum(i))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + mn
	s4 = "%.13e" % (m.getProb('fail',mn))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	 # South Success
	s1 = pr + str(t+1) + "=" + str(getRowNum(i-1))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + ms
	s4 = "%.13e" % (m.getProb('success',ms))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# South Fail
	s1 = pr + str(t+1) + "=" + str(getRowNum(i))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + ms
	s4 = "%.13e" % (m.getProb('fail',ms))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# East Trivial
	s1 = pr + str(t+1) + "=" + str(getRowNum(i))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + me
	s4 = "%.13e" % (1.0)
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# West Trivial
	s1 = pr + str(t+1) + "=" + str(getRowNum(i))
	s2 = pr + str(t)	 + "=" + str(getRowNum(i))
	s3 = a	+ str(t)	 + "=" + mw
	s4 = "%.13e" % (1.0)
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	

  # cols!
  for i in xrange(1,numCols+1):
  # North Trivial
	s1 = pc + str(t+1) + "=" + str(getColNum(i))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + mn
	s4 = "%.13e" % (1.0)
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# South Trivial
	s1 = pc + str(t+1) + "=" + str(getColNum(i))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + ms
	s4 = "%.13e" % (1.0)
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line

	# East Success
	s1 = pc + str(t+1) + "=" + str(getColNum(i+1))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + me
	s4 = "%.13e" % (m.getProb('success',me))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# East Fail
	s1 = pc + str(t+1) + "=" + str(getColNum(i))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + me
	s4 = "%.13e" % (m.getProb('fail',me))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# West Success
	s1 = pc + str(t+1) + "=" + str(getColNum(i-1))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + mw
	s4 = "%.13e" % (m.getProb('success',mw))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	# West Fail
	s1 = pc + str(t+1) + "=" + str(getColNum(i))
	s2 = pc + str(t)	 + "=" + str(getColNum(i))
	s3 = a	+ str(t)	 + "=" + mw
	s4 = "%.13e" % (m.getProb('fail',mw))
	line = " ".join([s1,','.join([s2,s3]),s4])
	print >> out, line
	
at_t =	str(t)
for row in xrange(1,numRows+1):
	for col in xrange(1,numCols+1):
		for landmark in l.getLandmarks():
			# yes
			s1 = landmark +"_" + at_t +"=Yes"
			s2 = pr+at_t+"="+str(getRowNum(row))+","+pc+at_t+"="+str(getColNum(col))
			prob = l.getProb(landmark,row,col,'yes')
			if prob != 0.0:
				print >> out, " ".join([s1,s2,"%.13e" % (prob)])
			# no
			s1 = landmark + "_" +at_t +"=No"
			s2 = pr+at_t+"="+str(getRowNum(row))+","+pc+at_t+"="+str(getColNum(col))
			prob = l.getProb(landmark,row,col,'no')
			if prob != 0.0:
				print >> out, " ".join([s1,s2,"%.13e" % (prob)])

 
out.close()



