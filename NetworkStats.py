def listToStr(l):
  s = ""
  for i in xrange(len(l) - 1):
    s += l[i] + ","
  s += l[len(l)-1]
  return s
class NetworkStats:
  # Grid Size
  gridRows = 0
  gridCols = 0
  
  # VARIABLE COUNTS (NOT GRID SIZE!!!!!!)
  numVars = 0
  numRows = 0
  numCols = 0
  numActions = 0
  numLandmarks = 0
  landmarkDict = {} #not list for speed
  landmarkList = [] # for order preservation (hacky afterthought)
  
  def __init__(self, numVars):
    self.numVars = numVars
  def printStats(self):
    print "numVars      ", self.numVars
    print "numRows      ", self.numRows
    print "numCols      ", self.numCols
    print "numActions   ", self.numActions
    print "numLandmarks ", self.numLandmarks
    print "landmarks    ", self.landmarkDict.keys()
  def parseVar(self,line):
    strs = line.split(" ")
    varName = strs[0]
    if "PositionRow" in varName:
      self.numRows = self.numRows + 1
      if self.gridRows == 0:
        rowlist = strs[1].split(",")
        self.gridRows = int(rowlist[-1])
    elif "PositionCol" in varName:
      self.numCols = self.numCols + 1
      if self.gridCols == 0:
        collist = strs[1].split(",")
        self.gridCols = int(collist[-1])
    elif "Action" in varName:
      self.numActions = self.numActions + 1
    else:
      self.numLandmarks = self.numLandmarks + 1
      # remove timestep from landmark name
      landmarkName = varName.split("_")
      landmarkName = landmarkName[:-1]
      landmarkName = '_'.join(landmarkName)
      if landmarkName not in self.landmarkDict:
        self.landmarkDict[landmarkName] = {}
        self.landmarkList.append(landmarkName)
  def generateCliques(self,filename):
    f = open(filename,'w+')
    numCliques = self.numActions * (len(self.landmarkList) + 1)
    print >> f, numCliques
    # generate cliques
    for t in xrange(self.numActions):
      # clique for each observation/position combination
      at_t = "_" + str(t)
      at_tp1 = "_" + str(t+1)
      obsCliques = [[obs+at_t,"PositionRow"+at_t,"PositionCol"+at_t] for obs in self.landmarkList]
      for clique in obsCliques:
        print >> f, listToStr(clique)
      # clique for position_t,action_t,position_t+1
      actionClique = ["PositionRow"+at_t,"PositionCol"+at_t,"Action"+at_t,"PositionRow"+at_tp1,"PositionCol"+at_tp1]
      # final timestep check:
      if t == self.numActions - 1:
        actionClique = actionClique[:-2]
      print >> f, listToStr(actionClique)
    # generate edges
    for t in xrange(self.numActions):
      # clique for each observation/position combination
      at_t = "_" + str(t)
      at_tp1 = "_" + str(t+1)
      at_tp2 = "_" + str(t+2)
      obsCliques = [[obs+at_t,"PositionRow"+at_t,"PositionCol"+at_t] for obs in self.landmarkList]
      actionClique = ["PositionRow"+at_t,"PositionCol"+at_t,"Action"+at_t,"PositionRow"+at_tp1,"PositionCol"+at_tp1]
      nextActionClique = ["PositionRow"+at_tp1,"PositionCol"+at_tp1,"Action"+at_tp1,"PositionRow"+at_tp2,"PositionCol"+at_tp2]
      if t == self.numActions -2:
        nextActionClique = nextActionClique[:-2]
      if t == self.numActions -1:
        actionClique = actionClique[:-2]
      for clique in obsCliques:
        s1 = listToStr(clique)
        s2 = listToStr(actionClique)
        print >> f, s1 + " -- " + s2
      if t != self.numActions - 1:
        print >> f, listToStr(actionClique) + " -- " + listToStr(nextActionClique)
    f.close()
   
      